## For the paper.

#####
## CFB
##
## 10-fold CV to compare INCLUDING COMPLEMENTARY STATS vs NOT INCLUDING THEM
## For regular linear regression (not ordinal)
## [Calculates an object to be fed to "PAPER_RESubmission_11p6_LINEAR_VS_ORDINAL_BOTH_CFB_AND_COLLEGE_MAE_PLOTS.R"]
######

## Seed for reproducibility of CV
set.seed(1)

library(ordinalNet)

# Loading the enhanced ordinalNet code.
source("Submission_0_ordinalNet_package_source_code_enhanced.R")

library(tidyverse)
library(car)
library(fuzzyjoin)
library(sure)
library(ordinal)
library(ggplot2)
library(ggfortify)


# We will only focus on the complementary statistics that were consistently picked
# (yards gained and non-scoring takeaways)
all.variables <- FALSE

# We will be combining the yards for offense and special teams returns.
yds.combine <- TRUE


MAE <- TRUE

if (MAE){
  cost.fun <- function(y, pi) {
    mean(abs(y - pi))
  }
} else {
  cost.fun <- function(y, pi){
    sqrt(mean((y-pi)^2))
  }
}




## alpha trade-off value
alpha.val <- 0.99

## # of CV folds
n.folds <- 10

## We include the POS_TEAM/DEF_POS_TEAM/HOMEFIELD
incl.pos_team <- TRUE


projected.points <- NULL
projected.points.intercept <- NULL
ordnet.fits <- NULL


CV.results.with.complem <- NULL



for (year in 2014:2020){
  
  print("Year:")
  print(year)
  
  
  load(paste0("pbp_by_drive_",year,".Robj"))
  
  ## Non-scoring FORCED TOs, such as picks, fumbles, on-side kick recovery
  pbp_by_drive$takeaways.nonscor <- pbp_by_drive$drive_result_detailed %in% c("Fumble Recovery (Opponent)",
                                                                              "Interception Return",
                                                                              "Kickoff Team Fumble Recovery",
                                                                              "Punt Team Fumble Lost",
                                                                              "Blocked Punt Team Fumble Lost",
                                                                              "On-Side Kick Lost"
  )
  
  ## Any TOs, including FG misses/blocks, turnover on downs
  pbp_by_drive$turnovers.nonscor <- (pbp_by_drive$takeaways.nonscor | 
                                       pbp_by_drive$drive_result_detailed %in% c("Blocked Field Goal",
                                                                                 "Field Goal Missed",
                                                                                 "Missed Field Goal Return",
                                                                                 "Downs Turnover")
  )
  
  pbp_by_drive$defensive.score <- pbp_by_drive$drive_result_detailed %in% c("Blocked Field Goal Touchdown",
                                                                            "Fumble Return Touchdown",
                                                                            "Interception Return Touchdown",
                                                                            "Punt Team Fumble Recovery Touchdown",
                                                                            "Blocked Punt Team Fumble Recovery Touchdown",
                                                                            "Kickoff Team Fumble Recovery Touchdown"
  )
  
  
  ## Homefield indicator (no NEUTRAL field, evidently)
  pbp_by_drive$pos.homefield <- pbp_by_drive$home == pbp_by_drive$pos_team
  
  ## Punts or safeties (both result in turning the ball over via a 'punting motion', although the latter also has -2 pts)
  pbp_by_drive$punt.safety <- pbp_by_drive$drive_result_detailed %in% c("Punt", "Safety")
  
  ## Number of positive runs
  pbp_by_drive$n.positive.runs <- pbp_by_drive$n.rush - pbp_by_drive$n.stuffed.runs
  
  ## Special team return variables:
  pbp_by_drive$yds_ST_return <- ifelse(!is.na(pbp_by_drive$yds_punt_return),
                                       pbp_by_drive$yds_punt_return,
                                       ifelse(!is.na(pbp_by_drive$yds_kickoff_return),
                                              pbp_by_drive$yds_kickoff_return,
                                              0))
  
  pbp_by_drive$yds_ST_return_net <- ifelse(!is.na(pbp_by_drive$yds_punt_net),
                                           pbp_by_drive$yds_punt_net,
                                           ifelse(!is.na(pbp_by_drive$yds_kickoff_net),
                                                  pbp_by_drive$yds_kickoff_net,
                                                  0))
  
  ####
  ## Combining the yards for offense and special teams.
  ####
  if (yds.combine == TRUE){
    pbp_by_drive$off.yards_gained <- pbp_by_drive$off.yards_gained + pbp_by_drive$yds_ST_return
  }
  
  
  #####
  ## Creating the indicator variable for "complementary drives", as in - anything but
  #     1) Kickoffs at the start of halves, 
  #     2) Drives directly after a defensive touchdown.
  #####
  
  pbp_by_drive$complem.ind <- ifelse(lag(pbp_by_drive$game_id_half) == pbp_by_drive$game_id_half,
                                     ifelse(lag(pbp_by_drive$pos_team) != pbp_by_drive$pos_team,
                                            1,
                                            0),
                                     1)
  pbp_by_drive$complem.ind[1] <- 0   # Cleaning up the "NA" generated by the first lag
  
  
  # Initializing a lagged variable set
  pbp_by_drive_lag_vars <- data.frame(game_id_half = pbp_by_drive$game_id_half,
                                      pos_team = pbp_by_drive$pos_team,
                                      pbp_by_drive[, 8:ncol(pbp_by_drive)])
  
  # For numerical variables, creating their lagged values, 
  for (c.ind in 3:ncol(pbp_by_drive_lag_vars)){
    pbp_by_drive_lag_vars[, c.ind] <- lag(pbp_by_drive_lag_vars[, c.ind])
   # pbp_by_drive_lag_vars[1, c.ind] <- 0 # For the initial NA value
    pbp_by_drive_lag_vars[1, c.ind] <- pbp_by_drive_lag_vars[2, c.ind] # For the initial NA value
    
  }
  
  # Making the variable names for lagged values explicit (adding "lagged" to them)
  colnames(pbp_by_drive_lag_vars)[3:ncol(pbp_by_drive_lag_vars)] <- paste0("lagged_", colnames(pbp_by_drive_lag_vars)[3:ncol(pbp_by_drive_lag_vars)])
  
  # Merging the lagged values into original data set
  pbp_by_drive <- data.frame(pbp_by_drive, pbp_by_drive_lag_vars[,-1])
  
  ## All the complementary statistics under consideration
  relev.var.names <- c("n.completions", "n.incompletions", "n.stuffed.runs", "n.positive.runs", "n.negative.plays.with.penalties",
                       "off.yards_gained",
                       "score_pts_by_text",
                       "yds_ST_return_net", "yds_ST_return",
                       "n.sacks", "n.fumbles",
                       "first.downs.gained", "third.downs.converted",
                       "takeaways.nonscor", "turnovers.nonscor",
                       "punt.safety"
  )
  
  
  ## If we combine yards, then DROP the "yds_ST_return"
  if (yds.combine) relev.var.names <- relev.var.names[relev.var.names != "yds_ST_return"]
  
  
  
    pbp_by_drive$lagged_start_pos_post_turnover <- ifelse(pbp_by_drive$lagged_turnovers.nonscor == 1,
                                                          100-pbp_by_drive$lagged_stopped_position,
                                                          100-mean(pbp_by_drive$lagged_stopped_position[pbp_by_drive$lagged_turnovers.nonscor == 1], na.rm=T))




    #######
    ## Complementary statistics to be included in the model for testing:
    #######

    # relev.var.names.lagged <- paste0("lagged_", relev.var.names)

    if (all.variables){
      relev.var.names.lagged <- paste0("lagged_", relev.var.names)
    } else {
      # relev.var.names <- c("off.yards_gained",
      #                      "takeaways.nonscor")
      relev.var.names.lagged <- c("lagged_turnovers.nonscor",
                                  "lagged_start_pos_post_turnover")
    }
  
  pbp_by_drive$categ.score <- ifelse(pbp_by_drive$score_pts_by_text %in% c(6,7,8),
                                     "Offensive TD",
                                     ifelse(pbp_by_drive$score_pts_by_text == 3,
                                            "Field Goal",
                                            ifelse(pbp_by_drive$score_pts_by_text == -2,
                                                   "Safety",
                                                   ifelse(pbp_by_drive$score_pts_by_text %in% c(-6,-7,-8),
                                                          "Defensive TD",
                                                          "No Score"))))
  
  pbp_by_drive$score_pts <- ifelse(pbp_by_drive$score_pts_by_text %in% c(6,7,8),
                                   7,
                                   ifelse(pbp_by_drive$score_pts_by_text == 3,
                                          3,
                                          ifelse(pbp_by_drive$score_pts_by_text == -2,
                                                 -2,
                                                 ifelse(pbp_by_drive$score_pts_by_text == 2,
                                                        NA,
                                                        ifelse(pbp_by_drive$score_pts_by_text %in% c(-6,-7,-8),
                                                               -7,
                                                               0)))))
  
  
  pbp_by_drive$categ.score <- factor(pbp_by_drive$categ.score,
                                     ordered=T,
                                     levels = c("Defensive TD", "Safety", "No Score", "Field Goal", "Offensive TD"))
  
  
  
  ########
  ## Cleaning up the team names, including the "Non-Major" category for all the non-FBS teams
  ########
  
  team.names.year <- data.frame(Team=sort(unique(c(pbp_by_drive$pos_team, pbp_by_drive$def_pos_team))))
  #CFBSTATS_Team_Names <- data.frame(Team = read.csv("~/Documents/Work/New_College/Research/Play_by_Play_Complementary_Football_Project/CFBSTATS_vs_REFERENCE_Team_Names.csv")$CFBSTATS)
  CFBSTATS_Team_Names <- data.frame(Team = read.csv("CFBSTATS_vs_REFERENCE_Team_Names.csv")$CFBSTATS)
  CFBSTATS_Team_Names_Bar_Last <- data.frame(Team = gsub("\\s\\w+$", "", CFBSTATS_Team_Names$Team))
  
  
  
  # Matching up with standardized team names from "cfbstats.com" website.
  matches.df <- stringdist_join(CFBSTATS_Team_Names_Bar_Last, team.names.year, 
                                by='Team', #match based on team
                                mode='left', #use left join
                                method = "jw", #use jw distance metric
                                max_dist=1, 
                                distance_col='dist') %>%
    group_by(Team.x) %>%
    slice_min(order_by=dist, n=1)
  
  
  ## Fixing up all the identified mismatches, such as:
  ##       "Ole Miss" for "Mississippi"
  ##       "NC State" for "North Carolina State" 
  ##       "North Carolina A&T" matches to "North Carolina" along with "North Carolina Tar"...
  ##        For 2020, "Connecticut" didn't play, gotta drop its match ("Cincinnati")
  ##        etc..
  
  FBS.team.names <- matches.df$Team.y
  FBS.team.names[matches.df$Team.x == "Mississippi"] <- "Ole Miss"
  FBS.team.names[matches.df$Team.x == "North Carolina State"] <- "NC State"
  FBS.team.names[matches.df$Team.x == "Miami (Florida)"] <- "Miami"
  FBS.team.names <- sort(
    FBS.team.names[!(matches.df$Team.x == "Coastal Carolina" & matches.df$Team.y == "East Carolina") & 
                     FBS.team.names != "Charleston Southern" & 
                     matches.df$Team.x != "North Carolina A&T" & 
                     FBS.team.names != "North Carolina A&T" &
                     !(matches.df$Team.x == "UAB" & matches.df$Team.y == "UT San Antonio") & 
                     !(matches.df$Team.x == "Connecticut" & matches.df$Team.y == "Cincinnati") & 
                     !(matches.df$Team.x == "Idaho" & matches.df$Team.y == "Indiana") & 
                     !(matches.df$Team.x == "New Mexico State" & matches.df$Team.y == "New Mexico") & 
                     !(matches.df$Team.x == "Old Dominion" & matches.df$Team.y == "Wyoming")])
  
  # Liberty only became FBS from 2018 onwards:
  if (year <= 2017) FBS.team.names <- FBS.team.names[FBS.team.names != "Liberty"]
  # Idaho stopped being FBS from 2018 onwards:
  if (year >= 2018) FBS.team.names <- FBS.team.names[FBS.team.names != "Idaho"]
  
  
  ## Creating the non-major category
  nonmajor.teams <- team.names.year$Team[!team.names.year$Team %in% FBS.team.names]
  pbp_by_drive$pos_team <- ifelse(pbp_by_drive$pos_team %in% FBS.team.names,
                                  pbp_by_drive$pos_team,
                                  "Non-Major")
  pbp_by_drive$def_pos_team <- ifelse(pbp_by_drive$def_pos_team %in% FBS.team.names,
                                      pbp_by_drive$def_pos_team,
                                      "Non-Major")
  
  
  
  
  
  ###########
  ###########
  ### Calculating PROJECTED POINTS for EACH TEAM
  ###########
  ###########
  
  pbp_by_drive$game_id <- as.factor(str_remove(pbp_by_drive$game_id_half, "-1|-2"))
  
  ## Time left in half
  pbp_by_drive$drive_time_left_in_half <- pbp_by_drive$drive_time_minutes_start + 
    pbp_by_drive$drive_time_seconds_start/60 +
    ifelse(pbp_by_drive$drive_qtr_start %in% c(1,3),
           15,
           0)
  
  pbp_by_drive.noNA <- na.omit(pbp_by_drive[, c(# "score_pts_by_text", 
                                                "score_pts",
                                                "pos_team", "def_pos_team", relev.var.names.lagged, "game_id", "game_id_half", "pos.homefield", "complem.ind",
                                                "score_diff_start", "drive_time_left_in_half", "drive_half_start")] %>%
                                 mutate(pos_team = factor(pos_team),
                                        def_pos_team = factor(def_pos_team),
                                        game_id = factor(game_id),
                                        game_id_half = factor(game_id_half)))
  
  
  
  ## Setting up the contrasts so that the intercept represented a "league-average opponent"
  ## (sum_i alpha_i = sum_j beta_j = 0)
  contrasts(pbp_by_drive.noNA$pos_team) <- contr.sum(nlevels(pbp_by_drive.noNA$pos_team))
  contrasts(pbp_by_drive.noNA$def_pos_team) <- contr.sum(nlevels(pbp_by_drive.noNA$def_pos_team))
  colnames(contrasts(pbp_by_drive.noNA$pos_team)) <- rownames(contrasts(pbp_by_drive.noNA$pos_team))[-nrow(contrasts(pbp_by_drive.noNA$pos_team))]
  colnames(contrasts(pbp_by_drive.noNA$def_pos_team)) <- rownames(contrasts(pbp_by_drive.noNA$def_pos_team))[-nrow(contrasts(pbp_by_drive.noNA$def_pos_team))]
  
  
  
    # if (stand == FALSE) {
    #   pbp_by_drive.noNA <- pbp_by_drive.noNA %>%
    #     mutate_if(is.numeric, scale)
    # }

    pbp_by_drive.noNA$complem.ind <- as.numeric(pbp_by_drive.noNA$complem.ind)

    #pbp_by_drive.noNA$lagged_takeaways.nonscor <- as.numeric(pbp_by_drive.noNA$lagged_takeaways.nonscor)
    pbp_by_drive.noNA$lagged_turnovers.nonscor <- as.numeric(pbp_by_drive.noNA$lagged_turnovers.nonscor)

  
  
  ## First: basic lm fit, just to get the design matrix X & response y
  # lm.obj <- lm(formula(paste0("as.numeric(categ.score) ~", paste0(c("pos_team", "def_pos_team", "pos.homefield", "complem.ind", paste0(relev.var.names.lagged, ":complem.ind", sep="")),
  #                                                                 collapse = " + "), collapse =" ")),
  #              data=pbp_by_drive.noNA[, c(if(incl.pos_team) c("pos_team", "def_pos_team", "pos.homefield", "complem.ind") else NULL,
  #                                         relev.var.names.lagged,
  #                                         "categ.score")])
  
  lm.obj <- glm(formula(paste0(# "score_pts_by_text ~", 
                              "score_pts ~",
                               paste0(c("pos_team", "def_pos_team", "pos.homefield", "complem.ind", 
                                                               # paste0(relev.var.names.lagged, ":complem.ind", sep="")
                                                               paste0(c(relev.var.names.lagged[1], paste0(relev.var.names.lagged[1],":", relev.var.names.lagged[2])), ":complem.ind", sep="")
                                                                    , "score_diff_start", "drive_time_left_in_half", "drive_half_start"
                                                                    , "score_diff_start:drive_time_left_in_half", "score_diff_start:drive_half_start", "drive_time_left_in_half:drive_half_start"
  ),
  collapse = " + "), collapse =" ")),
  data=pbp_by_drive.noNA[, c(if(incl.pos_team) c("pos_team", "def_pos_team", "pos.homefield", "complem.ind", 
                                                 "score_diff_start", "drive_time_left_in_half", "drive_half_start") else NULL,
                             relev.var.names.lagged,
                             # "score_pts_by_text"
                             "score_pts"
                             )])
  
  cv.obj <- cv.glm(data = pbp_by_drive.noNA[, c(if(incl.pos_team) c("pos_team", "def_pos_team", "pos.homefield", "complem.ind", 
                                                                    "score_diff_start", "drive_time_left_in_half", "drive_half_start") else NULL,
                                                relev.var.names.lagged,
                                                # "score_pts_by_text"
                                                "score_pts"
                                                )],
                   lm.obj,
                   K=10,
                   cost=cost.fun)
  
  CV.results.with.complem <- rbind(CV.results.with.complem,
                                   data.frame(Year = year,
                                              RMSE = cv.obj$delta[1],
                                              SE = sqrt(cv.obj$delta[2]/nrow(pbp_by_drive.noNA))))
  
}



print(CV.results.with.complem)
# print(CV.results.no.complem)
# print(CV.results.nothing)
# print(CV.results.only.complem)


save(CV.results.with.complem, file = "LINEAR_CFB_W_START_POS_INTERACTION_CV.results.with.complem.Robj")
# save(CV.results.no.complem, file = "LINEAR_CFB_W_START_POS_INTERACTION_CV.results.no.complem.Robj")
# save(CV.results.nothing, file = "LINEAR_CFB_W_START_POS_INTERACTION_CV.results.nothing.Robj")
# save(CV.results.only.complem, file = "LINEAR_CFB_W_START_POS_INTERACTION_CV.results.only.complem.Robj")



# > print(CV.results.with.complem)
# Year     RMSE
# 1 2014 3.009150
# 2 2015 3.006887
# 3 2016 3.049069
# 4 2017 3.009614
# 5 2018 3.021870
# 6 2019 3.007626
# 7 2020 3.057638

