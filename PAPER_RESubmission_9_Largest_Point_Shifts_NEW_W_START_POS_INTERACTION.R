### To be included in the paper.

#########
## CFB, Season-Long adjustments (Table 3 in main text; Table 4 in supplementary materials)
##
## Generating the table from the paper of ranking/value shifts for 
## offensive and defensive points per drive modifications based on the
## complementary football adjustments. 
#########


library(ordinalNet)
# Loading the enhanced ordinalNet code.
source("Submission_0_ordinalNet_package_source_code_enhanced.R")

library(tidyverse)
library(car)
library(fuzzyjoin)
library(ordinal)
library(ggplot2)
library(ggfortify)


# We will be combining the yards for offense and special teams returns.
yds.combine <- TRUE


## Loading all the relevant objects
# load("RESUBMISSION_NFL_LEAVE_CONTEXT_AS_IS_ORDNET_projected.points.intercept.RData")
# load("ORDNET_ordnet.fits.RData")

## Whether it was balanced bootstrap or not
balanced <- FALSE

## Picking the season
year <- 2016

## Picking the side ("Offense" or "Defense")
side <- "Offense"



# #####
# ## Obtaining the values with and without adjustment for complementary football features
# #####
# 
# shift.df <- NULL
# 
# for (year in 2009:2017){
#   adj_opp.rank.table <- projected.points.intercept[[as.character(year)]][, c("Team", paste0(side, ".Avg.Points.With.Complem.But.As.Is"))]
#   adj_opp_and_ballside.rank.table <- projected.points.intercept[[as.character(year)]][, c("Team", paste0(side, c(".Avg.Points.Avg.Takeaways")))]
#   colnames(adj_opp.rank.table)[1:2] <- colnames(adj_opp_and_ballside.rank.table)[1:2] <- c("Team", "Value")
#   
#   adj_opp.rank.table$Rank <- if (side == "Defense") rank(adj_opp.rank.table$Value, tie="first") else rank(-adj_opp.rank.table$Value, tie="first")
#   adj_opp_and_ballside.rank.table$Rank <- if (side == "Defense") rank(adj_opp_and_ballside.rank.table$Value, tie="first") else rank(-adj_opp_and_ballside.rank.table$Value, tie="first")
#   
#   
#   shift.df <- rbind(shift.df,
#                     data.frame(Year = year,
#                                Team = adj_opp.rank.table[,1],
#                                adj_opp.rank.table[,-1],
#                                adj_opp_and_ballside.rank.table[,-1]) %>%
#                       mutate(Value.Diff = Value.1 - Value,
#                              Rank.Diff = Rank.1 - Rank))
# }
# 
# 
# dim(shift.df)
# 
# ## Largest positive shifts
# 
# our.table.top <- 
#   shift.df %>%
#   arrange(desc(Value.Diff)) %>%
#   head(5)
# 
# ## Largest negative shifts
# our.table.bottom <-
#   shift.df %>%
#   arrange(Value.Diff) %>%
#   head(5)





#######
## BOOTSTRAP STUFF
#######

## Getting the 95% bootstrap CI for each estimate (no complem and with complem)

# load(paste0("NEW_W_START_POS_INTERACTION_my.df.balanced.", side, ".Robj"))
load(paste0("NEW_W_START_POS_INTERACTION_my.df.not.balanced.Robj"))


######
## ADDING RANKS for CONFIDENCE INTERVALS
######

# my.df %>%
#   group_by(Year) %>%
#   summarise(RANK.Offense.Avg.Points.With.Complem.But.As.Is = rank(-Offense.Avg.Points.With.Complem.But.As.Is),
#          RANK.Offense.Avg.Points.Avg.Takeaways = rank(-Offense.Avg.Points.Avg.Takeaways))


### Whether to cut out teams with really low sample size (need AT LEAST 5 GAMES WORTH OF DATA)
if (side == "Offense"){
my.df <- my.df %>%
  filter(Offense.Halves.Included >= 10)
} else {
  my.df <- my.df %>%
    filter(Defense.Halves.Included >= 10)
}

print(my.df %>%
  group_by(Year) %>%
  summarise(length(unique(Team))))


if (side == "Offense"){
  
  # View(my.df %>%
  #   group_by(Team, Year) %>%
  #   summarise(Boot.Mean.No.Complem = mean(Offense.Avg.Points.With.Complem.But.As.Is),
  #             Boot.Mean.With.Complem = mean(Offense.Avg.Points.Avg.Takeaways),
  #             CI.Left.No.Complem = quantile(Offense.Avg.Points.With.Complem.But.As.Is, 0.025),
  #             CI.Right.No.Complem = quantile(Offense.Avg.Points.With.Complem.But.As.Is, 0.975),
  #             CI.Left.With.Complem = quantile(Offense.Avg.Points.Avg.Takeaways, 0.025),
  #             CI.Right.With.Complem = quantile(Offense.Avg.Points.Avg.Takeaways, 0.975)))
  
  boot.df <- my.df %>%
    group_by(Team, Year) %>%
    summarise(Boot.Mean.No.Complem = mean(Offense.Avg.Points.With.Complem.But.As.Is),
              Boot.Mean.With.Complem = mean(Offense.Avg.Points.Avg.Takeaways),
              CI.Left.Diff = quantile(Offense.Avg.Points.Avg.Takeaways - Offense.Avg.Points.With.Complem.But.As.Is, 0.025),
              CI.Right.Diff = quantile(Offense.Avg.Points.Avg.Takeaways - Offense.Avg.Points.With.Complem.But.As.Is, 0.975),
              Boot.Mean.Diff = Boot.Mean.With.Complem - Boot.Mean.No.Complem) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(Team=Team,
              Value = Boot.Mean.No.Complem,
              CI.Left.Diff = CI.Left.Diff,
              CI.Right.Diff = CI.Right.Diff,
              Rank = if (side == "Defense") rank(Boot.Mean.No.Complem, tie="first") else rank(-Boot.Mean.No.Complem, tie="first"),
              Value.1 = Boot.Mean.With.Complem,
              Rank.1 = if (side == "Defense") rank(Boot.Mean.With.Complem, tie="first") else rank(-Boot.Mean.With.Complem, tie="first"),
              Value.Diff = Value.1 - Value,
              Rank.Diff = Rank.1 - Rank)
  
} else {
  
  boot.df <- my.df %>%
    group_by(Team, Year) %>%
    summarise(Boot.Mean.No.Complem = mean(Defense.Avg.Points.With.Complem.But.As.Is),
              Boot.Mean.With.Complem = mean(Defense.Avg.Points.Avg.Takeaways),
              CI.Left.Diff = quantile(Defense.Avg.Points.Avg.Takeaways - Defense.Avg.Points.With.Complem.But.As.Is, 0.025),
              CI.Right.Diff = quantile(Defense.Avg.Points.Avg.Takeaways - Defense.Avg.Points.With.Complem.But.As.Is, 0.975),
              Boot.Mean.Diff = Boot.Mean.With.Complem - Boot.Mean.No.Complem) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(Team=Team,
              Value = Boot.Mean.No.Complem,
              CI.Left.Diff = CI.Left.Diff,
              CI.Right.Diff = CI.Right.Diff,
              Rank = if (side == "Defense") rank(Boot.Mean.No.Complem, tie="first") else rank(-Boot.Mean.No.Complem, tie="first"),
              Value.1 = Boot.Mean.With.Complem,
              Rank.1 = if (side == "Defense") rank(Boot.Mean.With.Complem, tie="first") else rank(-Boot.Mean.With.Complem, tie="first"),
              Value.Diff = Value.1 - Value,
              Rank.Diff = Rank.1 - Rank)
  
}

our.table.top.boot <- 
  boot.df %>%
  arrange(desc(Value.Diff)) %>%
  # arrange(desc(Rank.Diff)) %>%
  head(5)

our.table.bottom.boot <- 
  boot.df %>%
  arrange(Value.Diff) %>%
  # arrange(Rank.Diff) %>%
  head(5)


if (side == "Offense"){
  our.table.boot <- rbind(our.table.top.boot,
                          our.table.bottom.boot)  
} else {
  our.table.boot <- rbind(our.table.bottom.boot,
                          our.table.top.boot)  
}










######
## Loading all the team statistics from that season season
## to calculate the per-drive complementary statistics values
## (which will be used to demonstrate the underlying mechanism of ranking shifts)
######

pbp_by_drive_full <- NULL


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
                                            TRUE,
                                            FALSE),
                                     TRUE)
  pbp_by_drive$complem.ind[1] <- FALSE   # Cleaning up the "NA" generated by the first lag
  
  
  # Initializing a lagged variable set
  pbp_by_drive_lag_vars <- data.frame(game_id_half = pbp_by_drive$game_id_half,
                                      pos_team = pbp_by_drive$pos_team,
                                      pbp_by_drive[, 8:ncol(pbp_by_drive)])
  
  # For numerical variables, creating their lagged values, 
  for (c.ind in 3:ncol(pbp_by_drive_lag_vars)){
    pbp_by_drive_lag_vars[, c.ind] <- lag(pbp_by_drive_lag_vars[, c.ind])
    pbp_by_drive_lag_vars[1, c.ind] <- pbp_by_drive_lag_vars[2, c.ind] # For the initial NA value, which will get wiped out by complem.ind=0 anyway
  }
  
  # Making the variable names for lagged values explicit (adding "lagged" to them)
  colnames(pbp_by_drive_lag_vars)[3:ncol(pbp_by_drive_lag_vars)] <- paste0("lagged_", colnames(pbp_by_drive_lag_vars)[3:ncol(pbp_by_drive_lag_vars)])
  
  # Merging the lagged values into original data set
  pbp_by_drive <- data.frame(pbp_by_drive, pbp_by_drive_lag_vars[,-1])
  
  
  
  ## If we combine yards, then DROP the "yds_ST_return"
  #  if (yds.combine) relev.var.names <- relev.var.names[relev.var.names != "yds_ST_return"]
  
  
  # pbp_by_drive$lagged_start_pos_post_takeaway <- ifelse(pbp_by_drive$lagged_takeaways.nonscor == 1, 
  #                                                       100-pbp_by_drive$lagged_stopped_position,
  #                                                       100-mean(pbp_by_drive$lagged_stopped_position[pbp_by_drive$lagged_takeaways.nonscor == 1], na.rm=T))
  # 
  pbp_by_drive$lagged_start_pos_post_turnover <- ifelse(pbp_by_drive$lagged_turnovers.nonscor == 1, 
                                                        100-pbp_by_drive$lagged_stopped_position,
                                                        100-mean(pbp_by_drive$lagged_stopped_position[pbp_by_drive$lagged_turnovers.nonscor == 1], na.rm=T))
  
  
  
  
  #######
  ## Complementary statistics to be included in the model for testing:
  #######
  
  # relev.var.names.lagged <- paste0("lagged_", relev.var.names)
  
    relev.var.names.lagged <- c("lagged_turnovers.nonscor",
                                "lagged_start_pos_post_turnover")
  
  ## Defining the categorical ordinal scoring outcome variable
  
  pbp_by_drive$categ.score <- ifelse(pbp_by_drive$score_pts_by_text %in% c(6,7,8),
                                     "Offensive TD",
                                     ifelse(pbp_by_drive$score_pts_by_text == 3,
                                            "Field Goal",
                                            ifelse(pbp_by_drive$score_pts_by_text == -2,
                                                   "Safety",
                                                   ifelse(pbp_by_drive$score_pts_by_text == 2,
                                                          NA,
                                                          ifelse(pbp_by_drive$score_pts_by_text %in% c(-6,-7,-8),
                                                                 "Defensive TD",
                                                                 "No Score")))))
  
  
  pbp_by_drive$categ.score <- factor(pbp_by_drive$categ.score,
                                     ordered=T,
                                     levels = c("Defensive TD", "Safety", "No Score", "Field Goal", "Offensive TD"))
  
  
  
  
  ########
  ## Cleaning up the team names, including the "Non-Major" category for all the non-FBS teams
  ########
  
  team.names.year <- data.frame(Team=sort(unique(c(pbp_by_drive$pos_team, pbp_by_drive$def_pos_team))))
  # CFBSTATS_Team_Names <- data.frame(Team = read.csv("~/Documents/Work/New_College/Research/Play_by_Play_Complementary_Football_Project/CFBSTATS_vs_REFERENCE_Team_Names.csv")$CFBSTATS)
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
  ### Preparing the data frame and design matrices for cross-validation
  ###########
  
  pbp_by_drive$game_id <- as.factor(str_remove(pbp_by_drive$game_id_half, "-1|-2"))
  
  ## Time left in half
  pbp_by_drive$drive_time_left_in_half <- pbp_by_drive$drive_time_minutes_start + 
    pbp_by_drive$drive_time_seconds_start/60 +
    ifelse(pbp_by_drive$drive_qtr_start %in% c(1,3),
           15,
           0)
  
  
  pbp_by_drive_full <- rbind(pbp_by_drive_full,
                             data.frame(Year=year,
                                        pbp_by_drive))
  
}





## Calculating the per-drive average rankings on complementary football stats

if (side == "Offense"){
  our.avg.mat <- pbp_by_drive_full %>%
    group_by(Year, def_pos_team) %>%
    summarize(n.drives.played = nrow(unique(data.frame(game_id, game_id_drive))),
              turnovers.nonscor.per.drive = mean(turnovers.nonscor, na.rm=T),
    ) %>%
    rename(Team = def_pos_team) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(Team=Team,
              n.drives.played = n.drives.played,
              turnovers.nonscor.per.drive = turnovers.nonscor.per.drive,
              rank.turnovers.nonscor.per.drive = rank(-turnovers.nonscor.per.drive, tie="first")
    )
  
  our.avg.mat <- our.avg.mat %>% left_join(pbp_by_drive_full %>%
                                             group_by(Year, pos_team) %>%
                                             summarize(n.drives.played = nrow(unique(data.frame(game_id, game_id_drive))),
                                                       # start_pos_post_turnover.per.drive = if (turnovers.nonscor.per.drive > 0) mean(start_pos_post_turnover, na.rm=T)
                                                       start_pos_post_turnover.per.drive = mean(lagged_start_pos_post_turnover[lagged_turnovers.nonscor == TRUE], na.rm=T)
                                                       # start_pos_post_turnover.per.drive = median(start_pos_post_turnover[complem.ind == TRUE & turnovers.nonscor == TRUE], na.rm=T)
                                             ) %>% rename(Team = pos_team) %>%
                                             ungroup() %>%
                                             group_by(Year) %>%
                                             summarise(Team=Team,
                                                       # rank.turnovers.nonscor.per.drive = rank(-turnovers.nonscor.per.drive, tie="first")
                                                       start_pos_post_turnover.per.drive = start_pos_post_turnover.per.drive,
                                                       rank.start_pos_post_turnover.per.drive = rank(-start_pos_post_turnover.per.drive, tie="first")
                                             ))
  
  
  
} else {
  
  our.avg.mat <- pbp_by_drive_full %>%
    group_by(Year, pos_team) %>%
    summarize(n.drives.played = nrow(unique(data.frame(game_id, game_id_drive))),
              turnovers.nonscor.per.drive = mean(turnovers.nonscor, na.rm=T),
    ) %>%
    rename(Team = pos_team) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(Team=Team,
              n.drives.played = n.drives.played,
              turnovers.nonscor.per.drive = turnovers.nonscor.per.drive,
              rank.turnovers.nonscor.per.drive = rank(turnovers.nonscor.per.drive, tie="first")
    )
  
  our.avg.mat <- our.avg.mat %>% left_join(pbp_by_drive_full %>%
                                             group_by(Year, def_pos_team) %>%
                                             summarize(n.drives.played = nrow(unique(data.frame(game_id, game_id_drive))),
                                                       # start_pos_post_turnover.per.drive = if (turnovers.nonscor.per.drive > 0) mean(start_pos_post_turnover, na.rm=T)
                                                       start_pos_post_turnover.per.drive = mean(lagged_start_pos_post_turnover[lagged_turnovers.nonscor == TRUE], na.rm=T)
                                                       # start_pos_post_turnover.per.drive = median(start_pos_post_turnover[complem.ind == TRUE & turnovers.nonscor == TRUE], na.rm=T)
                                             ) %>% rename(Team = def_pos_team) %>%
                                             ungroup() %>%
                                             group_by(Year) %>%
                                             summarise(Team=Team,
                                                       # rank.turnovers.nonscor.per.drive = rank(-turnovers.nonscor.per.drive, tie="first")
                                                       start_pos_post_turnover.per.drive = start_pos_post_turnover.per.drive,
                                                       rank.start_pos_post_turnover.per.drive = rank(start_pos_post_turnover.per.drive, tie="first")
                                             ))
}

our.pred.val <- colnames(our.avg.mat)[str_detect(colnames(our.avg.mat), "per.drive") & !str_detect(colnames(our.avg.mat), "rank")]
our.pred.rank <- colnames(our.avg.mat)[str_detect(colnames(our.avg.mat), "per.drive") & str_detect(colnames(our.avg.mat), "rank")]




## Adding the values and rankings of teams based on the complementary football statistics

if (side == "Offense"){
  
  our.rank.mat <- data.frame(Team=our.avg.mat$Team,
                             Year=our.avg.mat$Year,
                             n.drives.played=our.avg.mat$n.drives.played,
                             TO.Value=format(round(unlist(our.avg.mat[, our.pred.val[1]]),2), nsmall=2),
                             TO.Rank=our.avg.mat[, our.pred.rank[1]][[1]],
                             TO.Rank.Full=rank(-unlist(our.avg.mat[, our.pred.val[1]]), tie="first"),
                             StartPos.Value=format(round(unlist(our.avg.mat[, our.pred.val[2]]))),
                             StartPos.Rank=our.avg.mat[, our.pred.rank[2]][[1]],
                             StartPos.Rank.Full=rank(-unlist(our.avg.mat[, our.pred.val[2]]), tie="first"))
  rownames(our.rank.mat) <- NULL
  
  final.table <- our.table.boot %>% mutate(PPG.Value.Shift = ifelse(Value.Diff > 0, 
                                                                    paste0(format(round(Value.1,2), nsmall=2), " ($\\textcolor{darkgreen}{\\Uparrow}$ ", format(round(abs(Value.Diff),2), nsmall=2), ", [", format(round(CI.Left.Diff,2), nsmall=2),", ", format(round(CI.Right.Diff,2), nsmall=2) ,"])"),
                                                                    ifelse(Value.Diff <0,
                                                                           paste0(format(round(Value.1,2), nsmall=2), " ($\\textcolor{red}{\\Downarrow}$ ", format(round(abs(Value.Diff),2), nsmall=2), ", [", format(round(CI.Left.Diff,2), nsmall=2),", ", format(round(CI.Right.Diff,2), nsmall=2) ,"])"),
                                                                           paste0(format(round(Value.1,2), nsmall=2), " ($\\textcolor{blue}{=}", format(round(abs(Value.Diff),2), nsmall=2), "$)"))),
                                           PPG.Rank.Shift = ifelse(Rank.Diff < 0, 
                                                                   paste0(Rank.1, " ($\\textcolor{darkgreen}{\\Uparrow}$ ", abs(Rank.Diff), ")"),
                                                                   ifelse(Rank.Diff >0,
                                                                          paste0(Rank.1, " ($\\textcolor{red}{\\Downarrow}$ ", abs(Rank.Diff), ")"),
                                                                          paste0(Rank.1, " ($\\textcolor{blue}{=}", abs(Rank.Diff), "$)")))) %>%
    dplyr::select(-Value, -CI.Right.Diff, -CI.Left.Diff, -Rank,  -Rank.Diff, -Value.Diff) %>%
    left_join(our.rank.mat)
  
  final.table <- final.table %>% mutate(TO.Rank.Value = paste0(TO.Rank, " (", TO.Value, ")"),
                                        StartPos.Rank.Value = paste0(StartPos.Rank, " (", StartPos.Value, ")"))
  
  
} else {
  
  our.rank.mat <- data.frame(Team=our.avg.mat$Team,
                             Year=our.avg.mat$Year,
                             n.drives.played=our.avg.mat$n.drives.played,
                             TO.Value=format(round(unlist(our.avg.mat[, our.pred.val[1]]),2), nsmall=2),
                             TO.Rank=our.avg.mat[, our.pred.rank[1]][[1]],
                             TO.Rank.Full=rank(unlist(our.avg.mat[, our.pred.val[1]]), tie="first"),
                             StartPos.Value=format(round(unlist(our.avg.mat[, our.pred.val[2]]))),
                             StartPos.Rank=our.avg.mat[, our.pred.rank[2]][[1]],
                             StartPos.Rank.Full=rank(unlist(our.avg.mat[, our.pred.val[2]]), tie="first"))
  rownames(our.rank.mat) <- NULL
  
  final.table <- our.table.boot %>% mutate(PPG.Value.Shift = ifelse(Value.Diff < 0, 
                                                                    paste0(format(round(Value.1,2), nsmall=2), " ($\\textcolor{darkgreen}{\\Downarrow}$ ", format(round(abs(Value.Diff),2), nsmall=2), ", [", format(round(CI.Left.Diff,2), nsmall=2),", ", format(round(CI.Right.Diff,2), nsmall=2) ,"])"),
                                                                    ifelse(Value.Diff >0,
                                                                           paste0(format(round(Value.1,2), nsmall=2), " ($\\textcolor{red}{\\Uparrow}$ ", format(round(abs(Value.Diff),2), nsmall=2), ", [", format(round(CI.Left.Diff,2), nsmall=2),", ", format(round(CI.Right.Diff,2), nsmall=2) ,"])"),
                                                                           paste0(format(round(Value.1,2), nsmall=2), " ($\\textcolor{blue}{=}", format(round(abs(Value.Diff),2), nsmall=2), "$)"))),
                                           PPG.Rank.Shift = ifelse(Rank.Diff < 0, 
                                                                   paste0(Rank.1, " ($\\textcolor{darkgreen}{\\Uparrow}$ ", abs(Rank.Diff), ")"),
                                                                   ifelse(Rank.Diff > 0,
                                                                          paste0(Rank.1, " ($\\textcolor{red}{\\Downarrow}$ ", abs(Rank.Diff), ")"),
                                                                          paste0(Rank.1, " ($\\textcolor{blue}{=}", abs(Rank.Diff), "$)")))) %>%
    dplyr::select(-Value, -CI.Right.Diff, -CI.Left.Diff, -Rank,  -Rank.Diff, -Value.Diff) %>%
    left_join(our.rank.mat)
  
  final.table <- final.table %>% mutate(TO.Rank.Value = paste0(TO.Rank, " (", TO.Value, ")"),
                                        StartPos.Rank.Value = paste0(StartPos.Rank, " (", StartPos.Value, ")"))
  
  
}


final.table$Value.1 <- format(round(final.table$Value.1, 2), nsmall=2)



for (j in 1){
  
  if (side == "Offense"){
    cat(paste0(c("Team \\", "Year \\", "Drives \\",  "Points Scored (Per Drive)", "Defense Statistics (Per Drive)"), collapse = " & "), "\\\\ \\hline")
  }
  
  
  if (side == "Defense"){
    cat(paste0(c("Team \\ ", "Year \\", "Drives\\", "Points Allowed (Per Drive)", "Offense Statistics (Per Drive)"), collapse = " & "), "\\\\ \\hline")
  }
  
  
  
  
  
  # cat(" \n \\begin{tabular}{r} \\\\ \\\\", paste0(final.table$Team[1:5], " & ", final.table$Year[1:5], " & ", final.table$game_id[1:5], collapse = " \\\\ "),
  #     " \\\\ .... \\\\",
  #     "\\end{tabular} &")
  
  cat(" \n \\begin{tabular}{r} \\\\ \\\\", paste0(final.table$Team[1:5], collapse = " \\\\ "),
      " \\\\ .... \\\\",
      paste0(final.table$Team[6:10], collapse = " \\\\ "),
      "\\end{tabular} &")
  
  cat(" \n \\begin{tabular}{r} \\\\ \\\\", paste0(final.table$Year[1:5], collapse = " \\\\ "),
      " \\\\ .... \\\\",
      paste0(final.table$Year[6:10], collapse = " \\\\ "),
      "\\end{tabular} &")
  
  cat(" \n \\begin{tabular}{r} \\\\ \\\\", paste0(final.table$n.drives.played[1:5], collapse = " \\\\ "),
      " \\\\ .... \\\\",
      paste0(final.table$n.drives.played[6:10], collapse = " \\\\ "),
      "\\end{tabular} &")
  
  
  
  
  
  ## The Offensive Pts Per Drive shifts
  cat("\n \\begin{tabular}{rr}",
      "\\\\ Val (Shift, 95\\% CI) \\ \\ & Rk (Shift) \\\\ \\hline ")
  # cat("\n \\begin{tabular}{rr}",
  #     "\\\\ Val (Shift, 95\\% CI) \\ \\ \\\\ \\hline ")  
  
  final.string <- NULL
  for (i in 1:5){
    final.string <- paste0(final.string, paste0(final.table[i, c(5:6)],  collapse=" & "), " \\\\ ")
  }
  
  final.string <- paste0(final.string, "... & ... \\\\")
  
  for (i in 6:10){
    final.string <- paste0(final.string, paste0(final.table[i, c(5:6)],  collapse=" & "), " \\\\ ")
  }
  
  final.string <- paste0(final.string, " \\end{tabular} & ")
  cat(final.string)
  
  
  if (side == "Offense"){
    cat("\n \\begin{tabular}{rr} ",
        "Turnovers & Start Pos \\\\ \\hline  Rk (Val) & Rk (Val) \\\\ \\hline ")
  } else {
    cat("\n \\begin{tabular}{rr} ",
        "Turnovers & Start Pos \\\\ \\hline  Rk (Val) & Rk (Val) \\\\ \\hline ")
  }
  
  final.string <- NULL
  for (i in 1:5){
    final.string <- paste0(final.string, paste0(c(final.table[i, c(14:15)]), collapse =" & "), " \\\\ ")
  }
  
  final.string <- paste0(final.string, " ... & ... \\\\")
  
  for (i in 6:10){
    final.string <- paste0(final.string, paste0(c(final.table[i, c(14:15)]), collapse =" & "), " \\\\ ")
  }
  
  
  final.string <- paste0(final.string, " \\end{tabular}")
  
  cat(final.string)
  
}
