### For the paper

#####
## CFB
##
## That figure isn't included in the paper, but it is a CFB 2014 season analog
## to Figure 3(b) from the paper that showed the nature of the effects
## of consistently selected complementary statistics (yards gained and non-scoring takeaways)
## on probabilities of scoring outcomes of 2009 NFL season (league-average opponent).
#####

library(ordinalNet)

# Loading the enhanced ordinalNet code.
source("Submission_0_ordinalNet_package_source_code_enhanced.R")

library(tidyverse)
library(ggplot2)
library(fuzzyjoin)
library(gridExtra)

# We will only focus on the complementary statistics that were consistently picked
# (yards gained and non-scoring takeaways)
all.variables <- FALSE

# We will be combining the yards for offense and special teams returns.
yds.combine <- TRUE


load("RESUBMISSION_CFB_W_START_POS_ORDNET_projected.points.intercept.RData")

load("RESUBMISSION_CFB_W_START_POS_ORDNET_ordnet.fits.RData")
# load("ORDNET_ordnet.fits.RData")

tail(coef(ordnet.fits[[as.character(year)]]$with.complem), 20)

# load("ORDNET_ordnet.fits.RData")
# tail(coef(ordnet.fits[[as.character(year)]]$with.complem), 10)




projected.probabilities <- NULL

year <- 2014


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
  pbp_by_drive_lag_vars[1, c.ind] <- pbp_by_drive_lag_vars[2, c.ind] # For the initial NA value
}

# Making the variable names for lagged values explicit (adding "lagged" to them)
colnames(pbp_by_drive_lag_vars)[3:ncol(pbp_by_drive_lag_vars)] <- paste0("lagged_", colnames(pbp_by_drive_lag_vars)[3:ncol(pbp_by_drive_lag_vars)])

# Merging the lagged values into original data set
pbp_by_drive <- data.frame(pbp_by_drive, pbp_by_drive_lag_vars[,-1])




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

if (all.variables){
  relev.var.names.lagged <- paste0("lagged_", relev.var.names)
} else {
  # relev.var.names <- c("off.yards_gained",
  #                      "takeaways.nonscor")
  relev.var.names.lagged <- c("lagged_turnovers.nonscor",
                              "lagged_start_pos_post_turnover")
}



## Defining the categorical ordinal scoring outcome variable

pbp_by_drive$categ.score <- ifelse(pbp_by_drive$score_pts_by_text %in% c(6,7,8),
                                   "Offensive TD",
                                   ifelse(pbp_by_drive$score_pts_by_text == 3,
                                          "Field Goal",
                                          ifelse(pbp_by_drive$score_pts_by_text == -2,
                                                 "Safety",
                                                 ifelse(pbp_by_drive$score_pts_by_text %in% c(-6,-7,-8),
                                                        "Defensive TD",
                                                        "No Score"))))

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



pbp_by_drive.noNA <- na.omit(pbp_by_drive[, c("categ.score", "pos_team", "def_pos_team", relev.var.names.lagged, "game_id", "game_id_half", "pos.homefield", "complem.ind",
                                              "score_diff_start", "drive_time_left_in_half", "drive_half_start")] %>%
                               mutate(pos_team = factor(pos_team),
                                      def_pos_team = factor(def_pos_team),
                                      game_id = factor(game_id),
                                      game_id_half = factor(game_id_half)))



# if (stand == FALSE) {
#   pbp_by_drive.noNA <- pbp_by_drive.noNA %>%
#     mutate_if(is.numeric, scale)
# }

pbp_by_drive.noNA$complem.ind <- as.numeric(pbp_by_drive.noNA$complem.ind)

#pbp_by_drive.noNA$lagged_takeaways.nonscor <- as.numeric(pbp_by_drive.noNA$lagged_takeaways.nonscor)
pbp_by_drive.noNA$lagged_turnovers.nonscor <- as.numeric(pbp_by_drive.noNA$lagged_turnovers.nonscor)




## To avoid issue with DROPPED LEVELS (NEW HERE):
pbp_by_drive.noNA$pos_team <- droplevels(pbp_by_drive.noNA$pos_team)
pbp_by_drive.noNA$def_pos_team <- droplevels(pbp_by_drive.noNA$def_pos_team)

## Setting up the contrasts so that the intercept represented a "league-average opponent"
## (sum_i alpha_i = sum_j beta_j = 0)
contrasts(pbp_by_drive.noNA$pos_team) <- contr.sum(nlevels(pbp_by_drive.noNA$pos_team))
contrasts(pbp_by_drive.noNA$def_pos_team) <- contr.sum(nlevels(pbp_by_drive.noNA$def_pos_team))
colnames(contrasts(pbp_by_drive.noNA$pos_team)) <- rownames(contrasts(pbp_by_drive.noNA$pos_team))[-nrow(contrasts(pbp_by_drive.noNA$pos_team))]
colnames(contrasts(pbp_by_drive.noNA$def_pos_team)) <- rownames(contrasts(pbp_by_drive.noNA$def_pos_team))[-nrow(contrasts(pbp_by_drive.noNA$def_pos_team))]



ordnet.obj <- ordnet.fits[[as.character(year)]][["with.complem"]]
ordnet.no.complem.obj <- ordnet.fits[[as.character(year)]][["no.complem"]]
# ordnet.paral.obj <- ordnet.fits[[as.character(year)]][["with.complem.paral.only"]]


off.vec.probabilities.no_takeaway <- NULL
off.vec.probabilities <- NULL
off.vec.probabilities.no.complem <- NULL
off.vec.probabilities.paral.only <- NULL
off.games.included <- NULL
off.halves.included <- NULL
off.vec.probabilities.no_takeaway <- NULL 
off.vec.probabilities.with_takeaway <- NULL
off.vec.probabilities.paral.only.no_takeaway <- NULL 
off.vec.probabilities.paral.only.with_takeaway <- NULL
off.vec.probabilities.at_home <- NULL
off.vec.probabilities.away <- NULL

def.vec.probabilities <- NULL
def.vec.probabilities.no.complem <- NULL
def.vec.probabilities.paral.only <- NULL
def.games.included <- NULL
def.halves.included <- NULL
def.vec.probabilities.no_takeaway <- NULL
def.vec.probabilities.with_takeaway <- NULL
def.vec.probabilities.paral.only.no_takeaway <- NULL 
def.vec.probabilities.paral.only.with_takeaway <- NULL
def.vec.probabilities.at_home <- NULL
def.vec.probabilities.away <- NULL


contr.mat.pos_team <- contrasts(pbp_by_drive.noNA$pos_team)
contr.mat.def_pos_team <- contrasts(pbp_by_drive.noNA$def_pos_team)

mean.vec <- as.matrix(t(apply(pbp_by_drive.noNA[, relev.var.names.lagged],
                              2,
                              mean)))


#########
## Making the stacked plot for the effects of yards gained
## on scoring outcomes for the league-average opponent.
##########

pts.proj <- predict.ordinalNet.mine(ordnet.fits[[as.character(year)]]$with.complem) %*% c(-7,-2,0,3,7)
start.pos <- pbp_by_drive.noNA$lagged_start_pos_post_turnover

plot(pts.proj[pbp_by_drive.noNA$lagged_turnovers.nonscor == 1] ~ start.pos[pbp_by_drive.noNA$lagged_turnovers.nonscor == 1])

X.obj <- ordnet.fits[[as.character(year)]]$with.complem$args$x
our.pred.mat <- X.obj[1:100, ]
our.pred.mat[, str_detect(colnames(our.pred.mat), "pos_team")] <- 0
our.pred.mat[, !str_detect(colnames(our.pred.mat), "pos_team|lagged")] <- apply(our.pred.mat[, !str_detect(colnames(our.pred.mat), "pos_team|lagged")],
                                                                                2,
                                                                                function(col) rep(mean(col), length(col)))
our.pred.mat[,"complem.ind:lagged_turnovers.nonscor"] <- 1
our.pred.mat[,"complem.ind:lagged_turnovers.nonscor:lagged_start_pos_post_turnover"] <- c(1:100)

our.pred <- data.frame(predict.ordinalNet.mine(ordnet.fits[[as.character(year)]]$with.complem,
                                               newx=our.pred.mat),
                       lagged_start_pos_post_turnover = c(1:100))

colnames(our.pred)[1:nlevels(pbp_by_drive.noNA$categ.score)] <- paste0(1:5, ". ", levels(pbp_by_drive.noNA$categ.score))


our.pred.long_nonparallel_yds <- gather(our.pred, Outcome, prob, 1:5)

## The actual plot:
# Width: 393 Height: 344
print(ggplot(our.pred.long_nonparallel_yds, aes(x = lagged_start_pos_post_turnover, y = prob, fill = Outcome)) +
        geom_area() + 
        xlab("Offense's Starting Position\nAfter a Defensive Turnover") + 
        ylab("Probability") + 
        ggtitle("Impact of starting position after turnover\non scoring probabilities"))
