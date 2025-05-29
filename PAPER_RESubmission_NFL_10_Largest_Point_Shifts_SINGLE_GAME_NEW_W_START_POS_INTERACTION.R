### To be included in the paper.

#########
## NFL, Single-Game adjustments (supplementary materials, Tables 5 & 6)
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
load("RESUBMISSION_NFL_LEAVE_CONTEXT_AS_IS_ORDNET_projected.points.intercept.RData")
# load("ORDNET_ordnet.fits.RData")


## Picking the season
year <- 2016

## Picking the side ("Offense" or "Defense")
side <- "Offense"

balanced <- FALSE




# #######
# ## POTENTIALLY MIGHT WANNA DO BOOTSTRAP (as CONFIDENCE INTERVALS MIGHT BE TOUGH FOR LASSO)
# ##    * 1000 replicates for each year
# ##    * Do it on the server
# #######
# 
# my.df.NFL <- NULL
# 
# for (year in 2009:2017){
# 
#   print(year)
# 
#   if (balanced){
#   # load(paste0("Bootstrap/RESUBMISSION_NFL_LEAVE_CONTEXT_AS_IS_BOOTSTRAP_", year, "_ORDNET_projected.points.intercept.RData"))
#   # load(paste0("Bootstrap/RESUBMISSION_NFL_BALANCED_", side, "_LEAVE_CONTEXT_AS_IS_BOOTSTRAP_", year, "_ORDNET_projected.points.intercept.RData"))
#  # load(paste0("Bootstrap/NEW_RESUBMISSION_NFL_BALANCED_", side, "_LEAVE_CONTEXT_AS_IS_BOOTSTRAP_", year, "_ORDNET_projected.by.game.RData"))
#   load(paste0("Bootstrap/NEW_RESUBMISSION_NFL_W_START_POS_INTERACTION_BALANCED_", side, "_LEAVE_CONTEXT_AS_IS_BOOTSTRAP_", year, "_ORDNET_projected.by.game.RData"))
# 
#     } else {
#       load(paste0("Bootstrap/NEW_RESUBMISSION_NFL_W_START_POS_INTERACTION_NOT_BALANCED_LEAVE_CONTEXT_AS_IS_BOOTSTRAP_", year, "_ORDNET_projected.by.game.RData"))
# }
#   #   Extra = Offense.Avg.Points.With.Takeaway - Offense.Avg.Points.No.Takeaway),
#   # aes(x=Offense.Avg.Points.No.Takeaway
# 
#   interm.df <- NULL
# 
#   for (b in 1:1000){
#     print(b)
#     # projected.points.intercept[[as.character(year)]][[b]][,2][[1]]
#     # projected.points.intercept[[as.character(year)]][[b]]$Offense.Avg.Points.No.Takeaway
#     # projected.points.intercept[[as.character(year)]][[b]]$Offense.Avg.Points.With.Takeaway - projected.points.intercept[[as.character(year)]][[b]]$Offense.Avg.Points.No.Takeaway
# 
#     interm.df <- rbind(interm.df,
#                        data.frame(Year = year, projected.by.game[[as.character(year)]][[b]]))
#   }
# 
#   my.df.NFL <- rbind(my.df.NFL,
#                      interm.df)
# 
# }
# 
# 
# # save(file=paste0("NEW_W_START_POS_INTERACTION_my.df.NFL.balanced.by.game.",side,".Robj"),
# #      my.df.NFL)
# 
# if (balanced){
#   save(file=paste0("NEW_W_START_POS_INTERACTION_my.df.NFL.balanced.by.game.",side,".Robj"),
#        my.df.NFL)
# } else {
#   save(file=paste0("NEW_W_START_POS_INTERACTION_my.df.NFL.not.balanced.by.game.Robj"),
#        my.df.NFL)
# }







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

# load("my.df.NFL.Robj)
load(paste0("NEW_W_START_POS_INTERACTION_my.df.NFL.not.balanced.by.game.Robj"))

if (side == "Offense"){
  
  
  boot.df <- my.df.NFL %>%
    group_by(Team, Year, game_id) %>%
    summarise(Boot.Mean.No.Complem = mean(Offense.Avg.Points.With.Complem.But.As.Is),
              Boot.Mean.With.Complem = mean(Offense.Avg.Points.Avg.Takeaways),
              CI.Left.Diff = quantile(Offense.Avg.Points.Avg.Takeaways - Offense.Avg.Points.With.Complem.But.As.Is, 0.025),
              CI.Right.Diff = quantile(Offense.Avg.Points.Avg.Takeaways - Offense.Avg.Points.With.Complem.But.As.Is, 0.975),
              Boot.Mean.Diff = Boot.Mean.With.Complem - Boot.Mean.No.Complem,
              n.drives = head(Offense.Drives.Included,1)) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(Team=Team,
              game_id = game_id,
              Value = Boot.Mean.No.Complem,
              CI.Left.Diff = CI.Left.Diff,
              CI.Right.Diff = CI.Right.Diff,
              # Rank = if (side == "Defense") rank(Boot.Mean.No.Complem, tie="first") else rank(-Boot.Mean.No.Complem, tie="first"),
              Value.1 = Boot.Mean.With.Complem,
              # Rank.1 = if (side == "Defense") rank(Boot.Mean.With.Complem, tie="first") else rank(-Boot.Mean.With.Complem, tie="first"),
              Value.Diff = Value.1 - Value,
              n.drives = n.drives
              # , Rank.Diff = Rank.1 - Rank
    )
  
} else {
  
  boot.df <- my.df.NFL %>%
    group_by(Team, Year, game_id) %>%
    summarise(Boot.Mean.No.Complem = mean(Defense.Avg.Points.With.Complem.But.As.Is),
              Boot.Mean.With.Complem = mean(Defense.Avg.Points.Avg.Takeaways),
              CI.Left.Diff = quantile(Defense.Avg.Points.Avg.Takeaways - Defense.Avg.Points.With.Complem.But.As.Is, 0.025),
              CI.Right.Diff = quantile(Defense.Avg.Points.Avg.Takeaways - Defense.Avg.Points.With.Complem.But.As.Is, 0.975),
              Boot.Mean.Diff = Boot.Mean.With.Complem - Boot.Mean.No.Complem,
              n.drives = head(Defense.Drives.Included,1)) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(Team=Team,
              game_id = game_id,
              Value = Boot.Mean.No.Complem,
              CI.Left.Diff = CI.Left.Diff,
              CI.Right.Diff = CI.Right.Diff,
              # Rank = if (side == "Defense") rank(Boot.Mean.No.Complem, tie="first") else rank(-Boot.Mean.No.Complem, tie="first"),
              Value.1 = Boot.Mean.With.Complem,
              # Rank.1 = if (side == "Defense") rank(Boot.Mean.With.Complem, tie="first") else rank(-Boot.Mean.With.Complem, tie="first"),
              Value.Diff = Value.1 - Value,
              n.drives = n.drives
              # , Rank.Diff = Rank.1 - Rank
    )
  
}

if (side == "Offense"){
  our.table.top.boot <- 
    boot.df %>%
    arrange(desc(Value.Diff)) %>%
    head(5)
  
  our.table.bottom.boot <- 
    boot.df %>%
    arrange(Value.Diff) %>%
    head(5)
} else {
  our.table.top.boot <- 
    boot.df %>%
    arrange(Value.Diff) %>%
    head(5)
  
  our.table.bottom.boot <- 
    boot.df %>%
    arrange(desc(Value.Diff)) %>%
    head(5)
}

our.table.boot <- rbind(our.table.top.boot,
                        our.table.bottom.boot)




######
## Loading all the team statistics from that season season
## to calculate the per-drive complementary statistics values
## (which will be used to demonstrate the underlying mechanism of ranking shifts)
######

pbp_by_drive_full <- NULL


for (year in 2009:2017){
  
  print("Year:")
  print(year)
  
  projected.points.intercept[[as.character(year)]] <- list()
  
  pbp_by_drive <- read.csv("NFL_2009_2017_Drive_by_Drive_Data_Cleaned.csv") %>%
    filter(Season == year)
  
  
  
  ## Just make onside kick stuff a Fumble, for simplicity
  pbp_by_drive$drive_Outcome[pbp_by_drive$drive_Outcome == "Onside Kick Recovered / Fumble"] <- "Fumble"
  
  
  table(pbp_by_drive$drive_Outcome)
  
  
  ## Getting an ID variable
  pbp_by_drive$game_id_half <- paste0(pbp_by_drive$GameID, "-", pbp_by_drive$drive_half_start)
  
  ## Non-scoring FORCED TOs, such as picks, fumbles, on-side kick recovery
  pbp_by_drive$takeaways.nonscor <- pbp_by_drive$drive_Outcome %in% c("Fumble", # "Fumble Recovery (Opponent)",
                                                                      "Interception" # "Interception Return",
                                                                      # "Kickoff Team Fumble Recovery",
                                                                      # "Punt Team Fumble Lost",
                                                                      #  "Blocked Punt Team Fumble Lost",
                                                                      #  "On-Side Kick Lost"
  )
  
  
  
  ## Any TOs, including FG misses/blocks, turnover on downs
  pbp_by_drive$turnovers.nonscor <- (pbp_by_drive$takeaways.nonscor | 
                                       pbp_by_drive$drive_Outcome %in% c(# "Blocked Field Goal",
                                         # "Field Goal Missed",
                                         "Missed Field Goal", # "Missed Field Goal Return",
                                         "Turnover on Downs" #, "Downs Turnover"
                                       )
  )
  
  pbp_by_drive$defensive.score <- pbp_by_drive$drive_Outcome %in% c("Defensive Touchdown")
  
  
  ## Punts or safeties (both result in turning the ball over via a 'punting motion', although the latter also has -2 pts)
  pbp_by_drive$punt.safety <- pbp_by_drive$drive_Outcome %in% c("Punt", "Safety")
  
  ## Number of positive runs
  # pbp_by_drive$n.positive.runs <- pbp_by_drive$n.rush - pbp_by_drive$n.stuffed.runs
  
  ## Special team return variables:
  pbp_by_drive$yds_ST_return_net <- pbp_by_drive$ST.return.yards.net
  
  
  ####
  ## Combining the yards for offense and special teams.
  ####
  if (yds.combine == TRUE){
    
    # pbp_by_drive$off.yards_gained <- pbp_by_drive$off.yards_gained + pbp_by_drive$yds_ST_return
    pbp_by_drive$off.yards_gained <- pbp_by_drive$off.ST.yards.gained
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
  
  
  pbp_by_drive$n.negative.plays.with.penalties <- pbp_by_drive$n.negative.plays
  
  
  pbp_by_drive$score_pts_by_text <- pbp_by_drive$pts.scored
  
  # There was always an issue with keeping track of extra point for defensive TD, 
  # so gotta switch from "-6" to "-7"
  
  pbp_by_drive$score_pts_by_text[pbp_by_drive$score_pts_by_text == -6] <- -7
  
  
  
  ## All the complementary statistics under consideration
  relev.var.names <- c("n.completions", "n.incompletions", "n.stuffed.runs", "n.positive.runs", "n.negative.plays.with.penalties",
                       "off.yards_gained",
                       "score_pts_by_text",
                       "yds_ST_return_net", 
                       # "yds_ST_return",
                       "n.sacks", "n.fumbles",
                       "first.downs.gained", "third.downs.converted",
                       #"takeaways.nonscor", 
                       "turnovers.nonscor",
                       "punt.safety"
  )
  
  
  
  
  # Initializing a lagged variable set
  pbp_by_drive_lag_vars <- data.frame(game_id_half = pbp_by_drive$game_id_half,
                                      pos_team = pbp_by_drive$pos_team,
                                      # pbp_by_drive[, 8:ncol(pbp_by_drive)]
                                      pbp_by_drive[, relev.var.names]
  )
  
  
  # For numerical variables, creating their lagged values, 
  for (c.ind in 3:ncol(pbp_by_drive_lag_vars)){
    pbp_by_drive_lag_vars[, c.ind] <- lag(pbp_by_drive_lag_vars[, c.ind])
    # pbp_by_drive_lag_vars[1, c.ind] <- 0 # For the initial NA value
    pbp_by_drive_lag_vars[1, c.ind] <- pbp_by_drive_lag_vars[2, c.ind] # For the initial NA value, which will get wiped out by complem.ind=0 anyway
    
  }
  
  # Making the variable names for lagged values explicit (adding "lagged" to them)
  colnames(pbp_by_drive_lag_vars)[3:ncol(pbp_by_drive_lag_vars)] <- paste0("lagged_", colnames(pbp_by_drive_lag_vars)[3:ncol(pbp_by_drive_lag_vars)])
  
  # Merging the lagged values into original data set
  pbp_by_drive <- data.frame(pbp_by_drive, pbp_by_drive_lag_vars[,-1])
  
  
  
  ## If we combine yards, then DROP the "yds_ST_return"
  #  if (yds.combine) relev.var.names <- relev.var.names[relev.var.names != "yds_ST_return"]
  
  
  #######
  ## Complementary statistics to be included in the model for testing:
  #######
  
  ## ADDING THE START_POS ONES to LAGGED VARIABLES
  ## 
  
  # pbp_by_drive$lagged_start_pos_post_takeaway <- ifelse(pbp_by_drive$lagged_takeaways.nonscor,
  #                                                       100-pbp_by_drive$off.ST.start.pos,
  #                                                       100-mean(pbp_by_drive$off.ST.start.pos[pbp_by_drive$lagged_takeaways.nonscor]))
  # 
  pbp_by_drive$lagged_start_pos_post_turnover <- ifelse(pbp_by_drive$lagged_turnovers.nonscor == 1, 
                                                        100-pbp_by_drive$off.ST.start.pos,
                                                        100-mean(pbp_by_drive$off.ST.start.pos[pbp_by_drive$lagged_turnovers.nonscor == 1]))
  
  
  pbp_by_drive_full <- rbind(pbp_by_drive_full,
                             data.frame(Year=year,
                                        pbp_by_drive))
  
}





## Calculating the per-drive average rankings on complementary football stats

if (side == "Offense"){
  our.avg.mat <- pbp_by_drive_full %>%
    group_by(Year, def_pos_team, GameID, pos_team, pos.homefield) %>%
    summarize(n.drives.played = nrow(unique(data.frame(GameID, Drive))),
              turnovers.nonscor.per.drive = mean(turnovers.nonscor, na.rm=T)) %>%
    rename(Team = def_pos_team, Opponent = pos_team) %>%
    mutate(Homefield = 1-pos.homefield) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(Team=Team,
              Opponent=Opponent,
              Homefield=Homefield,
              GameID=GameID,
              turnovers.nonscor.per.drive = turnovers.nonscor.per.drive,
              rank.turnovers.nonscor.per.drive = rank(-turnovers.nonscor.per.drive, tie="first"),
              n.drives.played = n.drives.played)
  
  
  our.avg.mat <- our.avg.mat %>% left_join(pbp_by_drive_full %>%
    group_by(Year, pos_team, GameID# , def_pos_team, pos.homefield
             ) %>%
    summarize(n.drives.played = nrow(unique(data.frame(GameID, Drive))),
              start_pos_post_turnover.per.drive = mean(lagged_start_pos_post_turnover[lagged_turnovers.nonscor == TRUE], na.rm=T)) %>%
    rename(Team = pos_team) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(Team=Team,
              GameID=GameID,
              start_pos_post_turnover.per.drive = start_pos_post_turnover.per.drive,
              rank.start_pos_post_turnover.per.drive = rank(-start_pos_post_turnover.per.drive, tie="first"))
  )
  
  
  
} else {
  
  our.avg.mat <- pbp_by_drive_full %>%
    group_by(Year, pos_team, GameID, def_pos_team, pos.homefield) %>%
    summarize(n.drives.played = nrow(unique(data.frame(GameID, Drive))),
              turnovers.nonscor.per.drive = mean(turnovers.nonscor, na.rm=T)) %>%
    rename(Team = pos_team, Opponent = def_pos_team) %>%
    mutate(Homefield = pos.homefield) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(Team=Team,
              GameID=GameID,
              Opponent=Opponent,
              Homefield=Homefield,
              turnovers.nonscor.per.drive = turnovers.nonscor.per.drive,
              rank.turnovers.nonscor.per.drive = rank(turnovers.nonscor.per.drive, tie="first"),
              n.drives.played = n.drives.played)
  
  
  our.avg.mat <- our.avg.mat %>% left_join(pbp_by_drive_full %>%
                                             group_by(Year, def_pos_team, GameID) %>%
                                             summarize(n.drives.played = nrow(unique(data.frame(GameID, Drive))),
                                                       start_pos_post_turnover.per.drive = mean(lagged_start_pos_post_turnover[lagged_turnovers.nonscor == TRUE], na.rm=T)) %>%
                                             rename(Team = def_pos_team) %>%
                                             ungroup() %>%
                                             group_by(Year) %>%
                                             summarise(Team=Team,
                                                       GameID=GameID,
                                                       start_pos_post_turnover.per.drive = start_pos_post_turnover.per.drive,
                                                       rank.start_pos_post_turnover.per.drive = rank(start_pos_post_turnover.per.drive, tie="first"))
  )
}

our.pred.val <- colnames(our.avg.mat)[str_detect(colnames(our.avg.mat), "per.drive") & !str_detect(colnames(our.avg.mat), "rank")]
our.pred.rank <- colnames(our.avg.mat)[str_detect(colnames(our.avg.mat), "per.drive") & str_detect(colnames(our.avg.mat), "rank")]




## Adding the values and rankings of teams based on the complementary football statistics

if (side == "Offense"){
  
  our.rank.mat <- data.frame(Team=our.avg.mat$Team,
                             Opponent=our.avg.mat$Opponent,
                             Homefield=our.avg.mat$Homefield,
                             game_id=factor(our.avg.mat$GameID),
                             Year=our.avg.mat$Year,
                             TO.Value=format(round(unlist(our.avg.mat[, our.pred.val[1]]),2), nsmall=2),
                             TO.Rank=our.avg.mat[, our.pred.rank[1]][[1]],
                             TO.Rank.Full=rank(-unlist(our.avg.mat[, our.pred.val[1]]), tie="first"),
                             StartPos.Value=format(round(unlist(our.avg.mat[, our.pred.val[2]]))),
                             StartPos.Rank=our.avg.mat[, our.pred.rank[2]][[1]],
                             StartPos.Rank.Full=rank(-unlist(our.avg.mat[, our.pred.val[2]]), tie="first"))
  rownames(our.rank.mat) <- NULL
  
  final.table <- our.table.boot %>% mutate(PPG.Value.Shift = ifelse(Value.Diff > 0, 
                                                                    paste0(format(round(Value.1,2), nsmall=2), " ($\\textcolor{darkgreen}{\\Uparrow}$ ", format(round(abs(Value.Diff),2), nsmall=2), ", [", format(round(CI.Left.Diff,2), nsmall=2),",", format(round(CI.Right.Diff,2), nsmall=2) ,"])"),
                                                                    ifelse(Value.Diff <0,
                                                                           paste0(format(round(Value.1,2), nsmall=2), " ($\\textcolor{red}{\\Downarrow}$ ", format(round(abs(Value.Diff),2), nsmall=2), ", [", format(round(CI.Left.Diff,2), nsmall=2),",", format(round(CI.Right.Diff,2), nsmall=2) ,"])"),
                                                                           paste0(format(round(Value.1,2), nsmall=2), " ($\\textcolor{blue}{=}", format(round(abs(Value.Diff),2), nsmall=2), "$)"))),
                                           # PPG.Rank.Shift = ifelse(Rank.Diff > 0, 
                                           #                         paste0(Rank.1, " ($\\textcolor{darkgreen}{\\Uparrow}$ ", abs(Rank.Diff), ")"),
                                           #                         ifelse(Rank.Diff <0,
                                           #                                paste0(Rank.1, " ($\\textcolor{red}{\\Downarrow}$ ", abs(Rank.Diff), ")"),
                                           #                                paste0(Rank.1, " ($\\textcolor{blue}{=}", abs(Rank.Diff), "$)")))
  ) %>%
    dplyr::select(-Value, -CI.Right.Diff, -CI.Left.Diff, -Value.Diff
                  # , -Rank,  -Rank.Diff
    ) %>%
    left_join(our.rank.mat)
  
  final.table <- final.table %>% mutate(TO.Rank.Value = paste0(TO.Rank, " (", TO.Value, ")"),
                                        StartPos.Rank.Value = paste0(StartPos.Rank, " (", StartPos.Value, ")"))
  
  
} else {
  
  our.rank.mat <- data.frame(Team=our.avg.mat$Team,
                             Opponent=our.avg.mat$Opponent,
                             Homefield=our.avg.mat$Homefield,
                             game_id=factor(our.avg.mat$GameID),
                             Year=our.avg.mat$Year,
                             TO.Value=format(round(unlist(our.avg.mat[, our.pred.val[1]]),2), nsmall=2),
                             TO.Rank=our.avg.mat[, our.pred.rank[1]][[1]],
                             TO.Rank.Full=rank(unlist(our.avg.mat[, our.pred.val[1]]), tie="first"),
                             StartPos.Value=format(round(unlist(our.avg.mat[, our.pred.val[2]]))),
                             StartPos.Rank=our.avg.mat[, our.pred.rank[2]][[1]],
                             StartPos.Rank.Full=rank(unlist(our.avg.mat[, our.pred.val[2]]), tie="first"))
  rownames(our.rank.mat) <- NULL
  
  final.table <- our.table.boot %>% mutate(PPG.Value.Shift = ifelse(Value.Diff < 0, 
                                                                    paste0(format(round(Value.1,2), nsmall=2), " ($\\textcolor{darkgreen}{\\Downarrow}$ ", format(round(abs(Value.Diff),2), nsmall=2), ", [", format(round(CI.Left.Diff,2), nsmall=2),",", format(round(CI.Right.Diff,2), nsmall=2) ,"])"),
                                                                    ifelse(Value.Diff >0,
                                                                           paste0(format(round(Value.1,2), nsmall=2), " ($\\textcolor{red}{\\Uparrow}$ ", format(round(abs(Value.Diff),2), nsmall=2), ", [", format(round(CI.Left.Diff,2), nsmall=2),",", format(round(CI.Right.Diff,2), nsmall=2) ,"])"),
                                                                           paste0(format(round(Value.1,2), nsmall=2), " ($\\textcolor{blue}{=}", format(round(abs(Value.Diff),2), nsmall=2), "$)"))),
                                           # PPG.Rank.Shift = ifelse(Rank.Diff > 0, 
                                           #                         paste0(Rank.1, " ($\\textcolor{darkgreen}{\\Uparrow}$ ", abs(Rank.Diff), ")"),
                                           #                         ifelse(Rank.Diff <0,
                                           #                                paste0(Rank.1, " ($\\textcolor{red}{\\Downarrow}$ ", abs(Rank.Diff), ")"),
                                           #                                paste0(Rank.1, " ($\\textcolor{blue}{=}", abs(Rank.Diff), "$)")))
  ) %>%
    dplyr::select(-Value, -CI.Right.Diff, -CI.Left.Diff, -Value.Diff
                  # , -Rank,  -Rank.Diff
    ) %>%
    left_join(our.rank.mat)
  
  final.table <- final.table %>% mutate(TO.Rank.Value = paste0(TO.Rank, " (", TO.Value, ")"),
                                        StartPos.Rank.Value = paste0(StartPos.Rank, " (", StartPos.Value, ")"))
  
  
}


final.table$Value.1 <- format(round(final.table$Value.1, 2), nsmall=2)



#######
## Generating the Latex code to be copy-pasted into the tabular environment
## to reproduce the tables
#######


for (j in 1){
  
  
  
  if (side == "Offense"){
    cat(paste0(c("Team \\", "Year \\", "Game \\", "Drives \\", "Points Scored (Per Drive)", "Defense Statistics (Per Drive)"), collapse = " & "), "\\\\ \\hline")
  }
  
  
  if (side == "Defense"){
    cat(paste0(c("Team \\ ", "Year \\", "Game \\", "Drives \\", "Points Allowed (Per Drive)", "Offense Statistics (Per Drive)"), collapse = " & "), "\\\\ \\hline")
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
  
 # cat(" \n \\begin{tabular}{r} \\\\ \\\\", paste0(final.table$game_id[1:5], collapse = " \\\\ "),
  cat(" \n \\begin{tabular}{r} \\\\ \\\\", 
      paste0(paste0(ifelse(final.table$Homefield[1:5] == 1, "vs ", "@ "), final.table$Opponent[1:5]), collapse = " \\\\ "),
      " \\\\ .... \\\\",
      paste0(paste0(ifelse(final.table$Homefield[6:10] == 1, "vs ", "@ "), final.table$Opponent[6:10]), collapse = " \\\\ "),
      "\\end{tabular} &")
  
  cat(" \n \\begin{tabular}{r} \\\\ \\\\", paste0(final.table$n.drives[1:5], collapse = " \\\\ "),
      " \\\\ .... \\\\",
      paste0(final.table$n.drives[6:10], collapse = " \\\\ "),
      "\\end{tabular} &")
  
  
  
  
  ## The Offensive Pts Per Drive shifts
  # cat("\n \\begin{tabular}{rr}",
  #     "\\\\ Val (Shift, 95% CI) \\ \\ & Rk (Shift) \\\\ \\hline ")    
  cat("\n \\begin{tabular}{rr}",
      "\\\\ Val (Shift, 95\\% CI) \\ \\ \\\\ \\hline ")  
  
  final.string <- NULL
  for (i in 1:5){
    final.string <- paste0(final.string, paste0(final.table[i, c(6)],  collapse=" & "), " \\\\ ")
  }
  
  final.string <- paste0(final.string, " ... & ... \\\\")
  
  for (i in 6:10){
    final.string <- paste0(final.string, paste0(final.table[i, c(6)],  collapse=" & "), " \\\\ ")
  }
  
  final.string <- paste0(final.string, " \\end{tabular} & ")
  cat(final.string)
  
  
  if (side == "Offense"){
    cat("\n \\begin{tabular}{rr} ",
       # "Turnovers & Start Pos \\\\ \\hline Rk (Val) & Rk (Val) \\\\ \\hline ")
       "TOs & Start Pos \\\\ \\hline Val & Val \\\\ \\hline ")
  } else {
    cat("\n \\begin{tabular}{rr} ",
       # "Turnovers & Start Pos \\\\ \\hline Rk (Val) & Rk (Val) \\\\ \\hline ")
       "TOs & Start Pos \\\\ \\hline Val & Val \\\\ \\hline ")
  }
  
  final.string <- NULL
  for (i in 1:5){
    final.string <- paste0(final.string, paste0(c(final.table[i, c(9,12)]), collapse =" & "), " \\\\ ")
  }
  
  final.string <- paste0(final.string, "  ... & ... \\\\")
  
  for (i in 6:10){
    final.string <- paste0(final.string, paste0(c(final.table[i, c(9,12)]), collapse =" & "), " \\\\ ")
  }
  
  
  final.string <- paste0(final.string, " \\end{tabular}")
  
  cat(final.string)
  
}