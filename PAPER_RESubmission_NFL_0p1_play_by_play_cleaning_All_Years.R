#########
##  Cleaning the initial raw dataset, combining it into a drive-by-drive format.
## 
##  Saving it into a csv file as follows:
##    write_csv(drive.by.drive, "./NFL_2009_2017_Drive_by_Drive_Data.csv")
########

## Necessary Packages

library(dplyr)
library(tidyverse)


## Obtain Drive Outcomes for each game of each Season


all_seasons <- read.csv("./NFL Play by Play 2009-2017 (v4).csv")
drive.by.drive <- NULL #Initialize drive by drive

# season <- 2009

for(season in unique(all_seasons$Season) ){
  print("Year:")
  print(season)
  
  play_by_play <- all_seasons %>% filter(Season == season)
  nOrig <- length(unique(play_by_play$GameID))
  print(nOrig)
  
  ## Define Half variable
  play_by_play$Half <- ifelse(play_by_play$qtr %in% c(1,2),
                              1,
                              ifelse(play_by_play$qtr %in% c(3,4),
                                     2,
                                     "OT"))
  
  # where PlayType = "Timeout" or "Two Minute Warning"
  play_by_play <- play_by_play %>% 
    filter(!(PlayType %in% c("Timeout", "Two Minute Warning")))
  
  # where PlayType = "Quarter End" and qtr = 1, 3
  play_by_play <- play_by_play %>%
    filter(!(PlayType == "Quarter End" & qtr %in% c(1,3)))
  
  ## Convert Penalty.Yards into signed version (if posteam is penalized, sign is negative) within each drive
  play_by_play$Penalty.Yards <- ifelse(is.na(play_by_play$Penalty.Yards),
                                       0, play_by_play$Penalty.Yards)
  
  ## Assign sign to Penalty.Yards
  play_by_play$Penalty.Yards <- ifelse(play_by_play$posteam ==
                                         play_by_play$PenalizedTeam, -1*play_by_play$Penalty.Yards,
                                       play_by_play$Penalty.Yards)
  
  
  #####
  ## Variable describing field position change (yrdln[current]-yrdln[next]) within each game, each drive
  #####
  
  play_by_play$pos_net <- c(-1*diff(play_by_play$yrdline100), NA)
  # play_by_play$pos_net[which(play_by_play$Drive != lead(play_by_play$Drive))] <- NA
  # play_by_play$yrdline100[which(play_by_play$posteam != lead(play_by_play$posteam))][1:6]
  # play_by_play$yrdline100[which(play_by_play$posteam != lead(play_by_play$posteam))+1][1:6]
  
  # For the "possession switching" plays within a half (make it "current_to_go - (100-after_to_go)")
  our.ind <- which((play_by_play$GameID == lead(play_by_play$GameID)) &
                     (play_by_play$Half == lead(play_by_play$Half)) &
                     play_by_play$posteam != lead(play_by_play$posteam))
  play_by_play$pos_net[our.ind] <-  play_by_play$yrdline100[our.ind] - (100-play_by_play$yrdline100[our.ind+1])
  
  # For the half-switching plays (make them NA)
  our.ind <- which(play_by_play$Half != lead(play_by_play$Half))
  play_by_play$pos_net[our.ind] <-  NA
  
  ## Get the "ST.air.yardage" on punts & kicks
  
  play_by_play$ST.air.yardage <- NA
  
  our.ind <- which(str_detect(tolower(play_by_play$desc), " kicks ") 
                   & !str_detect(tolower(play_by_play$desc), "no play")
                   )
  
  if (length(our.ind) > 0){
    play_by_play$ST.air.yardage[our.ind] <- sapply(tolower(play_by_play$desc[our.ind]),
                                                   function(x) as.numeric(unlist(str_extract_all(x, "(?<=kicks |kicks onside )-?\\d+"))[1]))
  }
    # Figuring out which entry has more than 1 of "kicks" ,"kicks onside" substrings
    ## Bad entries - just doubled up. Pick the first number

    
  our.ind <- which(str_detect(tolower(play_by_play$desc), " punts ") 
                    & !str_detect(tolower(play_by_play$desc), "no play")
                   )
  
  # play_by_play$desc[our.ind]
  if (length(our.ind) > 0){
  play_by_play$ST.air.yardage[our.ind] <- sapply(tolower(play_by_play$desc[our.ind]),
                                                 function(x) head(as.numeric(unlist(str_extract_all(x, "(?<=punts )-?\\d+"))[1]),1))
  }
  
  our.ind <- which(str_detect(tolower(play_by_play$desc), " punt is blocked ") & !str_detect(tolower(play_by_play$desc), "no play"))
  play_by_play$desc[our.ind]
  if (length(our.ind) > 0){
  play_by_play$ST.air.yardage[our.ind] <- 0
  }
  
  
  our.ind <- which(str_detect(tolower(play_by_play$desc), " fake punt") & !str_detect(tolower(play_by_play$desc), "no play"))
  # play_by_play$desc[our.ind]
  if (length(our.ind) > 0){
  play_by_play$ST.air.yardage[our.ind] <- sapply(tolower(play_by_play$desc[our.ind]),
                                                 function(x) head(as.numeric(unlist(str_extract_all(x, "(?<=for )-?\\d+"))[1]),1))
  # In case it's NA/Incomplete pass on a fake
  play_by_play$ST.air.yardage[our.ind] <- ifelse(is.na(play_by_play$ST.air.yardage[our.ind]), 0, play_by_play$ST.air.yardage[our.ind])
  }
  

  # View(play_by_play[sort(c(our.ind, our.ind+1)), ] %>%
  #        select(GameID, Drive, desc, PlayType, posteam, DefensiveTeam, yrdline100))
  
  
  
  
  
  # Locate any mismatches between pos_net
  mismatch <- which(play_by_play$pos_net != play_by_play$Yards.Gained + play_by_play$Penalty.Yards)
  
  ## Create a Kickoff Return.Yards variable
  play_by_play$Return.Yards <- ifelse(play_by_play$PlayType == "Kickoff",
                                      play_by_play$Yards.Gained, NA)
  
  mismatch_df <- play_by_play[mismatch,] %>% select(Drive, posteam, yrdln, yrdline100, pos_net, Yards.Gained, Return.Yards, Penalty.Yards, Accepted.Penalty, desc, PlayType, qtr) %>% filter(PlayType != "No Play")
  
  ## Create Fumble.Lost variable
  play_by_play$Fumble.Lost <- play_by_play$Fumble
  play_by_play$Fumble.Lost <- ifelse(play_by_play$Fumble == 1 & play_by_play$Drive != lead(play_by_play$Drive), 1, 0)
  
  ## All instances of a fumble recovered for touchdown
  fmb_td_index <- which(play_by_play$Touchdown == 1 & play_by_play$Fumble == 1 & play_by_play$Fumble.Lost == 0)
  
  ## All instances of a touchdown
  td_index <- which(play_by_play$Touchdown == 1)
  
  ## Home.Team.Sc and Away.Team.Sc
  play_by_play$Home.Team.Sc <- ifelse(play_by_play$posteam == play_by_play$HomeTeam, play_by_play$PosTeamScore, play_by_play$DefTeamScore)
  
  play_by_play$Away.Team.Sc <- ifelse(play_by_play$posteam == play_by_play$AwayTeam, play_by_play$PosTeamScore, play_by_play$DefTeamScore)
  
  ## Home.Team.Points and Away.Team.Points
  play_by_play$Home.Team.Points <- c(diff(play_by_play$Home.Team.Sc), NA)
  play_by_play$Away.Team.Points <- c(diff(play_by_play$Away.Team.Sc), NA)
  
  # Makes sure that differences don't include different games
  play_by_play$Home.Team.Points[which(play_by_play$GameID != lead(play_by_play$GameID))] <- NA
  play_by_play$Away.Team.Points[which(play_by_play$GameID != lead(play_by_play$GameID))] <- NA
  
  ## Assign 3 points to home and away teams where their score is NA & field goal made
  home_away_NAs <- which(is.na(play_by_play$Home.Team.Points) & play_by_play$FieldGoalResult == "Good")
  
  play_by_play$Home.Team.Points[home_away_NAs] <- ifelse(play_by_play$posteam[home_away_NAs] == play_by_play$HomeTeam[home_away_NAs], 3, 0)
  
  play_by_play$Away.Team.Points[home_away_NAs] <- ifelse(play_by_play$posteam[home_away_NAs] == play_by_play$AwayTeam[home_away_NAs], 3, 0)
  
  ## Assign 6 points to home and away teams where their score is NA & TD scored
  TD_home_away_NAs  <- which(is.na(play_by_play$Home.Team.Points) &
                               play_by_play$Touchdown == 1)
  
  play_by_play$Home.Team.Points[TD_home_away_NAs] <- ifelse(play_by_play$posteam[TD_home_away_NAs] == play_by_play$HomeTeam[TD_home_away_NAs], 6, 0)
  
  play_by_play$Away.Team.Points[TD_home_away_NAs] <- ifelse(play_by_play$posteam[TD_home_away_NAs] == play_by_play$AwayTeam[TD_home_away_NAs], 6, 0)
  # We're yet to know if these TDs are offensive or defensive
  
  ## Assign 2 points to home and away teams where their score is NA & safety scored
  sfty_NAs <- which(is.na(play_by_play$Home.Team.Points) & play_by_play$Safety == 1)
  
  play_by_play$Home.Team.Points[sfty_NAs] <- ifelse(
    play_by_play$DefensiveTeam[sfty_NAs] == play_by_play$HomeTeam[sfty_NAs], 2, 0)
  
  play_by_play$Away.Team.Points[sfty_NAs] <- ifelse(
    play_by_play$DefensiveTeam[sfty_NAs] == play_by_play$AwayTeam[sfty_NAs], 2, 0)
  
  ## Remove the half where TD and Safety == 1
  badIndex <- which(play_by_play$Touchdown == 1 & play_by_play$Safety == 1)
  play_by_play <- play_by_play %>% filter(!(play_by_play$GameID %in% play_by_play$GameID[badIndex]))
  
  ## Check for "clean" punts that are NOT of "Punt" playtype
  clean.punt <- which(!is.na(play_by_play$PuntResult) & play_by_play$PlayType != "Punt")
  
  ## Sanity check to see if drive no. of punt play is different from drive no. of next play
  bad.punt.indx <- which(play_by_play$PlayType == "Punt" & play_by_play$Drive == lead(play_by_play$Drive) & !str_detect(tolower(play_by_play$desc), "fake punt"))
  
  ## Removes instances of the above ---Big Removal of Games
  badGameIDs <- play_by_play$GameID[bad.punt.indx]
  print("REMOVING GAMES:")
  print(length(badGameIDs))
  play_by_play <- play_by_play %>% filter(!(GameID %in% badGameIDs))
  # print(length(unique(play_by_play$GameID)))
  
  ## Punt not followed by 2 consecutive plays of same drive
  punt_indx2 <- which(!is.na(play_by_play$PuntResult) & !(play_by_play$Drive == lead(play_by_play$Drive) & play_by_play$Drive == lead(play_by_play$Drive, 2)))
  
  # Switches
  play_by_play[punt_indx2, c("posteam", "DefensiveTeam", "PosTeamScore", "DefTeamScore")] <- play_by_play[punt_indx2, c("DefensiveTeam", "posteam", "DefTeamScore", "PosTeamScore")]
  play_by_play[punt_indx2, c("ScoreDiff", "pos_net")] <- -play_by_play[punt_indx2, c("ScoreDiff", "pos_net")]
  play_by_play[punt_indx2, "yrdline100"] <- 100 - play_by_play[punt_indx2, "yrdline100"]
               
               
  ## Change drive number into the next drive number
  play_by_play$Drive[punt_indx2] <- play_by_play$Drive[punt_indx2]+1
  
  ## Create Pos.Team variable
  play_by_play$Pos.Team.Points <- ifelse(play_by_play$posteam == play_by_play$HomeTeam,
                                         ifelse(play_by_play$Home.Team.Points >= play_by_play$Away.Team.Points,
                                                play_by_play$Home.Team.Points,
                                                -1*play_by_play$Away.Team.Points),
                                         ifelse(play_by_play$Away.Team.Points >= play_by_play$Home.Team.Points,
                                                play_by_play$Away.Team.Points,
                                                -1*play_by_play$Home.Team.Points))
  
  ## Extracting last play for each drive of every Game
  last.play.drive <- play_by_play %>% group_by(GameID, Drive) %>% summarize(nxt.drive.first.play = head(PlayType,1), nxt.drive.punt.result = head(PuntResult,1), nxt.drive.qtr = head(qtr,1), points = tail(Pos.Team.Points,1), qtr = tail(qtr,1), down = tail(down,1), PlayType = tail(PlayType,1), desc = tail(desc,1), Fumble.Lost = tail(Fumble.Lost,1), InterceptionThrown = tail(InterceptionThrown,1), FieldGoalResult = tail(FieldGoalResult,1), ChalReplayResult = tail(ChalReplayResult,1), TwoPointConv = tail(TwoPointConv,1), Onsidekick = tail(Onsidekick,1), posteam = head(posteam,1)) %>% ungroup() %>% mutate(nxt.drive.first.play = lead(nxt.drive.first.play), nxt.drive.punt.result = lead(nxt.drive.punt.result), nxt.drive.qtr = lead(nxt.drive.qtr), .by="GameID")
  
  ## Last play was 6 points without extra point following it
  no.xtra.pts <- last.play.drive %>% filter(points == 6)
  
  bad.drive.end <- play_by_play %>% filter(GameID %in% no.xtra.pts$GameID) %>% group_by(GameID) %>% summarize(last.drive = tail(Drive,1)) %>% left_join(no.xtra.pts) %>% 
    filter(last.drive != Drive) %>% .[["GameID"]]
  
  ## Removing bad.drive.end (GameID) games from play_by_play ---Big Removal of Games
  print("REMOVING GAMES:")
  print(length(unique(bad.drive.end)))
  play_by_play <- play_by_play %>% filter(!(GameID %in% bad.drive.end))
  # print(length(unique(play_by_play$GameID)))

  
  ## Initializing drive outcome
  last.play.drive$Drive.Outcome <- NA
  
  ## When last.play.points = 3 (field goal)
  last.play.drive$Drive.Outcome[last.play.drive$points == 3] <- "Made Field Goal"
  
  ## When last.play.points = -2 (safety)
  last.play.drive$Drive.Outcome[last.play.drive$points == -2] <- "Safety"
  
  ## When last.play.points = 1, 2, or 6 (off TD)
  last.play.drive$Drive.Outcome[last.play.drive$points %in% c(1,2,6) | last.play.drive$PlayType == "Extra Point" | !is.na(last.play.drive$TwoPointConv)] <- "Offensive Touchdown"
  
  ## When last.play.points == -6 (def TD)
  last.play.drive$Drive.Outcome[last.play.drive$points == -6] <- "Defensive Touchdown"
  
  ## Drive ends with half end
  last.play.drive$Drive.Outcome[(last.play.drive$PlayType %in% c("Half End", "End of Game")) | (last.play.drive$PlayType == "Quarter End" & last.play.drive$qtr %in% c(2,4,5))] <- "End of Half"
  
  ## Drive ends with interception
  last.play.drive$Drive.Outcome[last.play.drive$points == 0 & last.play.drive$InterceptionThrown == 1] <- "Interception"
  
  ## Drive ends with fumble
  #Change Fumble indicator to 0 due to reversal
  fmb_aft <- str_detect(str_remove_all(tolower(last.play.drive$desc), ".*reverse"), "fumble")
  last.play.drive$Fumble.Lost[!fmb_aft] <- 0
  
  last.play.drive$Drive.Outcome[last.play.drive$Fumble.Lost == 1] <- "Fumble"
  
  ## Drive ends with missed or blocked field goal
  last.play.drive$Drive.Outcome[last.play.drive$FieldGoalResult %in% c("No Good", "Blocked")] <- "Missed Field Goal"
  
  ## Drive ends with punt
  last.play.drive$Drive.Outcome[which(!is.na(last.play.drive$nxt.drive.punt.result))] <- "Punt"
  
  ## Quarter transitions from 2 to 3 or from 4 to 5 (OT)
  last.play.drive$Drive.Outcome[which(((last.play.drive$qtr == 2 & last.play.drive$nxt.drive.qtr == 3) | (last.play.drive$qtr == 4 & last.play.drive$nxt.drive.qtr == 5)) & is.na(last.play.drive$Drive.Outcome))] <- "End of Half"
  
  ## Remove badly recorded games ---Big Removal of Games
  bad.record.ind <- which(!is.na(last.play.drive$nxt.drive.first.play) & last.play.drive$down != 4 & is.na(last.play.drive$Drive.Outcome))
  bad.record <- last.play.drive$GameID[bad.record.ind]
  
  print("REMOVING GAMES:")
  print(length(unique(bad.record)))

  play_by_play <- play_by_play %>% filter(!(GameID %in% bad.record))
  # print(length(unique(play_by_play$GameID)))
  last.play.drive <- last.play.drive %>% filter(!(GameID %in% bad.record))
  
  ## Drive ends with time running out
  last.play.drive$Drive.Outcome[which(((is.na(last.play.drive$nxt.drive.first.play) & last.play.drive$down != 4) | (last.play.drive$down == 4 & last.play.drive$PlayType == "Punt")) & is.na(last.play.drive$Drive.Outcome))] <- "End of Half"
  
  ## Drive involves onside kick recovery
  last.play.drive$Drive.Outcome[last.play.drive$Onsidekick == 1 & last.play.drive$posteam != lead(last.play.drive$posteam) & str_detect(last.play.drive$desc, "RECOVER")] <- "Onside Kick Recovery"
  
  ## Drive ends with turnover on downs
  last.play.drive$Drive.Outcome[is.na(last.play.drive$Drive.Outcome)] <- "Turnover on Downs"
  
  
  ##########
  ##########
  
  dim(play_by_play)
  summary(play_by_play$TimeSecs)
  # boxplot(play_by_play$TimeSecs)
  play_by_play$time[play_by_play$TimeSecs < 0]
  play_by_play$qtr
  
 # play_by_play$drive_time_left_in_half <- 
  
  
  # Get into the DRIVE_BY_DRIVE for the FIRST PLAY:
  # "qtr", "Half", "posteam", "defposteam", 
  # "time", "TimeSecs", "TimeUnder"
  # "AwayTimeoutsRemainingPre", "HomeTimeoutsRemainingPre",
  # "ScoreDiff", "PosTeamScore", "DefTeamScore"
  
  ##########
  ##########
  
  
  
  
  
  ## Merge last.play.drive with play_by_play
  play_by_play <- play_by_play %>% left_join(last.play.drive %>% select(GameID, Drive, Drive.Outcome))
  
  
  
  ########
  ## CHECKING DRIVES that have SEVERAL "POS_TEAM" VALUES
  ########
  
  
  ### If the first play is a PUNT, and on the following play the POSTEAM CHANGES => gotta subtract "0.5" from the drive # for the PUNT PLAY
 our.ind <- which( (play_by_play$Drive == lead(play_by_play$Drive)) & 
                    (play_by_play$PlayType == "Punt")
                   & (play_by_play$posteam != lead(play_by_play$posteam)) 
                   &  (play_by_play$posteam != "") & (lead(play_by_play$posteam) != "")
 )
  
  print("Punt drive start issues:")
  print(length(our.ind))
 
  ## For points, just make corresponding drive outcomes (6 - Offensive TD, -6 - defensive TD, 2 - safety)
  print(table(play_by_play$Pos.Team.Points[our.ind],
              play_by_play$Touchdown[our.ind]))
  
  play_by_play$Drive.Outcome[our.ind] <- ifelse(play_by_play$Pos.Team.Points[our.ind] == 6,
                                                "Offensive Touchdown",
                                                ifelse(play_by_play$Pos.Team.Points[our.ind] == -6,
                                                       "Defensive Touchdown",
                                                       ifelse(play_by_play$Pos.Team.Points[our.ind] == 2,
                                                              "Safety",
                                                              "Fumble")))
  
  ## Most of the 0-pointers are fumbles & muffs, so JUST MAKE THOSE DRIVE OUTCOMES "Fumble"
  # View(play_by_play[our.ind[which(play_by_play$Pos.Team.Points[our.ind] == 0)], c("desc", "Fumble", "Fumble.Lost", "InterceptionThrown")])
  
  # table(play_by_play$Drive.Outcome[our.ind],
  #       play_by_play$Touchdown[our.ind])
  
 
 play_by_play$Drive[our.ind] <- play_by_play$Drive[our.ind]-0.5

 
 ###
 ## "Punt" that also was a SAFETY => 
 ##     need to switch the drive number there (just subtract 0.5), AND make sure POINTS are "+2"
 ###
 our.ind <- which(play_by_play$PlayType == "Punt" & (play_by_play$Pos.Team.Points == 2 | str_detect(tolower(play_by_play$desc), "safety")))
 our.ind
 
 play_by_play$Drive[our.ind] <- play_by_play$Drive[our.ind]-0.5
 play_by_play$Drive.Outcome[our.ind] <- "Safety"
 play_by_play$Pos.Team.Points[our.ind] <- 2
 
 
 
 print("Points for 'NO PLAY' types:")
 print(table(play_by_play$Pos.Team.Points[play_by_play$PlayType == "No Play"]))
 
 
 ###
 ## PUNT MUFF TD ( where drive number didn't change) => DEFENSIVE TD !!!
 ###
 our.ind <- which(play_by_play$PlayType == "Punt" & 
                    (play_by_play$Pos.Team.Points == 6) & 
                    play_by_play$Drive == lead(play_by_play$Drive))
 our.ind
 
 
#  View(play_by_play[sort(c(our.ind, our.ind+1)), ])
 
 play_by_play$Drive[our.ind] <- play_by_play$Drive[our.ind]-0.5
 play_by_play$Drive.Outcome[our.ind] <- "Defensive Touchdown"
 play_by_play$Pos.Team.Points[our.ind] <- -6
 

 
 # View(play_by_play[sort(c(our.ind, our.ind+1)), ])
 
 
 
 ###
 ## "No Play" that also was a SAFETY benefitting the PUNT-RETURNING TEAM => 
 ##     need to switch the drive number there (just subtract 0.5), AND make sure POINTS are "+2"
 ###
 
 our.ind <- which(play_by_play$PlayType == "No Play" & (play_by_play$Pos.Team.Points == 2 
                                                        # | str_detect(tolower(play_by_play$desc), "safety")
                                                        ))
 our.ind
 
 play_by_play$Drive[our.ind] <- play_by_play$Drive[our.ind]-0.5
 play_by_play$Drive.Outcome[our.ind] <- "Safety"
 play_by_play$Pos.Team.Points[our.ind] <- 2
 
 
 
 
#  View(play_by_play[our.ind, ])
 
 
 ### If the first play is a KICKOFF, then it's most probably a SUCCESSFUL ONSIDE KICK or just RECOVERED FUMBLE
 ##  Gotta subtract 0.5 from that drive # as well, marking it a FUMBLE
 
 our.ind <- which( (play_by_play$Drive == lead(play_by_play$Drive)) & 
                     (play_by_play$PlayType == "Kickoff")
                   & (play_by_play$posteam != lead(play_by_play$posteam)) 
                   &  (play_by_play$posteam != "") & (lead(play_by_play$posteam) != "")
 )
 
 print("Kickoffs, Onside/Fumble recoveries:")
 print(length(our.ind))
 
 play_by_play$Drive[our.ind] <- play_by_play$Drive[our.ind]-0.5
 play_by_play$Drive.Outcome[our.ind] <- "Onside Kick Recovered / Fumble"
 
# View(play_by_play[our.ind, ])
 # View(play_by_play[sort(c(our.ind-1, our.ind, our.ind+1)), ])
 
 
 
 ## NO PLAY / PENALTIES ASSESSED ON KICKOFF
 ## => No change really needed per se.. a bit annoying that the possession switches within a drive, but nothing crazy.
 
 our.ind <- which( (play_by_play$Drive == lead(play_by_play$Drive)) & 
                     (play_by_play$PlayType == "No Play") & (lead(play_by_play$PlayType) == "Kickoff")
                   & (play_by_play$posteam != lead(play_by_play$posteam)) 
                   &  (play_by_play$posteam != "") & (lead(play_by_play$posteam) != "")
 )
 
 print("NO PLAY cases (seemingly penalties enforced on kickoffs):")
 print(length(our.ind))


 ## OTHER STUFF
 ##
 ## "Under review" - simply gives a "pause" to action... JUST 3 CASES LIKE THAT ACROSS ALL YEARS
 our.ind <- which( (play_by_play$Drive == lead(play_by_play$Drive)) & 
                     !(play_by_play$PlayType %in% c("Kickoff", "Punt") | ((play_by_play$PlayType == "No Play") & (lead(play_by_play$PlayType) == "Kickoff"))  )
                   & (play_by_play$posteam != lead(play_by_play$posteam)) 
                   &  (play_by_play$posteam != "") & (lead(play_by_play$posteam) != "")
 )
 
 print("OTHER CASES OF SWITCHING POSTEAM WITHIN A DRIVE:")
 print(length(our.ind))
 
 # print(play_by_play$PlayType[our.ind])
# print(play_by_play$Drive.Outcome[our.ind])
 # print(play_by_play$desc[our.ind])
 
 ## Just in case, make sure the drive outcome isn't carrying over from the previous drive
 if (length(our.ind) > 0){
  play_by_play$Drive.Outcome[our.ind] <- play_by_play$Drive.Outcome[our.ind+1]
 }

 
 ###
 # PUNT where drive # DIDN'T CHANGE from previous play for some reason, while the punt itself resulted in a SAFETY
 ##  => need to change the punt play # by +0.5
 our.ind <- which(play_by_play$PlayType == "Punt" & 
                     (play_by_play$Pos.Team.Points == 2) & 
                    play_by_play$Drive == lag(play_by_play$Drive))
 our.ind
 
 play_by_play$Drive[our.ind] <- play_by_play$Drive[our.ind] + 0.5
 play_by_play$Drive.Outcome[our.ind] <- "Safety"
 play_by_play$Pos.Team.Points[our.ind] <- -2
 
 
 print("PUNT where drive # didn't change, and it was a safety")
 print(length(our.ind))
 
 
 
 
 # View(play_by_play[our.ind, ])
 
 # View(play_by_play %>%
 #        filter(GameID == "2017102903", Drive == 6))

  # View(play_by_play %>%
  #        filter(GameID == "2010112107", Drive %in% c(17,18)))
  
 
 
 
  ### SEEMS TO BE NO CASES WITH >1 DRIVE OUTCOMES (WHICH IS GOOD)
  
  # View(play_by_play %>%
  #        group_by(GameID, Drive) %>%
  #        summarise(n.pos.teams=length(unique(posteam[posteam != ""])),
  #                  n.drive.outcomes = length(unique(Drive.Outcome)),
  #                  first.play.type = PlayType[1],
  #                  first.play.Touchdown = Touchdown[1],
  #                  first.play.desc = desc[1],
  #                  first.play.posteam = posteam[1],
  #                  second.play.type = PlayType[2],
  #                  second.play.Touchdown = Touchdown[2],
  #                  second.play.desc = desc[2],
  #                  second.play.posteam = posteam[2],
  #                  last.play.type = tail(PlayType,1),
  #                  last.play.Touchdown = tail(Touchdown, 1),
  #                  last.play.desc = tail(desc, 1),
  #                  last.play.posteam = tail(posteam,1)) %>%
  #        filter(n.drive.outcomes == 1))
  




  
  
  # nNew <- length(unique(play_by_play$GameID))
  # print(season)
  # print(1 - (nNew/nOrig))
  # print(table(last.play.drive$Drive.Outcome))
  
  
  # "qtr", "Half", "posteam", "defposteam", 
  # "time", "TimeSecs", "TimeUnder"
  # "AwayTimeoutsRemainingPre", "HomeTimeoutsRemainingPre",
  # "ScoreDiff", "PosTeamScore", "DefTeamScore"
  
 
 

 
 ## Adding a lead yrdline100 variable (for next drive's first play's yardage - very helpful in case of punts, or just generally for positional change stuff
 ## MAKE SURE TO KEEP IT FROM STANDPOINT OF THE TEAM THAT POSSESSES THE BALL ON THE CURRENT PLAY (SO FLIP IN CASE OF FLIPPED POSSESSION)
 play_by_play <- play_by_play %>%
   mutate(lead.yrdline100 = ifelse(Half != lead(Half),
                                   NA,
                                   ifelse(posteam != lead(posteam),
                                          100-lead(yrdline100),
                                          lead(yrdline100))))
 
 
  ## Calculate Numerical Statistics
  drive.by.drive <- rbind(drive.by.drive,  play_by_play %>% 
                            group_by(Season, Date, GameID, Drive) %>% 
                            summarize(drive_qtr_start = head(qtr,1),
                                      drive_half_start = ifelse(head(Half, 1) == "1",
                                                                "1st",
                                                                ifelse(head(Half, 1) == "2",
                                                                       "2nd",
                                                                       "OT")),
                                      pos_team = head(posteam, 1),
                                      def_pos_team = head(DefensiveTeam, 1),
                                      pos.homefield = ifelse(HomeTeam[1] == pos_team, 1, 0),
                                      drive_time_start = head(time, 1),
                                      drive_TimeSecs_start = head(TimeSecs, 1),
                                      drive_TimeUnder_start = head(TimeUnder, 1),
                                      drive_HomeTimeoutsRemainingPre_start = head(HomeTimeouts_Remaining_Pre, 1),
                                      drive_AwayTimeoutsRemainingPre_start = head(AwayTimeouts_Remaining_Pre, 1), 
                                      drive_ScoreDiff_start = head(ScoreDiff, 1),
                                      drive_PosTeamScore_start = head(PosTeamScore, 1), 
                                      drive_DefTeamScore_start = head(DefTeamScore, 1),
                                      drive_Outcome = head(Drive.Outcome, 1),
                                      n.sacks = sum(Sack),
                                      n.completions = sum(PassOutcome == "Complete", na.rm=T),
                                      n.incompletions = sum(PassOutcome == "Incomplete Pass", na.rm=T),
                                      n.stuffed.runs = sum(RushAttempt == 1 & pos_net <= 0, na.rm=T),
                                      n.positive.runs = sum(RushAttempt == 1 & pos_net > 0, na.rm=T),
                                      n.negative.plays = sum(pos_net < 0 & PlayType != "Kickoff", na.rm=T),
                                      n.fumbles = sum(Fumble == 1, na.rm=T),
                                      first.downs.gained = sum(FirstDown == 1 & pos_net > 0, na.rm=T),
                                      third.downs.converted = sum(FirstDown == 1 & pos_net > 0 & lag(down) == 3, na.rm=T),
                                      takeaway.nonscore = ifelse((InterceptionThrown[1] == 1 | Fumble.Lost[1] == 1 | (Onsidekick[1] == 1 & posteam[1] != lead(posteam[1]) & str_detect(desc[1], "RECOVER"))) & Drive.Outcome[1] != "Defensive Touchdown",1,0),
                                      turnover.nonscore = ifelse(takeaway.nonscore[1] == 1 | Drive.Outcome[1] %in% c("Missed Field Goal", "Turnover on Downs"),1,0),
                                      punt.safety = ifelse(Drive.Outcome[1] %in% c("Punt", "Safety"),1,0),
                                      pts.scored = sum(Pos.Team.Points, na.rm=T),
                                      
                                      ## OLD VERSION
                                      # ST.return.yards.net = ifelse((head(PlayType,1) %in% c("Kickoff", "Punt")) | !str_detect(tolower(desc[1]),"touchback") | !str_detect(tolower(desc[1]),"fair catch") | !str_detect(tolower(desc[1]),"penalty"), 
                                      #                              tail(as.numeric(unlist(str_extract_all(desc[1], "[+-]?\\d+"))),1), 
                                      #                              0),
                                      # off.ST.start.pos = ifelse(head(PlayType,1) %in% c("Kickoff", "Punt"), head(yrdline100,2)[2], head(yrdline100,1)),
                                      # off.ST.yards.gained = off.ST.start.pos-ifelse(head(Drive.Outcome,1) != "Offensive Touchdown",
                                      #                                               ifelse(head(Drive.Outcome,1) == "Punt",
                                      #                                                      off.ST.start.pos-tail(as.numeric(unlist(str_extract_all(desc, "[+-]?\\d+"))),1), tail(yrdline100,1)),
                                      #                                               0) +ST.return.yards.net,
                                      
                                      ## NEW VERSION
                                      
                                      ## ST.return.yards.net:
                                      ##    - If it's a returning team's score (TD or Safety): we get the yrdline100 (the starting "to go" value, as the team clearly went that way to score)
                                      ##    - If it's punting team's score (muffed TD, or Safety): we get 100-yrdline100, so the reverse of the above
                                      ##    - Otherwise, we just get the pos-net
                                      ST.return.yards.net = {
                                        our.ind <- which(str_detect(tolower(desc), " kicks | punts | punt is blocked | fake punt"))
                                        if (length(our.ind) > 0){
                                          sum(ifelse(Pos.Team.Points[our.ind] > 0,
                                                     yrdline100[our.ind],
                                                     ifelse(Pos.Team.Points[our.ind] < 0,
                                                            yrdline100[our.ind]-100,
                                                            pos_net[our.ind])))
                                        } else {
                                          0
                                          }},

                                      ## Checking first 3 entries (if there are even that many plays) for being a kick or punt that wasn't a NO PLAY
                                      ## Picking the LAST SUCH ENTRY, taking its ((yrdline + air.yardage)) as the starting position from the 
                                      # # In case of touchbacks though, redefine it to be next play's yrdline100
                                      
                                      
                                      off.ST.start.pos = {our.ind <- tail(which(str_detect(tolower(desc[1:(ifelse(length(desc) > 3, 3, length(desc)))]), " kicks | punts | punt is blocked | fake punt") & !str_detect(tolower(desc[1:(ifelse(length(desc) > 3, 3, length(desc)))]), "no play")), 1)
                                                          if (length(our.ind) > 0) {
                                                            if (!(str_detect(tolower(desc[our.ind]), "touchback"))){
                                                              yrdline100[our.ind] + ST.air.yardage[our.ind]
                                                            } else {
                                                              yrdline100[our.ind+1]
                                                            }
                                                          } else {
                                                            yrdline100[1]
                                                          }},
                                      
                                      # Make sure we don't have the "start within your own endzone" values
                                      off.ST.start.pos = ifelse(off.ST.start.pos > 100, 100, off.ST.start.pos),
                                      
       
                                      # off.ST.start.pos = ifelse(length(tail(which(str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), " kicks | punts | punt is blocked | fake punt") & !str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), "no play")), 1)) > 0,
                                      #                           100 - (yrdline100[tail(which(str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), " kicks | punts | punt is blocked | fake punt") & !str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), "no play")), 1)] + 
                                      #                                    ST.air.yardage[tail(which(str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), " kicks | punts | punt is blocked | fake punt") & !str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), "no play")), 1)]),
                                      #                           
                                      #                           yrdline100[1]),
                                      # 
                                      # # In case of touchbacks though, redefine it to be next play's yrdline100
                                      # off.ST.start.pos = ifelse(length(tail(which(str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), " kicks | punts | punt is blocked | fake punt") & !str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), "no play")), 1)) > 0,
                                      #                           ifelse(str_detect(
                                      #                             desc[tail(which(str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), " kicks | punts | punt is blocked | fake punt") & !str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), "no play")), 1)],
                                      #                             "touchback"), 
                                      #                             yrdline100[2],
                                      #                             off.ST.start.pos),
                                      #                           off.ST.start.pos),
                                                                # 
                                                                # 
                                                                # 100 - (yrdline100[tail(which(str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), " kicks | punts | punt is blocked | fake punt") & !str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), "no play")), 1)] + 
                                                                #          ST.air.yardage[tail(which(str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), " kicks | punts | punt is blocked | fake punt") & !str_detect(tolower(desc.vec[1:ifelse(length(desc.vec) > 3, 3, length(desc.vec))]), "no play")), 1)]),
                                                                # 
                                                                # yrdline100[1]),

                                        
                                      ## If it's Offensive TD: just do start.pos - 0 (because they went all the way)
                                      ## If it's defensive TD: do start.pos - 100  (as they went the other way, negative)
                                      ## If it's Made field goal: do start.pos - yrdline100 at the beginning of the last play (which is field goal attempt)
                                      ## If it's any other outcome: do start.pos - lead.yrdline100 (the latter is already from the standpoint of the previously possessing team)
                                      off.ST.yards.gained = off.ST.start.pos-ifelse(tail(Drive.Outcome,1) == "Offensive Touchdown",
                                                                                    0,
                                                                                    ifelse(tail(Drive.Outcome,1) %in% c("Defensive Touchdown", "Safety"),
                                                                                           100,
                                                                                           ifelse(tail(Drive.Outcome,1) == c("Made Field Goal"),
                                                                                                  tail(yrdline100, 1),
                                                                                                  tail(lead.yrdline100,1)))), 
                                                                                           # ifelse(tail(Drive.Outcome,1) == "Punt",
                                                                                           #        100-tail(lead.yrdline100,1),
                                                                                           #        tail(yrdline100,1)))),

                                      first.play.text = head(desc, 1),
                                      penultimate.play.text = ifelse(length(desc) > 1, desc[length(desc)-1], "It's a 1-play drive"),
                                      last.play.text = tail(desc, 1)
                            )
                          
                          )
  
  
  
  # summary(drive.by.drive)
  # play_by_play$yrdline100
  # play_by_play$pos_net
  # # play_by_play$pos_net[which(play_by_play$Drive != lead(play_by_play$Drive))]
  # play_by_play$pos_net[which(play_by_play$posteam != lead(play_by_play$posteam))]
  # 
  # play_by_play$yrdline100[which(play_by_play$posteam != lead(play_by_play$posteam))][1:6]
  # play_by_play$yrdline100[which(play_by_play$posteam != lead(play_by_play$posteam))+1][1:6]
  # 
  # 
  # table(play_by_play$PlayType[str_detect(tolower(play_by_play$desc), " kicks ")])
  # table(play_by_play$PlayType[str_detect(tolower(play_by_play$desc), " punts | punt |fake punt")])
  # 
  # # "punt" blocked, "fake punt"
  # play_by_play$desc[which(play_by_play$PlayType == "Punt" & !str_detect(tolower(play_by_play$desc), " punts "))]
  # 
  # play_by_play$desc[which(play_by_play$PlayType == "Kickoff" & !str_detect(tolower(play_by_play$desc), " kicks "))]
  # 
  # 
  # our.ind <- which(play_by_play$PlayType %in% c("Kickoff", "Punt") | 
  #   !str_detect(tolower(play_by_play$desc),"touchback") | 
  #   !str_detect(tolower(play_by_play$desc),"fair catch") | 
  #   !str_detect(tolower(play_by_play$desc),"penalty"))
  # 
  # play_by_play
  
  

  # ST.return.yards.net
  # off.ST.yards.gained
  
  drive.by.drive$off.yards_gained <- drive.by.drive$off.ST.yards.gained
  bad.drives <- drive.by.drive[is.na(drive.by.drive$off.yards_gained) , c("GameID", "Drive", "off.ST.start.pos")]
  
  # off.ST.start.pos-ifelse(head(Drive.Outcome,1) != "Offensive Touchdown",
  #                         ifelse(head(Drive.Outcome,1) == "Punt",
  #                                off.ST.start.pos-tail(as.numeric(unlist(str_extract_all(desc, "[+-]?\\d+"))),1), tail(yrdline100,1)),
  #                         0) +ST.return.yards.net,
  
  
  ##
  # print("# of NA's for ST.return.yards.net, off.ST.start.pos, off.ST.yards.gained")
  # print(sum(is.na(drive.by.drive$ST.return.yards.net)))
  # print(sum(is.na(drive.by.drive$off.ST.start.pos)))
  # print(sum(is.na(drive.by.drive$off.ST.yards.gained)))
  
  print("# of NA's for ST.return.yards.net, off.ST.start.pos, off.ST.yards.gained that are NOT before the end of half ")
  print(length(which(is.na(drive.by.drive$ST.return.yards.net) & 
              drive.by.drive$drive_half_start == lead(drive.by.drive$drive_half_start) & 
              drive.by.drive$GameID == lead(drive.by.drive$GameID))))
  print(length(which(is.na(drive.by.drive$off.ST.start.pos) & 
              drive.by.drive$drive_half_start == lead(drive.by.drive$drive_half_start) & 
              drive.by.drive$GameID == lead(drive.by.drive$GameID))))
  print(length(which(is.na(drive.by.drive$off.ST.yards.gained) & 
              drive.by.drive$drive_half_start == lead(drive.by.drive$drive_half_start) & 
              drive.by.drive$GameID == lead(drive.by.drive$GameID))))
  
  
  our.ind <- which(is.na(drive.by.drive$off.ST.yards.gained) & 
                     drive.by.drive$drive_half_start == lead(drive.by.drive$drive_half_start) & 
                     drive.by.drive$GameID == lead(drive.by.drive$GameID))
  
  # print("Last play types for NA's")
  # # print(table(drive.by.drive$last.play.text[is.na(drive.by.drive$ST.return.yards.net)]))
  # print(table(drive.by.drive$last.play.text[is.na(drive.by.drive$off.ST.start.pos)]))
  # print(table(drive.by.drive$last.play.text[is.na(drive.by.drive$off.ST.yards.gained)]))
  
  ## The NA's for "ST.return.yards.net" 
  ##  => It's all the LAST-DRIVE-IN-THE-HALF TYPE OF DRIVES, so it's FINE TO KEEP THOSE NA, as we WON'T USE THEM AS "LAGS" FOR THE NEXT DRIVE
  
  View(drive.by.drive[sort(c(our.ind, our.ind+1)),])
  
  # View(drive.by.drive %>%
  #        filter(is.na(ST.return.yards.net)))
  # 
  View(drive.by.drive %>%
         filter(is.na(off.ST.start.pos)))
  # 
  # View(drive.by.drive %>%
  #        filter(is.na(off.ST.yards.gained)))
  
  
  table(drive.by.drive$drive_Outcome)
  
  # punt blocked - incorrect net ST return yardage
  
  View(drive.by.drive %>%
    select(GameID,
           Drive,
           pos_team,
           def_pos_team,
           first.play.text,
           last.play.text,
           drive_Outcome,
           ST.return.yards.net,
           off.ST.start.pos,
           off.ST.yards.gained))
  
  print("Numerical summaries for ST.return.yards.net, off.ST.start.pos, off.ST.yards.gained")
  print(summary(drive.by.drive %>%
            select(ST.return.yards.net,
                   off.ST.start.pos,
                   off.ST.yards.gained)))
  
  # 
  # View(play_by_play %>%
  #        filter(GameID == "2009092100", Drive == 5))
  # 
  # 
  # 
  # i <- 1
  # View(play_by_play %>%
  #        filter(GameID == bad.drives$GameID[i]) %>%
  #        select(GameID,
  #               Drive,
  #               posteam,
  #               DefensiveTeam,
  #          #off.ST.start.pos,
  #               Drive.Outcome,
  #               yrdline100,
  #               # ST.return.yards.net,
  #               desc,
  #          PlayType))
  # # print(drive.by.drive$Drive.Outcome[i])
  # print(drive.by.drive$off.ST.start.pos[i])
  # print(drive.by.drive$ST.return.yards.net[i])
  
  
  
}


# There are some BAD GAMES:
# 
# GameID == "2009120612", Season == 2009
# GameID == "2009102508", Season == 2009
# GameID == "2010121913", Season == 2010
# GameID == "2011121102", Season == 2011
# GameID == "2016111000", Season == 2016

drive.by.drive <- drive.by.drive %>%
  filter(!(GameID == "2009120612" & Season == 2009),
         !(GameID == "2009102508" & Season == 2009),
         !(GameID == "2010121913" & Season == 2010),
         !(GameID == "2011121102" & Season == 2011),
         !(GameID == "2016111000" &  Season == 2016))




## Saving drive.by.drive in a CSV file


write_csv(drive.by.drive, "./NFL_2009_2017_Drive_by_Drive_Data.csv")

