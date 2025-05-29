######
## Cleaning up certain drive outcome inconsistencies in terms of 
## lack of correspondence between points and the drive outcome
## (after having performed the initial cleaning and formatting in Part 0p1)
##
## Saving the cleaned-up version into a csv via updating the previous file:
##    write_csv(pbp_by_drive, "./NFL_2009_2017_Drive_by_Drive_Data_Cleaned.csv")
######

#####
## Carrying out preliminary diagnostics via surrogate approach,
## creating the residuals-vs-fitted and QQ-plots,
## saving them in the "Preliminary_Residual_Plots.pdf" file.
#####

### Tried ADDING INTERACTION TERMS as well. High significance, but NO REAL IMPROVEMENT TO GOF tests

library(tidyverse)
library(car)
library(fuzzyjoin)
library(sure)
library(ordinal)
library(ggplot2)
library(ggfortify)
library(ggplotify)


# We will be combining the yards for offense and special teams returns.
yds.combine <- TRUE


pbp_by_drive <- read.csv("NFL_2009_2017_Drive_by_Drive_Data.csv")
  
all.outcomes <- unique(pbp_by_drive$drive_Outcome)
all.outcomes[1]



## End of Half
our.ind <- which(pbp_by_drive$drive_Outcome == "End of Half" & pbp_by_drive$pts.scored != 0)

table(pbp_by_drive$drive_Outcome, pbp_by_drive$pts.scored)

pbp_by_drive$drive_Outcome[our.ind[which(pbp_by_drive$pts.scored[our.ind] == 3)]] <- "Made Field Goal"
pbp_by_drive$pts.scored[our.ind[which(pbp_by_drive$pts.scored[our.ind] == 6)]] <- 0


## Fumble


##    -6 => all are legit touchdowns seemingly
our.ind <- which(pbp_by_drive$drive_Outcome == "Fumble" & pbp_by_drive$pts.scored == -6)
pbp_by_drive$drive_Outcome[our.ind] <- "Defensive Touchdown"

# Refurbish the yards gained: make it negative of how much was left to go the other way
pbp_by_drive$off.ST.yards.gained[our.ind] <- pbp_by_drive$off.ST.start.pos[our.ind] - 100


##    -2 => all are legit safeties seemingly
our.ind <- which(pbp_by_drive$drive_Outcome == "Fumble" & pbp_by_drive$pts.scored == -2)
pbp_by_drive$drive_Outcome[our.ind] <- "Safety"

# Refurbish the yards gained: make it negative of how much was left to go the other way
pbp_by_drive$off.ST.yards.gained[our.ind] <- pbp_by_drive$off.ST.start.pos[our.ind] - 100


##    6 => not sure why it got "+6", those are all clearly defensive TDs
our.ind <- which(pbp_by_drive$drive_Outcome == "Fumble" & pbp_by_drive$pts.scored == 6)
pbp_by_drive$drive_Outcome[our.ind] <- "Defensive Touchdown"
pbp_by_drive$pts.scored[our.ind] <- -6

# Refurbish the yards gained: make it negative of how much was left to go the other way
pbp_by_drive$off.ST.yards.gained[our.ind] <- pbp_by_drive$off.ST.start.pos[our.ind] - 100

## -4 => was just a safety (seemingly repeated entries, hence -2 + -2 = -4)
our.ind <- which(pbp_by_drive$drive_Outcome == "Fumble" & pbp_by_drive$pts.scored == -4)
pbp_by_drive$drive_Outcome[our.ind] <- "Safety"
pbp_by_drive$pts.scored[our.ind] <- -2

# Refurbish the yards gained: make it negative of how much was left to go the other way
pbp_by_drive$off.ST.yards.gained[our.ind] <- pbp_by_drive$off.ST.start.pos[our.ind] - 100



## 2 => just a fumble
# our.ind <- which(pbp_by_drive$drive_Outcome == "Fumble" & pbp_by_drive$pts.scored == 2)
# pbp_by_drive$drive_Outcome[our.ind] <- "Fumble"
# pbp_by_drive$pts.scored[our.ind] <- 0






###
## "Interception"

## 6 => interception on a 2pt play
our.ind <- which(pbp_by_drive$drive_Outcome == "Interception" & pbp_by_drive$pts.scored == 6)
pbp_by_drive$drive_Outcome[our.ind] <- "Offensive Touchdown"

# They gained everything needed from start pos (for a TD)
pbp_by_drive$off.ST.yards.gained[our.ind] <- pbp_by_drive$off.ST.start.pos[our.ind]


####
## "Missed Field Goal"

## 6 => Blocked FG returned for a TD
our.ind <- which(pbp_by_drive$drive_Outcome == "Missed Field Goal" & pbp_by_drive$pts.scored == 6)
pbp_by_drive$drive_Outcome[our.ind] <- "Defensive Touchdown"
pbp_by_drive$pts.scored[our.ind] <- -6

# Refurbish the yards gained: make it negative of how much was left to go the other way
pbp_by_drive$off.ST.yards.gained[our.ind] <- pbp_by_drive$off.ST.start.pos[our.ind] - 100




####
## "Offensive Touchdown"

## 0 => just plain touchdowns (maybe with some issues on 1pt/2pt conversions)
our.ind <- which(pbp_by_drive$drive_Outcome == "Offensive Touchdown" & pbp_by_drive$pts.scored == 0)
pbp_by_drive$pts.scored[our.ind] <- 7

## 1 => touchdowns AFTER RECOVERING THEIR OWN FUMBLE (with extra point converted)
our.ind <- which(pbp_by_drive$drive_Outcome == "Offensive Touchdown" & pbp_by_drive$pts.scored == 1)
pbp_by_drive$pts.scored[our.ind] <- 7



## 13 => just a bad double-entry
our.ind <- which(pbp_by_drive$drive_Outcome == "Offensive Touchdown" & pbp_by_drive$pts.scored == 13)
pbp_by_drive$pts.scored[our.ind] <- 7


## Punt

## -2: actually a safety, not a punt
our.ind <- which(pbp_by_drive$drive_Outcome == "Punt" & pbp_by_drive$pts.scored == -2)
pbp_by_drive$drive_Outcome[our.ind] <- "Safety"

# Refurbish the yards gained: make it negative of how much was left to go the other way
pbp_by_drive$off.ST.yards.gained[our.ind] <- pbp_by_drive$off.ST.start.pos[our.ind] - 100



## 3: just some little mess-up evidently.. clearly these end in a field goal 
our.ind <- which(pbp_by_drive$drive_Outcome == "Punt" & pbp_by_drive$pts.scored == 3)
pbp_by_drive$drive_Outcome[our.ind] <- "Made Field Goal"

# Refurbish the yards gained: account for field goal distance
pbp_by_drive$off.ST.yards.gained[our.ind] <- 
pbp_by_drive$off.ST.start.pos[our.ind] -
(sapply(pbp_by_drive$last.play.text[our.ind],
       function(x) as.numeric(unlist(str_extract_all(tolower(x), "-?\\d+(?= yard)"))[1])) -17)




## Turnover on Downs

## 6 - just a typo; there was no score
our.ind <- which(pbp_by_drive$drive_Outcome == "Turnover on Downs" & pbp_by_drive$pts.scored == 6)
pbp_by_drive$pts.scored[our.ind] <- 0



## Onside Kick Recovered / Fumble

## 6 => kickoff recovered in the receiving team's endzone for a TD, hence -6 
##     (as the receiving team is considered the offense)

our.ind <- which(pbp_by_drive$drive_Outcome == "Onside Kick Recovered / Fumble" & pbp_by_drive$pts.scored == 6)
pbp_by_drive$pts.scored[our.ind] <- -6
pbp_by_drive$drive_Outcome[our.ind] <- "Defensive Touchdown"

# 0 yards for gained by Off & ST in that scenario:
pbp_by_drive$off.ST.yards.gained[our.ind] <- 0
table(pbp_by_drive$drive_Outcome, pbp_by_drive$pts.scored)

View(pbp_by_drive[our.ind, ])



# View(pbp_by_drive[our.ind, ])
# 
# table(pbp_by_drive$drive_Outcome, pbp_by_drive$pts.scored)


## Unifying the notation for Jacksonville Jaguars 
## (in 2016 there were multiple notations used due to typo)
pbp_by_drive$pos_team <- ifelse(pbp_by_drive$pos_team == "JAX", 
                                "JAC",
                                pbp_by_drive$pos_team)
pbp_by_drive$def_pos_team <- ifelse(pbp_by_drive$def_pos_team == "JAX", 
                                    "JAC",
                                    pbp_by_drive$def_pos_team)
write_csv(pbp_by_drive, "./NFL_2009_2017_Drive_by_Drive_Data_Cleaned.csv")
  