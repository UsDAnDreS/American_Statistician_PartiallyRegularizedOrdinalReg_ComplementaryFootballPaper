## To include in the paper

####
## NFL
##
## For each season (2009-2017), carrying out 3 replicates of 10-fold cross-validation.
## Recording the resulting cross-validation objects into "TUNE_OBJECTS.RData" files,
## and the picked "lambda.1se" estimates into "lambda.1se.est.RData" files.
##
## Saves the results as objects to be later used in Stage 3 for stability selection plots:
##  save(ordnet.obj.list, file= paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION/NFL_W_DURATION_year=", as.character(year), ifelse(NO_TAKEAWAYS, "_NO_TAKEAWAYS",""), "_stand=", stand,  "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_TUNE_OBJECTS.RData"))
##  save(lambda.1se.est, file= paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION/NFL_W_DURATION_year=", as.character(year), ifelse(NO_TAKEAWAYS, "_NO_TAKEAWAYS",""), "_stand=", stand, "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
##
## (NOTE: Takes a LOOOOOOONG time. Best to do year-by-year in parallel.)
####


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


## WHETHER TO EXCLUDE TAKEAWAYS and just have the TURNOVERS
NO_TAKEAWAYS <- TRUE


## Whether to use standardization in Lasso (TRUE), or to do it before Lasso (FALSE) instead 
## (the latter is needed to retain categorical variables & interactions' meaning)
stand <- TRUE


# We will be combining the yards for offense and special teams returns.
yds.combine <- TRUE


# Creating the folder to contain the resulting fits/estimates.
dir.create("MULT_REP_NONPARALLEL_ORDNET")


n.lambdas <- 40
n.folds <- 10
n.CV.reps <- 3

## Whether we include POS_TEAM/DEF_POS_TEAM/HOMEFIELD
incl.pos_team <- TRUE

## For the selections
lambda.min.est <- lambda.1se.est <- BIC.est <- list()

## For the # of folds that yielded negative probabilities
bad.folds <- list()

# List of calculated ordnet objects
ordnet.obj.list <- list()


for (year in 2009:2017){
  
  
  print("Year:")
  print(year)
  
  cat("\n")
  if (NO_TAKEAWAYS){
    cat("\n")
    print("NO TAKEAWAYS")
  } 
  
  print("stand")
  print(stand)
  
  
  pbp_by_drive <- read.csv("NFL_2009_2017_Drive_by_Drive_Data_Cleaned.csv") %>%
    filter(Season == year)
  
  
  ## Defining DURATION of a drive as: next drive's start time - this drive's start time
  our.ind <- which(pbp_by_drive$GameID == lead(pbp_by_drive$GameID) &
                     pbp_by_drive$drive_half_start == lead(pbp_by_drive$drive_half_start))
  pbp_by_drive$drive_duration_seconds <- NA
  pbp_by_drive$drive_duration_seconds[our.ind] <- pbp_by_drive$drive_TimeSecs_start[our.ind] - lead(pbp_by_drive$drive_TimeSecs_start)[our.ind]
  
  # Make the negatives and >1000 into NA's (those are clearly data entry errors)
  pbp_by_drive$drive_duration_seconds[pbp_by_drive$drive_duration_seconds < 0] <- NA
  pbp_by_drive$drive_duration_seconds[pbp_by_drive$drive_duration_seconds > 1000] <- NA
  
  # print(summary(pbp_by_drive$drive_duration_seconds))
  
  #our.ind <- which(pbp_by_drive$drive_duration_seconds > 1200)
  # View(pbp_by_drive[sort(c(our.ind, our.ind+1)),])
  # print(head(sort(pbp_by_drive$drive_duration_seconds, dec=T)))
  
  
  
  
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
                       "takeaways.nonscor", 
                       "turnovers.nonscor",
                       "punt.safety",
                       "drive_duration_seconds"
  )
  
  if (NO_TAKEAWAYS) relev.var.names <- relev.var.names[relev.var.names != "takeaways.nonscor"]
  
  
  
  # Initializing a lagged variable set
  pbp_by_drive_lag_vars <- data.frame(game_id_half = pbp_by_drive$game_id_half,
                                      pos_team = pbp_by_drive$pos_team,
                                      # pbp_by_drive[, 8:ncol(pbp_by_drive)]
                                      pbp_by_drive[, relev.var.names]
  )
  
  
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
  
  
  #######
  ## Complementary statistics to be included in the model for testing:
  #######
  
  relev.var.names.lagged <- paste0("lagged_", relev.var.names)
  
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
  
  
  
  ## Time left in half
  # "drive_TimeSecs_start" - doesn't look good, a bunch of negative values, etc..
  # Just using the "drive_time_start", which looks way more reliable
  
  pbp_by_drive$drive_time_left_in_half <-  as.numeric(substr(pbp_by_drive$drive_time_start, 1, 2)) +
    as.numeric(substr(pbp_by_drive$drive_time_start, 4, 5))/60 +
    ifelse(pbp_by_drive$drive_qtr_start %in% c(1,3),
           15,
           0)
  
  
  ## Convert the half from having 3 categories ("1st", "2nd", "OT") 
  ##  into having just 2: ("1st", "2nd/OT")
  ## which makes sense from the end-goal perspective: 
  ##  we draw a difference between the mere end of 1st half, and 
  ##  the actual end of games (which is end of 2nd half, or OT) where the score is infinitely more important
  
  
  pbp_by_drive$drive_half_start <- ifelse(pbp_by_drive$drive_half_start %in% c("2nd", "OT"),
                                          "2nd/OT",
                                          pbp_by_drive$drive_half_start)
  
  
  # pbp_by_drive$drive_time_left_in_half <- pbp_by_drive$drive_time_minutes_start + 
  #   pbp_by_drive$drive_time_seconds_start/60 +
  #   ifelse(pbp_by_drive$drive_qtr_start %in% c(1,3),
  #          15,
  #          0)
  
  
  
  
  ## Game ID & Score differential - need to just rename the variables
  pbp_by_drive <- pbp_by_drive %>% 
    rename(game_id = GameID,
           score_diff_start = drive_ScoreDiff_start)
  
  
  
  
  pbp_by_drive.noNA <- na.omit(pbp_by_drive[, c("categ.score", "pos_team", "def_pos_team", relev.var.names.lagged, "game_id", "game_id_half", "pos.homefield", "complem.ind",
                                                "score_diff_start", "drive_time_left_in_half", "drive_half_start")] %>%
                                 mutate(pos_team = factor(pos_team),
                                        def_pos_team = factor(def_pos_team),
                                        game_id = factor(game_id),
                                        game_id_half = factor(game_id_half)))
  
  ## To avoid issue with DROPPED LEVELS (NEW HERE):
  pbp_by_drive.noNA$pos_team <- droplevels(pbp_by_drive.noNA$pos_team)
  pbp_by_drive.noNA$def_pos_team <- droplevels(pbp_by_drive.noNA$def_pos_team)
  
  ## Setting up the contrasts so that the intercept represented a "league-average opponent"
  ## (sum_i alpha_i = sum_j beta_j = 0)
  contrasts(pbp_by_drive.noNA$pos_team) <- contr.sum(nlevels(pbp_by_drive.noNA$pos_team))
  contrasts(pbp_by_drive.noNA$def_pos_team) <- contr.sum(nlevels(pbp_by_drive.noNA$def_pos_team))
  colnames(contrasts(pbp_by_drive.noNA$pos_team)) <- rownames(contrasts(pbp_by_drive.noNA$pos_team))[-nrow(contrasts(pbp_by_drive.noNA$pos_team))]
  colnames(contrasts(pbp_by_drive.noNA$def_pos_team)) <- rownames(contrasts(pbp_by_drive.noNA$def_pos_team))[-nrow(contrasts(pbp_by_drive.noNA$def_pos_team))]
  
  
  our.df <- pbp_by_drive.noNA[, c(if(incl.pos_team) c("pos_team", "def_pos_team", "pos.homefield", "complem.ind", 
                                                      "score_diff_start", "drive_time_left_in_half", "drive_half_start") else NULL,
                                  relev.var.names.lagged,
                                  "categ.score")]
  
  # sapply(our.df[, c("complem.ind", relev.var.names.lagged)], mean)
  # sapply(our.df[, c("complem.ind", relev.var.names.lagged)], sd)
  # 
  # sapply(our.df <- our.df %>%
  #          mutate_if(is.numeric, scale) %>% .[c("complem.ind", relev.var.names.lagged)], mean)
  # sapply(our.df <- our.df %>%
  #          mutate_if(is.numeric, scale) %>% .[c("complem.ind", relev.var.names.lagged)], sd)
  
  
  if (stand == FALSE) {
    our.df <- our.df %>%
      mutate_if(is.numeric, scale) 
  }
  
  our.df$complem.ind <- as.numeric(our.df$complem.ind)
  
  
  ## First: basic lm fit, just to get the design matrix X & response y
  lm.obj <- lm(formula(paste0("as.numeric(categ.score) ~", paste0(c("pos_team", "def_pos_team", "pos.homefield", "complem.ind", paste0(relev.var.names.lagged, ":complem.ind", sep="")
                                                                    , "score_diff_start", "drive_time_left_in_half", "drive_half_start"
                                                                    , "score_diff_start:drive_time_left_in_half", "score_diff_start:drive_half_start", "drive_time_left_in_half:drive_half_start"
  ),
  collapse = " + "), collapse =" ")),
  data=our.df)
  
  X <- model.matrix(lm.obj)[,-1]
  y <- pbp_by_drive.noNA$categ.score
  
  # Indices of the design matrix X to NOT penalize:
  ind.nonpen <- which(!str_detect(colnames(X), fixed("lagged")))
  penfact.vec <- rep(1, ncol(X))
  penfact.vec[ind.nonpen] <- 0
  
  
  ########
  ## For CROSS-VALIDATION: Setting the seed, and RE-RUNNING THE K-FOLD CV SEVERAL TIMES
  ########
  
  
  set.seed(1)
  
  for (j in 1:n.CV.reps){
    print("CV REPLICATE #:")
    print(j)
    
    ######
    ## THE MAIN FITTING FUNCTION
    ######
    
    ordnet.obj <- ordinalNetTune.mine(x = as.matrix(X), y = y, alpha=0.99, standardize = stand, 
                                      penaltyFactors = penfact.vec, 
                                      family="cumulative",
                                      parallelTerms = T,
                                      nonparallelTerms = T,
                                      nonparallel.factor = str_detect(colnames(X), "lagged"),
                                      nFolds = n.folds,
                                      nLambda= n.lambdas,
                                      printProgress=TRUE,
                                      warn=FALSE
    )
    
    
    ### Saving the full CV object.
    ordnet.obj.list[[as.character(year)]] <- append(ordnet.obj.list[[as.character(year)]], list(ordnet.obj))
    
    ### Saving the lambda.1se estimate
    mean.CVs <- apply(ordnet.obj$loglik, 1, function(x) mean(x[x != -Inf]))
    se.CVs <- apply(ordnet.obj$loglik, 1, function(x) sd(x[x != -Inf]))/sqrt(n.folds)
    
    ## Calculating the lambda.1se estimate
    lambda.min.ind <- which.max(mean.CVs); 
    lambda.min <- ordnet.obj$lambdaVals[lambda.min.ind]
    lambda.1se.ind <- which(ordnet.obj$lambdaVals >= lambda.min & ((mean.CVs + se.CVs) >= mean.CVs[lambda.min.ind]));
    lambda.1se <- max(ordnet.obj$lambdaVals[lambda.1se.ind])
    lambda.1se.ind <- which(ordnet.obj$lambdaVals == lambda.1se)
    
    lambda.1se.est[[as.character(year)]] <- rbind(lambda.1se.est[[as.character(year)]],
                                                  ordnet.obj$fit$coefs[lambda.1se.ind, which(str_detect(colnames(ordnet.obj$fit$coefs), fixed("lagged")))])
    
    
  }
  
  
  save(ordnet.obj.list, file= paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION/NFL_W_DURATION_year=", as.character(year), ifelse(NO_TAKEAWAYS, "_NO_TAKEAWAYS",""), "_stand=", stand,  "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_TUNE_OBJECTS.RData"))
  save(lambda.1se.est, file= paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION/NFL_W_DURATION_year=", as.character(year), ifelse(NO_TAKEAWAYS, "_NO_TAKEAWAYS",""), "_stand=", stand, "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
  
}

