### To be included in the paper.

#########
##  NFL
##
##  Calculating the 10-fold Cross-Validation objects,
##  subsequently reproducing the Calibration Plots inspired by Yurko et al's nflWAR paper.
##  Figure 6 in the supplementary materials corresponds to the plots for NFL 2009 season.
#########

# Yurko_nflWAR_Calibration.pdf
#
# The estimated probability for each of the
# seven scoring events is binned in five percent increments
# (20 total possible bins), with the observed proportion of
# the event found in each bin.



## Seed for reproducibility of CV
set.seed(1)

library(ordinalNet)

library(tidyverse)
library(car)
library(fuzzyjoin)
library(sure)
library(ordinal)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(grid)

# Loading the enhanced ordinalNet code.
source("Submission_0_ordinalNet_package_source_code_enhanced.R")


ordinalNetCV.mine <- function(x, y, lambdaVals=NULL, folds=NULL, nFolds=5, nFoldsCV=5,
                              tuneMethod=c("cvLoglik", "cvMisclass", "cvBrier", "cvDevPct",
                                           "aic", "bic"),
                              printProgress=TRUE, warn=TRUE, ...)
{
  tuneMethod <- match.arg(tuneMethod)
  # cvID := indicator to use cross validation within folds
  cvID <- tuneMethod %in% c("cvLoglik", "cvMisclass", "cvBrier", "cvDevPct")
  # tuneMethod := argument passed to ordinalNetTune.mine
  if (tuneMethod == "cvLoglik") cvCriterion <- "loglik"
  if (tuneMethod == "cvMisclass") cvCriterion <- "misclass"
  if (tuneMethod == "cvBrier") cvCriterion <- "brier"
  if (tuneMethod == "cvDevPct") cvCriterion <- "devPct"

  # Argument checks
  if (is.matrix(y) && any(rowSums(y)!=1))
    warning(paste0("Data is split by row for cross validation, but note that ",
                   "y matrix rows have different weights. Be sure this is what you want."))
  if (!is.null(folds) && length(folds)<2)
    stop(paste0("\'folds\' should be a list of at least two vectors. ",
                "Each vector should contain indices of a cross validation fold. ",
                "Each index from 1:nrow(x) should be used exactly once."))
  if (!is.null(folds) && !setequal(unlist(folds), 1:nrow(x)))
    stop("\'folds\' should include each index from 1:nrow(x) exactly once.")

  yMat <- if (is.matrix(y)) y else yFactorToMatrix(y)  # for computing log-likelihood
  if (printProgress) cat("Fitting ordinalNet on full training data\n")
  fit <- ordinalNet.mine(x, y, lambdaVals=lambdaVals, warn=warn, ...)
  if (is.null(lambdaVals)) lambdaVals <- fit$lambdaVals

  if (is.null(folds))
  {
    n <- nrow(x)
    randIndex <- sample(n)
    folds <- split(randIndex, rep(1:nFolds, length.out=n))
  } else
  {
    nFolds <- length(folds)
  }

  nLambda <- length(lambdaVals)
  loglik <- misclass <- brier <- devPct <- bestLambdaIndex <- MAE <- RMSE <- rep(NA, nFolds)
  names(loglik) <- names(misclass) <- names(brier) <- names(devPct) <-
    names(bestLambdaIndex) <- names(MAE) <- names(RMSE) <- paste0("fold", 1:nFolds)

  pHatFull.all <- list()

  for (i in 1:nFolds)
  {
    testFold <- folds[[i]]
    xTrain <- x[-testFold, , drop=FALSE]
    xTest <- x[testFold, , drop=FALSE]
    yTrain <- if (is.matrix(y)) y[-testFold, , drop=FALSE] else y[-testFold]
    yMatTest <- yMat[testFold, , drop=FALSE]
    if (printProgress) cat("Fitting ordinalNet on fold", i, "of", nFolds, '\n')

    if (cvID)
    {
      fitTrainCV <- ordinalNetTune.mine(xTrain, yTrain, lambdaVals=lambdaVals, folds=NULL,
                                        nFolds=5, printProgress=FALSE, warn=FALSE, ...)
      fitTrain <- fitTrainCV$fit
      if (cvCriterion %in% c("loglik", "devPct"))
        wm <- which.max
      if (cvCriterion %in% c("misclass", "brier"))
        wm <- which.min
      bestLambdaIndex[[i]] <- wm(rowMeans(fitTrainCV[[cvCriterion]]))
    } else  # tuneMethod is either "aic" or "bic"
    {
      fitTrain <- ordinalNet.mine(xTrain, yTrain, lambdaVals=lambdaVals, warn=FALSE, ...)
      bestLambdaIndex[[i]] <- which.min(fitTrain[[tuneMethod]])
    }

    pHatFull <- predict.ordinalNet.mine(fitTrain, newx=xTest, type="response", whichLambda=bestLambdaIndex[[i]])
    pHat <- pHatFull[, -ncol(pHatFull), drop=FALSE]
    # print(head(pHat))
    #   print(head(yMatTest))
    #  stop()
    loglik[i] <- getLoglik(pHat, yMatTest)
    misclass[i] <- getMisclass(pHat, yMatTest)
    brier[i] <- getBrier(pHat, yMatTest)
    loglikNull <- getLoglikNull(yMatTest)
    devPct[i] <- 1 - loglik[i] / loglikNull
    error.vec <- apply(pHat, 1, function(x) sum(c(x*c(-7,-2,0,3), (1-sum(x))*7))) -
      apply(yMatTest, 1, function(x) sum(c(x*c(-7,-2,0,3, 7))))

    MAE[i] <- mean(abs(error.vec))
    RMSE[i] <- sqrt(mean(error.vec^2))
    pHatFull.all[[i]] <- pHatFull

  }

  if (printProgress) cat("Done\n")

  out <- list(loglik=loglik, misclass=misclass, brier=brier, devPct=devPct, MAE=MAE, RMSE=RMSE,
              bestLambdaIndex=bestLambdaIndex, lambdaVals=lambdaVals, folds=folds, fit=fit, pHatFull.all = pHatFull.all)
  class(out) <- "ordinalNetCV"
  out
}






# We will only focus on the complementary statistics that were consistently picked
# (yards gained and non-scoring takeaways)
all.variables <- FALSE

# We will be combining the yards for offense and special teams returns.
yds.combine <- TRUE




## alpha trade-off value
alpha.val <- 0.99

## # of CV folds
n.folds <- 10

## We include the POS_TEAM/DEF_POS_TEAM/HOMEFIELD
incl.pos_team <- TRUE


projected.points <- NULL
projected.points.intercept <- NULL
ordnet.fits <- NULL


CV.results.with.complem <- CV.results.no.complem <- NULL

all.folds <- list()
all.pred <- list()


for (year in 2009:2017){

  print("Year:")
  print(year)


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
                                            1,
                                            0),
                                     1)
  pbp_by_drive$complem.ind[1] <- 0   # Cleaning up the "NA" generated by the first lag

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
                       "takeaways.nonscor", "turnovers.nonscor",
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
    pbp_by_drive_lag_vars[1, c.ind] <- pbp_by_drive_lag_vars[2, c.ind] # For the initial NA value
  }

  # Making the variable names for lagged values explicit (adding "lagged" to them)
  colnames(pbp_by_drive_lag_vars)[3:ncol(pbp_by_drive_lag_vars)] <- paste0("lagged_", colnames(pbp_by_drive_lag_vars)[3:ncol(pbp_by_drive_lag_vars)])

  # Merging the lagged values into original data set
  pbp_by_drive <- data.frame(pbp_by_drive, pbp_by_drive_lag_vars[,-1])



  ## If we combine yards, then DROP the "yds_ST_return"
  #  if (yds.combine) relev.var.names <- relev.var.names[relev.var.names != "yds_ST_return"]


  pbp_by_drive$lagged_start_pos_post_takeaway <- ifelse(pbp_by_drive$lagged_takeaways.nonscor == 1,
                                                        100-pbp_by_drive$off.ST.start.pos,
                                                        100-mean(pbp_by_drive$off.ST.start.pos[pbp_by_drive$lagged_takeaways.nonscor == 1]))

  pbp_by_drive$lagged_start_pos_post_turnover <- ifelse(pbp_by_drive$lagged_turnovers.nonscor == 1,
                                                        100-pbp_by_drive$off.ST.start.pos,
                                                        100-mean(pbp_by_drive$off.ST.start.pos[pbp_by_drive$lagged_turnovers.nonscor == 1]))



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
                                                   ifelse(pbp_by_drive$score_pts_by_text == 2,
                                                          NA,
                                                          ifelse(pbp_by_drive$score_pts_by_text %in% c(-6,-7,-8),
                                                                 "Defensive TD",
                                                                 "No Score")))))


  pbp_by_drive$categ.score <- factor(pbp_by_drive$categ.score,
                                     ordered=T,
                                     levels = c("Defensive TD", "Safety", "No Score", "Field Goal", "Offensive TD"))




  ###########
  ###########
  ### Calculating PROJECTED POINTS for EACH TEAM
  ###########
  ###########

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

  lm.obj <- lm(formula(paste0("as.numeric(categ.score) ~", paste0(c("pos_team", "def_pos_team", "pos.homefield", "complem.ind",
                                                                    paste0(c(relev.var.names.lagged[1], paste0(relev.var.names.lagged[1],":", relev.var.names.lagged[2])), ":complem.ind", sep="")
                                                                    , "score_diff_start", "drive_time_left_in_half", "drive_half_start"
                                                                    , "score_diff_start:drive_time_left_in_half", "score_diff_start:drive_half_start", "drive_time_left_in_half:drive_half_start"
  ),
  collapse = " + "), collapse =" ")),
  data=pbp_by_drive.noNA[, c(if(incl.pos_team) c("pos_team", "def_pos_team", "pos.homefield", "complem.ind",
                                                 "score_diff_start", "drive_time_left_in_half", "drive_half_start") else NULL,
                             relev.var.names.lagged,
                             "categ.score")])

  X <- model.matrix(lm.obj)[,-1]
  y <- pbp_by_drive.noNA$categ.score

  # Indices of the design matrix X to NOT penalize:
  ind.nonpen <- which(!str_detect(colnames(X), fixed("lagged")))
  penfact.vec <- rep(1, ncol(X))
  penfact.vec[ind.nonpen] <- 0


  #####
  ## Fitting the reduced selected model
  #####

  ordnet.obj <- ordinalNetCV.mine(x = as.matrix(X), y = y, alpha=alpha.val, standardize = T,
                                  nFolds=n.folds,
                                  penaltyFactors = penfact.vec,
                                  family="cumulative",
                                  parallelTerms = T,
                                  nonparallelTerms = T,
                                  nonparallel.categ = c(FALSE, FALSE, TRUE, FALSE),
                                  nonparallel.factor = str_detect(colnames(X), "lagged"),
                                  lambdaVals = 0
                                  #, printIter=TRUE
  )

  all.folds[[as.character(year)]] <- ordnet.obj$folds
  all.pred[[as.character(year)]] <- ordnet.obj$pHatFull.all

}

save(file="NFL_W_START_POS_INTERACTION_all.folds.Robj", all.folds)
save(file="NFL_W_START_POS_INTERACTION_all.pred.Robj", all.pred)



load(file="NFL_W_START_POS_INTERACTION_all.folds.Robj")
load(file="NFL_W_START_POS_INTERACTION_all.pred.Robj")


all.folds


for (year in 2009){

  print("Year:")
  print(year)


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
                                            1,
                                            0),
                                     1)
  pbp_by_drive$complem.ind[1] <- 0   # Cleaning up the "NA" generated by the first lag

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
                       # "takeaways.nonscor",
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
    pbp_by_drive_lag_vars[1, c.ind] <- pbp_by_drive_lag_vars[2, c.ind] # For the initial NA value
  }

  # Making the variable names for lagged values explicit (adding "lagged" to them)
  colnames(pbp_by_drive_lag_vars)[3:ncol(pbp_by_drive_lag_vars)] <- paste0("lagged_", colnames(pbp_by_drive_lag_vars)[3:ncol(pbp_by_drive_lag_vars)])

  # Merging the lagged values into original data set
  pbp_by_drive <- data.frame(pbp_by_drive, pbp_by_drive_lag_vars[,-1])



  ## If we combine yards, then DROP the "yds_ST_return"
  #  if (yds.combine) relev.var.names <- relev.var.names[relev.var.names != "yds_ST_return"]


    pbp_by_drive$lagged_start_pos_post_turnover <- ifelse(pbp_by_drive$lagged_turnovers.nonscor == 1,
                                                          100-pbp_by_drive$off.ST.start.pos,
                                                          100-mean(pbp_by_drive$off.ST.start.pos[pbp_by_drive$lagged_turnovers.nonscor == 1]))



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
                                                   ifelse(pbp_by_drive$score_pts_by_text == 2,
                                                          NA,
                                                          ifelse(pbp_by_drive$score_pts_by_text %in% c(-6,-7,-8),
                                                                 "Defensive TD",
                                                                 "No Score")))))


  pbp_by_drive$categ.score <- factor(pbp_by_drive$categ.score,
                                     ordered=T,
                                     levels = c("Defensive TD", "Safety", "No Score", "Field Goal", "Offensive TD"))




  ###########
  ###########
  ### Calculating PROJECTED POINTS for EACH TEAM
  ###########
  ###########

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


  y <- pbp_by_drive.noNA$categ.score
  pHat.All <- matrix(0, nrow=nrow(pbp_by_drive.noNA), ncol=5)

  for (j in 1:length(all.folds[[as.character(year)]])){
    pHat.All[all.folds[[as.character(year)]][[j]], ] <- all.pred[[as.character(year)]][[j]]
  }

  y
  pHat.All

  all.outcomes <- levels(y)
  plot.list <- list()

  for (l in 1:length(all.outcomes)){
    bin.vec <- cut(pHat.All[,l], seq(0,1, by=0.05))
    prop.response <- tapply(y == all.outcomes[l], bin.vec, mean)
    # plot(prop.response ~seq(0.05,1, by=0.05), ylim=c(0,1),
    #      cex = 3*(table(bin.vec)/sum(table(bin.vec))))
    # abline(a=0,b=1)

    plot.list[[l]] <- ggplot(data=data.frame(x = seq(0.05,1, by=0.05), y = prop.response, z=as.numeric(table(bin.vec))),
                             aes(x=x,y=y, size=3*(z/sum(z)))) +
      geom_point() +
      # geom_smooth(method="loess") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      ylim(0,1) +
      theme(legend.position = "none") +
      ggtitle(all.outcomes[l]) +
      xlab("Estimated probability") +
      ylab("Observed probability")
  }


  legend_plot <- ggplot(data=data.frame(x = seq(0.05,1, by=0.05),
                                        y = prop.response,
                                        z =as.numeric(table(bin.vec)),
                                        Year = year),
                        aes(x=x,y=y, size=z, color=Year))+
    geom_point() +  # Add points to generate the legend
    theme_void() +  # Remove all plot elements
    theme(legend.position = "right") + # Position the legend
    #labs(size = "Number of drives") +
    scale_size_continuous(
      breaks = c(100, 500, 1000),  # Specify fewer breaks for the legend
      name = "Number of drives"
    ) +
    # scale_color_manual(values = c("black")) +
    scale_color_gradient2(
      low = "black",       # Low values color
      mid = "black",      # Midpoint color
      high = "black") +
    guides(color = guide_colorbar(barwidth = 0.5, barheight = 0.5))


  ## Width: 784
  ## Height: 462
  grid.arrange(plot.list[[1]], plot.list[[2]], plot.list[[3]], plot.list[[4]], plot.list[[5]], legend_plot,
               nrow=2, ncol=3,
               top = textGrob("10-fold CV Calibration Across Scoring Categories, NFL 2009 season", gp = gpar(fontsize = 16)))

}