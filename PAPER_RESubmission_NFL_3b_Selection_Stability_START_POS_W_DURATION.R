## To be included in the paper

#####
## NFL
##
## Reproducing the selection stability Figure 6(a) from the main paper (in the appendix),
## which calculates the proportion of times each complementary statistic got selected
## via lambda.1se 10-fold CV across all NFL seasons and replicates,
## (!) but after having included the post-turnover starting position (!).
#####


library(plotrix)
library(tidyverse)

## Whether to use standardization in Lasso (TRUE), or to do it before Lasso (FALSE) instead 
## (the latter is needed to retain categorical variables & interactions' meaning)
stand <- TRUE

# type <- NULL
type <- c("ONLY_TURNOVERS","ONLY_TAKEAWAYS")[1]


n.lambdas <- 40
n.folds <- 10
n.CV.reps <- 3

year.range <- c(2009:2017)
# year.range <- year.range[!(year.range %in% c(2013))]

# We will be combining the yards for offense and special teams returns.
yds.combine <- TRUE


# The order in which the complementary statistics will appear on the plot
stat.order <- c("off.ST.yards.gained", "ST.return.yards.net",
                "pts.scored",
                "first.downs.gained", "third.downs.converted",
                # "takeaway.nonscor", 
                "takeaway.nonscor:start.pos.post.takeaway",
                # "turnover.nonscor", 
                "TO.nonscor:start.pos",
                "punt.safety",
                "n.completions", "n.incompletions", "n.positive.runs", "n.stuffed.runs", "n.negative.plays", "n.sacks", "n.fumbles",
                "duration")

if (length(type) > 0){
  if (type == "ONLY_TURNOVERS"){
    stat.order <- stat.order[!(stat.order %in% c("takeaway.nonscor", "takeaway.nonscor:start.pos.post.takeaway"))]
  } else if (type == "ONLY_TAKEAWAYS"){
    stat.order <- stat.order[!(stat.order %in% c("turnover.nonscor", "turnover.nonscor:start.pos.post.turnover"))]
  }
}


## PROPORTIONAL ODDS PART:
lambda.1se.mat.linear <- NULL
for (year in year.range){
  # Loading the estimate and recording the sign and selection status for each statistic.
  # load(paste0("MULT_REP_NONPARALLEL_ORDNET/year=", as.character(year), "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
  # load(paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION_CLEANED/NFL_year=", as.character(year), "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
  
  load(paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION_CLEANED/NFL_START_POS_INTERACTION_W_DURATION_year=", as.character(year), ifelse(length(type) >0, paste0("_type=",type), ""), "_stand=", stand, "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
  
  our.mat <- lambda.1se.est[[as.character(year)]]
  lambda.1se.mat.linear <- cbind(lambda.1se.mat.linear,apply(sign(our.mat[, !str_detect(colnames(our.mat), ":[0-9]")]),
                                                             2,
                                                             mean))
}
colnames(lambda.1se.mat.linear) <- year.range

lambda.1se.mat.linear



## NON-PROPORTIONAL PART:
lambda.1se.mat.nonlinear <-  NULL
n.levels <- 5

# For each cumulative odds category
for (j in 1:(n.levels-1)){
  lambda.1se.mat.nonlinear[[j]] <- list(NULL)
  
  for (year in year.range){
    # Loading the estimate and recording the sign and selection status for each statistic.
    # load(paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION_CLEANED/NFL_year=", as.character(year), "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
    load(paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION_CLEANED/NFL_START_POS_INTERACTION_W_DURATION_year=", as.character(year), ifelse(length(type) >0, paste0("_type=",type), ""), "_stand=", stand, "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
    
    our.mat <- lambda.1se.est[[as.character(year)]]
    if (year == 2009){
      lambda.1se.mat.nonlinear[[j]] <- apply(sign(our.mat[, str_detect(colnames(our.mat), fixed(paste0(":", j)))]),
                                             2,
                                             mean)
      
    } else {
      lambda.1se.mat.nonlinear[[j]] <- cbind(lambda.1se.mat.nonlinear[[j]], 
                                             apply(sign(our.mat[, str_detect(colnames(our.mat), fixed(paste0(":", j)))]),
                                                   2,
                                                   mean))
    }
  }
  colnames(lambda.1se.mat.nonlinear[[j]]) <- year.range
}



####
## Combining the proportional and non-proportional parts into one condensed, 5-column, matrix, where
## each value would represent a proportion of selections across all years and replicates 
## for this particular coefficient of that particular complementary statistic
####

lambda.1se.condensed.mat <- as.matrix(apply(lambda.1se.mat.linear, 1, mean))
for (j in 1:(n.levels-1)){
  lambda.1se.condensed.mat <- cbind(lambda.1se.condensed.mat, 
                                    apply(lambda.1se.mat.nonlinear[[j]],1,mean))
}


# Cleaning up all the names for prettier look.

categ.names <- c("\u2265 Safety", "\u2265 No Score", "\u2265 FG", "\u2265 Off. TD")
colnames(lambda.1se.condensed.mat) <- c("All Categ.", categ.names)
rownames(lambda.1se.condensed.mat) <- str_remove(rownames(lambda.1se.condensed.mat), fixed("lagged_"))
rownames(lambda.1se.condensed.mat) <- str_remove(rownames(lambda.1se.condensed.mat), fixed("TRUE"))
rownames(lambda.1se.condensed.mat) <- str_remove(rownames(lambda.1se.condensed.mat), fixed(".with.penalties"))
rownames(lambda.1se.condensed.mat) <- str_replace(rownames(lambda.1se.condensed.mat), fixed("off."), fixed("off.ST."))
rownames(lambda.1se.condensed.mat) <- str_replace_all(rownames(lambda.1se.condensed.mat), fixed("_"), fixed("."))
rownames(lambda.1se.condensed.mat) <- str_remove(rownames(lambda.1se.condensed.mat), fixed(".by.text"))
rownames(lambda.1se.condensed.mat) <- str_replace(rownames(lambda.1se.condensed.mat), fixed("score.pts"), fixed("pts.scored"))
rownames(lambda.1se.condensed.mat) <- str_replace(rownames(lambda.1se.condensed.mat), fixed("takeaways"), fixed("takeaway"))
rownames(lambda.1se.condensed.mat) <- str_replace(rownames(lambda.1se.condensed.mat), fixed("turnovers"), fixed("turnover"))
rownames(lambda.1se.condensed.mat) <- str_replace(rownames(lambda.1se.condensed.mat), fixed("yds.ST.return.net"), fixed("ST.return.yards.net"))
rownames(lambda.1se.condensed.mat) <- str_remove(rownames(lambda.1se.condensed.mat), fixed("complem.ind:"))
rownames(lambda.1se.condensed.mat) <- str_remove(rownames(lambda.1se.condensed.mat), fixed("lagged."))

rownames(lambda.1se.condensed.mat) <- str_replace(rownames(lambda.1se.condensed.mat), fixed("drive.duration.seconds"), fixed("duration"))
 
rownames(lambda.1se.condensed.mat)[rownames(lambda.1se.condensed.mat) == "turnover.nonscor:start.pos.post.turnover"] <- "TO.nonscor:start.pos"




# Reshuffling the statistics to show up in a pre-specified order
ind.reshuffle <- sapply(stat.order,
                        function(x) which(rownames(lambda.1se.condensed.mat) == x))

lambda.1se.condensed.mat <- lambda.1se.condensed.mat[ind.reshuffle,]



#######
## The actual plot
#######

## width: 530; height: 424
par(mar = c(2.1, 10, 4.0, 2.1))

color2D.matplot(ifelse(lambda.1se.condensed.mat*(-1) == 0, 0, lambda.1se.condensed.mat*(-1)),
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=1,
                xlab='',
                ylab='',
                axes=F)
par(las=2)
staxlab(3,at=c(1:ncol(lambda.1se.condensed.mat))-0.25,labels=colnames(lambda.1se.condensed.mat),srt=-45)
par(las=1)
axis(2,at=c(nrow(lambda.1se.condensed.mat):1)-0.5,labels=rownames(lambda.1se.condensed.mat))

par(mar = c(5.1, 4.1, 4.1, 2.1))
