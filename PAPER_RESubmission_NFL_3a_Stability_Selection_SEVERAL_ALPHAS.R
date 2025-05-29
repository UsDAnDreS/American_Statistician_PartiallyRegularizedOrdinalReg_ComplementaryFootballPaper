### For the paper.

#####
## Reproducing the selection stability figure from the paper,
## which calculates the proportion of times each complementary statistic got selected
## via lambda.1se 10-fold CV across all seasons and replicates.
#####


library(plotrix)
library(tidyverse)

alpha <- 0.8

n.lambdas <- 40
n.folds <- 10
n.CV.reps <- 3

year.range <- c(2009:2017)
# year.range <- year.range[!(year.range %in% c(2017))]

# We will be combining the yards for offense and special teams returns.
yds.combine <- TRUE


# The order in which the complementary statistics will appear on the plot
stat.order <- c("off.ST.yards.gained", "ST.return.yards.net",
                "pts.scored",
                "first.downs.gained", "third.downs.converted",
                "takeaway.nonscor", "turnover.nonscor", "punt.safety",
                "n.completions", "n.incompletions", "n.positive.runs", "n.stuffed.runs", "n.negative.plays", "n.sacks", "n.fumbles")

# if (NO_TAKEAWAYS){
#   stat.order <- stat.order[stat.order != "takeaway.nonscor"]
# }

## PROPORTIONAL ODDS PART:
lambda.1se.mat.linear <- NULL
for (year in year.range){
  # Loading the estimate and recording the sign and selection status for each statistic.
  # load(paste0("MULT_REP_NONPARALLEL_ORDNET/year=", as.character(year), "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
  # load(paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION_CLEANED/NFL_year=", as.character(year), ifelse(NO_TAKEAWAYS, "_NO_TAKEAWAYS",""), ifelse(stand, "_stand=TRUE", "_stand=FALSE"), "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
  
  load(paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION_CLEANED/NFL_year=", as.character(year), 
              "_alpha=", alpha,
              "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
  
  # load(paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION_CLEANED/NFL_year=", as.character(year),
  #             "_alpha=", alpha,
  #             "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_TUNE_OBJECTS.RData"))
  
  our.mat <- lambda.alpha.1se.est[[as.character(year)]][[as.character(alpha)]]
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
    # load(paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION_CLEANED/NFL_year=", as.character(year), ifelse(stand, "", "_stand=FALSE"), "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
   # load(paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION_CLEANED/NFL_year=", as.character(year), ifelse(NO_TAKEAWAYS, "_NO_TAKEAWAYS",""), ifelse(stand, "_stand=TRUE", "_stand=FALSE"), "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
    load(paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION_CLEANED/NFL_year=", as.character(year), 
                "_alpha=", alpha,
                "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
    
    our.mat <- lambda.alpha.1se.est[[as.character(year)]][[as.character(alpha)]]
    
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














########
## CV result comparisons
########


alpha <- 0.8

n.lambdas <- 40
n.folds <- 10
n.CV.reps <- 3

year.range <- c(2009:2017)
# year.range <- year.range[!(year.range %in% c(2017))]

# We will be combining the yards for offense and special teams returns.
yds.combine <- TRUE


# The order in which the complementary statistics will appear on the plot
stat.order <- c("off.ST.yards.gained", "ST.return.yards.net",
                "pts.scored",
                "first.downs.gained", "third.downs.converted",
                "takeaway.nonscor", "turnover.nonscor", "punt.safety",
                "n.completions", "n.incompletions", "n.positive.runs", "n.stuffed.runs", "n.negative.plays", "n.sacks", "n.fumbles")

# if (NO_TAKEAWAYS){
#   stat.order <- stat.order[stat.order != "takeaway.nonscor"]
# }

alpha.vec <-  c(0.8, 0.9, 0.95, 0.99, 0.999, 0.9999)
CV.mean.table <- lambda.1se.table <- matrix(0, nrow=length(year.range), ncol=length(alpha.vec))


## PROPORTIONAL ODDS PART:
for (year in year.range){
  print("Year:")
  print(year)
  # Loading the estimate and recording the sign and selection status for each statistic.
  # load(paste0("MULT_REP_NONPARALLEL_ORDNET/year=", as.character(year), "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
  # load(paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION_CLEANED/NFL_year=", as.character(year), ifelse(NO_TAKEAWAYS, "_NO_TAKEAWAYS",""), ifelse(stand, "_stand=TRUE", "_stand=FALSE"), "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_lambda.1se.est.RData"))
  
  for (alpha in alpha.vec){
    
  load(paste0("MULT_REP_NONPARALLEL_ORDNET/RESUBMISSION_CLEANED/NFL_year=", as.character(year), 
              "_alpha=", alpha,
              "_nlambdas=", n.lambdas, "_nfolds=", n.folds, "_nreps=", n.CV.reps, "_TUNE_OBJECTS.RData"))

  our.df <- data.frame(lambda = ordnet.alpha.obj.list[[1]][[1]][[1]]$lambdaVals,
                       m = apply(ordnet.alpha.obj.list[[1]][[1]][[1]]$loglik,1 , mean),
                       se = apply(ordnet.alpha.obj.list[[1]][[1]][[1]]$loglik,1 , function(x) sd(x)/sqrt(length(x))))
  our.df
  
  lambda.min.ind <- which.max(our.df$m)
  lambda.min <- our.df$lambda[lambda.min.ind]
  
  lambda.1se.ind <- which(ordnet.alpha.obj.list[[1]][[1]][[1]]$lambdaVals >= lambda.min & ((our.df$m + our.df$se) >= our.df$m[lambda.min.ind]));
  lambda.1se <- max(ordnet.alpha.obj.list[[1]][[1]][[1]]$lambdaVals[lambda.1se.ind])
  lambda.1se.ind <- which(ordnet.alpha.obj.list[[1]][[1]][[1]]$lambdaVals == lambda.1se)
  
  # print("alpha")
  # print(alpha)
  # print("lambda.1se value:")
  # print(lambda.1se)
  # print("Log-likelihood:")
  # print(our.df$m[lambda.1se.ind])
  # print("SE:")
  # print(our.df$se[lambda.1se.ind])
  
  CV.mean.table[which(year.range == year), which(alpha.vec == alpha)] <- paste0(round(our.df$m[lambda.1se.ind],3), " (", round(our.df$se[lambda.1se.ind], 1),")")
  lambda.1se.table[which(year.range == year), which(alpha.vec == alpha)] <- lambda.1se
  }
  
  }


print(CV.mean.table)
print(round(lambda.1se.table,4))




# 
# [1,] -480.986 (6.3) -480.986 (6.3) -480.931 (6.3) -480.931 (6.3) -480.917 (6.3) -480.917 (6.3)
# [2,] -490.619 (3.9) -490.619 (3.9) -490.562 (3.9) -490.562 (3.9) -490.547 (3.9) -490.547 (3.9)
# [3,] -497.867 (6.6) -497.867 (6.6) -497.782 (6.6) -497.782 (6.6) -497.759 (6.6) -497.759 (6.6)
# [4,] -517.371 (8.1) -517.371 (8.1) -517.313 (8)   -517.313 (8)   -517.299 (8)   -517.299 (8)  
# [5,] -529.266 (8.3) -529.266 (8.3) -529.239 (8.3) -529.239 (8.3) -529.232 (8.3) -529.232 (8.3)
# [6,] -508.007 (7.9) -508.007 (7.9) -508.005 (7.9) -508.005 (7.9) -508.005 (7.9) -508.005 (7.9)
# [7,] -522.597 (5.8) -522.597 (5.8) -522.576 (5.8) -522.576 (5.8) -522.57 (5.8)  -522.57 (5.8) 
# [8,] -502.864 (4.4) -502.864 (4.4) -502.825 (4.4) -502.825 (4.4) -502.814 (4.4) -502.814 (4.4)
# [9,] -509.777 (7.4) -509.777 (7.4) -509.773 (7.4) -509.773 (7.4) -509.772 (7.4) -509.772 (7.4)