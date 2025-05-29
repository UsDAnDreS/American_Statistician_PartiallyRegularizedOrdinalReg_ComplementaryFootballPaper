#####
## NFL
##
## Calculating the objects to be eventually fed to "PAPER_RESubmission_6p5_BOTH_Plotting_Effects_of_Takeaways_On_Exp_Points.R"
## to produce the Figure 3(a) plot of the nature of the effect of turnovers on scoring.
#####


library(tidyverse)
library(gridExtra)
library(fuzzyjoin)
library(plotrix)

## Side of the ball that was getting balanced
side <- "Offense"

##
balanced <- FALSE

# We will only focus on the complementary statistics that were consistently picked
# (yards gained and non-scoring takeaways)
all.variables <- FALSE

# We will be combining the yards for offense and special teams returns.
yds.combine <- TRUE

## Loading all the relevant objects
load("RESUBMISSION_NFL_ORDNET_projected.points.intercept.RData")
load("RESUBMISSION_NFL_ORDNET_ordnet.fits.RData")


my.df.NFL <- NULL

for (year in 2009:2017){

  print(year)
  if (balanced){
    load(paste0("Bootstrap/NEW_RESUBMISSION_NFL_W_START_POS_INTERACTION_BALANCED_", side, "_LEAVE_CONTEXT_AS_IS_BOOTSTRAP_", year, "_ORDNET_projected.points.intercept.RData"))
  } else {
    load(paste0("Bootstrap/NEW_RESUBMISSION_NFL_W_START_POS_INTERACTION_NOT_BALANCED_LEAVE_CONTEXT_AS_IS_BOOTSTRAP_", year, "_ORDNET_projected.points.intercept.RData"))

  }


   interm.df <- NULL


   for (b in 1:length(projected.points.intercept[[as.character(year)]])){
    print(b)
    interm.df  <- rbind(interm.df,
                        data.frame(Year = year,
                                   projected.points.intercept[[as.character(year)]][[b]],
                                   RANK.Offense.Avg.Points.With.Complem.But.As.Is = rank(-projected.points.intercept[[as.character(year)]][[b]]$Offense.Avg.Points.With.Complem.But.As.Is),
                                   RANK.Offense.Avg.Points.Avg.Takeaways = rank(-projected.points.intercept[[as.character(year)]][[b]]$Offense.Avg.Points.Avg.Takeaways)))
  }

   my.df.NFL <- rbind(my.df.NFL,
                      interm.df)


}


if (balanced){
save(file=paste0("NEW_NEW_W_START_POS_INTERACTION_my.df.NFL.balanced.",side,".Robj"),
     my.df.NFL)
} else {
  save(file=paste0("NEW_NEW_W_START_POS_INTERACTION_my.df.NFL.not.balanced.Robj"),
       my.df.NFL)
}


# load(paste0("NEW_W_START_POS_INTERACTION_my.df.NFL.not.balanced.Robj"))