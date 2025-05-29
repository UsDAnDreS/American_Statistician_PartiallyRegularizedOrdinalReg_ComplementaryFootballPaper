#####
## CFB:
##
## Calculating the objects to be eventually fed to "PAPER_RESubmission_6p5_BOTH_Plotting_Effects_of_Takeaways_On_Exp_Points.R"
## to produce the Figure 3(a) plot of the nature of the effect of turnovers on scoring.
#####


library(tidyverse)
library(gridExtra)
library(fuzzyjoin)
library(plotrix)


balanced <- FALSE

# We will only focus on the complementary statistics that were consistently picked
# (yards gained and non-scoring takeaways)
all.variables <- FALSE

# We will be combining the yards for offense and special teams returns.
yds.combine <- TRUE

## Loading all the relevant objects
load("RESUBMISSION_CFB_ORDNET_projected.points.intercept.RData")
load("RESUBMISSION_CFB_ORDNET_ordnet.fits.RData")


######
##  Putting together a full data set of the expected points per drive projected onto a league-average complementary unit
##  for each team across all seasons
######

my.df <- NULL

for (year in 2014:2020){
  
  my.df <- rbind(my.df,
                 data.frame(Year = year, projected.points.intercept[[as.character(year)]]))
  
}

my.df$Year <- as.factor(my.df$Year)


####
# THE MAIN PLOT
####

## Width: 553; Height: 444
print(ggplot(data = my.df %>% mutate(Extra = Offense.Avg.Points.With.Takeaway - Offense.Avg.Points.No.Takeaway),
             aes(x=Offense.Avg.Points.No.Takeaway, y=Extra, col=Year)) +
        xlab("Points Scored Without A Takeaway (Baseline)") +
        ylab("Extra Points Scored After Takeaway") + 
        ggtitle("Offense"))


ggplot(data = my.df %>% mutate(Extra = Offense.Avg.Points.With.Takeaway - Offense.Avg.Points.No.Takeaway) %>%
         filter(Year == 2014),
       aes(x=Offense.Avg.Points.No.Takeaway, y=Extra)) +
  # geom_smooth(linetype=1) + 
  geom_point() +
  geom_smooth(method="loess", level=0.99, se=TRUE, span=50) +
  xlab("Points Scored Without A Takeaway (Baseline)") +
  ylab("Extra Points Scored After Takeaway") + 
  ggtitle("Offense")


################
################
#######
#######
# !!! CREATING THE BOOTSTRAP DATA FRAME !!!
#######
#######
################
################


#######
## POTENTIALLY MIGHT WANNA DO BOOTSTRAP (as CONFIDENCE INTERVALS MIGHT BE TOUGH FOR LASSO)
##    * 1000 replicates for each year
##    * Do it on the server
#######

my.df <- NULL

for (year in 2014:2020){
  
  print(year)
  if (balanced){
      load(paste0("Bootstrap/NEW_RESUBMISSION_CFB_W_START_POS_INTERACTION_BALANCED_", side, "_LEAVE_CONTEXT_AS_IS_BOOTSTRAP_", year, "_ORDNET_projected.points.intercept.RData"))
  } else {
    load(paste0("Bootstrap/NEW_RESUBMISSION_CFB_W_START_POS_INTERACTION_NOT_BALANCED_LEAVE_CONTEXT_AS_IS_BOOTSTRAP_", year, "_ORDNET_projected.points.intercept.RData"))
    
  }
  
  #   Extra = Offense.Avg.Points.With.Takeaway - Offense.Avg.Points.No.Takeaway),
  # aes(x=Offense.Avg.Points.No.Takeaway
  
  interm.df <- NULL
  
  for (b in 1:length(projected.points.intercept[[as.character(year)]])){
    print(b)
    interm.df  <- rbind(interm.df,
                        data.frame(Year = year, 
                                   projected.points.intercept[[as.character(year)]][[b]]))
  }
  
  my.df <- rbind(my.df,
                     interm.df)
  
  
}


if (balanced){
  save(file=paste0("NEW_W_START_POS_INTERACTION_my.df.balanced.",side,".Robj"),
       my.df)
} else {
  save(file=paste0("NEW_W_START_POS_INTERACTION_my.df.not.balanced.Robj"),
       my.df)
}


# load(paste0("NEW_W_START_POS_INTERACTION_my.df.not.balanced.Robj"))
