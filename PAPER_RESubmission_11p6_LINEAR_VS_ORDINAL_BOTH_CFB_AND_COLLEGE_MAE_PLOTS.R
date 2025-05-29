## The paper.

#####
## Getting the 10-fold MAE comparison plots:
##    - Linear vs Ordinal
##    - for NFL & CFB
#####

## Seed for reproducibility of CV
set.seed(1)

library(ordinalNet)
library(tidyverse)


########
## NFL
#########


load("LINEAR_NFL_W_START_POS_INTERACTION_CV.results.with.complem.Robj")
CV.results.with.complem.NFL.Linear <- CV.results.with.complem %>% rename(MAE.mean = RMSE, MAE.sd = SE)

load("NFL_W_START_POS_INTERACTION_CV.results.with.complem.Robj")
CV.results.with.complem.NFL.Ordinal <- CV.results.with.complem
# load("NFL_W_START_POS_INTERACTION_CV.results.with.complem.prop.Robj")
# load("NFL_W_START_POS_INTERACTION_CV.results.no.complem.Robj")
# load("NFL_W_START_POS_INTERACTION_CV.results.nothing.Robj")
# load("NFL_W_START_POS_INTERACTION_CV.results.only.complem.Robj")


######
## GET THE MAE plots
######

our.df.NFL <- rbind(
  # data.frame(Model = "3. GC + SOS + C",
  #            CV.results.with.complem %>%
  #              select(Year, MAE.mean, MAE.sd)),
  data.frame(Model = "2. Ordinal",
             CV.results.with.complem.NFL.Ordinal %>%
               select(Year, MAE.mean, MAE.sd)),
  data.frame(Model = "1. Linear",
             CV.results.with.complem.NFL.Linear %>%
               select(Year, MAE.mean, MAE.sd))
)


ggplot(our.df.NFL, aes(x = Year, y = MAE.mean, color = Model, group = Model)) +
  geom_line(size = 1) +  # Line plot
  geom_errorbar(aes(ymin = MAE.mean - MAE.sd, ymax = MAE.mean + MAE.sd), width = 0.2) +  # Error bars
  scale_x_continuous(
    breaks = unique(our.df.NFL$Year)  # Ensure all unique Time values are shown
  ) +
  labs(
    title = "Year-by-Year 10-fold Cross-Validation Results, NFL",
    subtitle = "GC: Game Context; SOS: Strength-of-Schedule; C: Complementary ",
    x = "Year",
    y = "CV Mean Absolute Error, +/- Std. Error"
  ) +
  theme_minimal() +
  theme(legend.position = "top")





########
## COLLEGE
#########

load("LINEAR_CFB_W_START_POS_INTERACTION_CV.results.with.complem.Robj")
CV.results.with.complem.CFB.Linear <- CV.results.with.complem %>% rename(MAE.mean = RMSE, MAE.sd = SE)

load("CFB_W_START_POS_INTERACTION_CV.results.with.complem.Robj")
CV.results.with.complem.CFB.Ordinal <- CV.results.with.complem

# load("CFB_W_START_POS_INTERACTION_CV.results.with.complem.prop.Robj")
# load("CFB_W_START_POS_INTERACTION_CV.results.no.complem.Robj")
# load("CFB_W_START_POS_INTERACTION_CV.results.nothing.Robj")
# load("CFB_W_START_POS_INTERACTION_CV.results.only.complem.Robj")


######
## GET THE MAE plots
######

our.df.CFB <- rbind(
  # data.frame(Model = "3. GC + SOS + C",
  #            CV.results.with.complem %>%
  #              select(Year, MAE.mean, MAE.sd)),
  data.frame(Model = "2. Ordinal",
             CV.results.with.complem.CFB.Ordinal %>%
               select(Year, MAE.mean, MAE.sd)),
  data.frame(Model = "1. Linear",
             CV.results.with.complem.CFB.Linear %>%
               select(Year, MAE.mean, MAE.sd))
)


ggplot(our.df.CFB, aes(x = Year, y = MAE.mean, color = Model, group = Model)) +
  geom_line(size = 1) +  # Line plot
  geom_errorbar(aes(ymin = MAE.mean - MAE.sd, ymax = MAE.mean + MAE.sd), width = 0.2) +  # Error bars
  scale_x_continuous(
    breaks = unique(our.df.CFB$Year)  # Ensure all unique Time values are shown
  ) +
  labs(
    title = "Year-by-Year 10-fold Cross-Validation Results, College",
    subtitle = "GC: Game Context; SOS: Strength-of-Schedule; C: Complementary ",
    x = "Year",
    y = "CV Mean Absolute Error, +/- Std. Error"
  ) +
  theme_minimal() +
  theme(legend.position = "top")




####
## COMBINING
###


our.df.full <- rbind(data.frame(League="2. CFB", our.df.CFB),
                     data.frame(League="1. NFL", our.df.NFL))



# width: 697;
# Height: 455

ggplot(our.df.full, aes(x = Year, y = MAE.mean, color = Model, linetype=League)) +
  geom_line(size = 1) +  # Line plot
  geom_errorbar(aes(ymin = MAE.mean - MAE.sd, ymax = MAE.mean + MAE.sd), linetype=1, width = 0.2) +  # Error bars
  scale_x_continuous(
    breaks = unique(our.df.full$Year)  # Ensure all unique Time values are shown
  ) +
  labs(
    title = "10-fold Cross-Validation Results for Predicting Points Scored in a Drive",
    subtitle = "All models include game context, strength-of-schedule, and complem. features",
    x = "Year",
    y = "CV Mean Absolute Error, +/- Std. Error"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
