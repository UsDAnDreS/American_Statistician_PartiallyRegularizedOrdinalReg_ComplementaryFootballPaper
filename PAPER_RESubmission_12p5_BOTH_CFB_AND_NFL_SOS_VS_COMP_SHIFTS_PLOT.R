#####
## USED FOR PAPER 
## PLOTTING THE BOXPLOTS OF SHIFTS
#####


##########################
##########################
##########################

side <- "Offense"

load(paste0("W_START_POS_INTERACTION_SeasonLong.Shifts.CFB.",side, ".df.Robj"))
load(paste0("W_START_POS_INTERACTION_SeasonLong.Shifts.NFL.",side, ".df.Robj"))
load(paste0("W_START_POS_INTERACTION_SingleGame.Shifts.CFB.",side, ".df.Robj"))
load(paste0("W_START_POS_INTERACTION_SingleGame.Shifts.NFL.",side, ".df.Robj"))


SeasonLong.Shifts.CFB.df$SOS$Offense.Drives.Included
SeasonLong.Shifts.NFL.df$SOS$Offense.Drives.Included

boxplot(SeasonLong.Shifts.CFB.df$SOS$Shift,
        SeasonLong.Shifts.CFB.df$complem$Shift,
        SeasonLong.Shifts.NFL.df$SOS$Shift,
        SeasonLong.Shifts.NFL.df$complem$Shift)

summary(SeasonLong.Shifts.CFB.df$SOS$Offense.Drives.Included)
summary(SeasonLong.Shifts.NFL.df$SOS$Offense.Drives.Included)


# Making it at least 100 drives for CFB
min.drives <- 100
dim(SeasonLong.Shifts.CFB.df$SOS)
dim(SeasonLong.Shifts.CFB.df$SOS[SeasonLong.Shifts.CFB.df$SOS$Offense.Drives.Included >= 100, ])

seasonlong.final.df <- rbind(data.frame(SeasonLong.Shifts.CFB.df$SOS, # %>% left_join(SeasonLong.Shifts.CFB.df$complem) %>% filter(Offense.Drives.Included >= 100),
                                        Complem.Shift = SeasonLong.Shifts.CFB.df$complem$Shift,
                                        League = "CFB"),
                             data.frame(SeasonLong.Shifts.NFL.df$SOS, # %>% left_join(SeasonLong.Shifts.NFL.df$complem) %>% filter(Offense.Drives.Included >= 100),
                                        Complem.Shift =SeasonLong.Shifts.NFL.df$complem$Shift,
                                        League = "NFL"))

singlegame.final.df <- rbind(data.frame(SingleGame.Shifts.CFB.df$SOS, # %>% left_join(SingleGame.Shifts.CFB.df$complem),
                                        Complem.Shift = SingleGame.Shifts.CFB.df$complem$Shift,
                                        League = "CFB"),
                             data.frame(SingleGame.Shifts.NFL.df$SOS, # %>% left_join(SingleGame.Shifts.NFL.df$complem),
                                        Complem.Shift = SingleGame.Shifts.NFL.df$complem$Shift,
                                        League = "NFL"))

# boxplot(SeasonLong.Shifts.CFB.df$SOS$Shift[SeasonLong.Shifts.CFB.df$SOS$Offense.Drives.Included >= 100],
#         SeasonLong.Shifts.CFB.df$SOS$Shift[SeasonLong.Shifts.CFB.df$SOS$Offense.Drives.Included >= 100],
#         SeasonLong.Shifts.NFL.df$SOS$Shift,
#         SeasonLong.Shifts.NFL.df$complem$Shift)

library(gridExtra)

y_cutoff_seasonlong <- .75
p1 <- ggplot(seasonlong.final.df) +
  geom_boxplot(aes(x=League, y=Shift, fill=League)) + 
  ggtitle("Team Season-Long \nStrength-of-Schedule (SOS) Adjustment", subtitle="Offense") +
  ylim(-y_cutoff_seasonlong,y_cutoff_seasonlong) + 
  ylab("Shift in Points Scored Per Drive") +
  scale_fill_manual(values = c("NFL" = "red", "CFB" = "blue")) +
  theme(legend.position = "none")

p2 <- ggplot(seasonlong.final.df) +
  geom_boxplot(aes(x=League, y=Complem.Shift, fill=League)) + 
  ggtitle("Team Season-Long \nComplementary Football Adjustment", subtitle="Offense; Applied Post-SOS Adjustment") +
  ylim(-y_cutoff_seasonlong,y_cutoff_seasonlong) + 
  ylab("Shift in Points Scored Per Drive") + 
  scale_fill_manual(values = c("NFL" = "red", "CFB" = "blue")) +
  theme(legend.position = "none")

grid.arrange(p1, p2, ncol=2)


y_cutoff_singlegame <- 0.75
p3 <- ggplot(singlegame.final.df) +
  geom_boxplot(aes(x=League, y=Shift, fill=League)) + 
  ggtitle("Team Single-Game \nStrength-of-Schedule (SOS) Adjustment", subtitle="Offense") +
  ylim(-y_cutoff_singlegame,y_cutoff_singlegame) + 
  ylab("Shift in Points Scored Per Drive") +
  scale_fill_manual(values = c("NFL" = "red", "CFB" = "blue")) +
  theme(legend.position = "none")

p4 <- ggplot(singlegame.final.df) +
  geom_boxplot(aes(x=League, y=Complem.Shift, fill=League)) + 
  ggtitle("Team Single-Game \nComplementary Football Adjustment", subtitle="Offense; Applied Post-SOS Adjustment") +
  ylim(-y_cutoff_singlegame,y_cutoff_singlegame) + 
  ylab("Shift in Points Scored Per Drive") + 
  scale_fill_manual(values = c("NFL" = "red", "CFB" = "blue")) +
  theme(legend.position = "none")

grid.arrange(p3, p4, ncol=2)



#######
## FINAL PLOT
##  Width: 741
##  Height: 488
#######
grid.arrange(p1,p2,p3,p4, nrow=2, ncol=2)

# grid.arrange(p2,p4)
