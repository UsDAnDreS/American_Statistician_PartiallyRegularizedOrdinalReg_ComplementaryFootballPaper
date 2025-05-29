#####
## A file using objects calculated in 
##  * "PAPER_RESubmission_NFL_6_Calculating_Objects_to_Feed_to_Plotting_Effects_of_Takeaways_on_Expected_Points.R"
##  * "PAPER_RESubmission_6_Calculating_Objects_to_Feed_to_Plotting_Effects_of_Takeaways_on_Expected_Points.R"
##
## to produce the Figure 3(a) in the main paper
## (showing the effect of a non-scoring takeaway
##  on the expected points scored per drive,
##  plotted against the baseline of points scored without a takeaway).
##
#####


library(tidyverse)
library(gridExtra)
library(fuzzyjoin)
library(plotrix)


side <- "Offense"


###########
## WITH BOOTSTRAP MEANS
###########


#####
## CFB
#####

# load(paste0("my.df.balanced.", side, ".Robj"))
load(paste0("NEW_W_START_POS_INTERACTION_my.df.not.balanced.Robj"))


## Some teams have fewer than 5 games recorded
df_cfb <- my.df %>% mutate(Extra = Offense.Avg.Points.With.Takeaway - Offense.Avg.Points.No.Takeaway) %>%
  # filter(Year == 2014) %>%
  filter(Offense.Games.Included > 4) %>%
  rename(y=Extra, x = Offense.Avg.Points.No.Takeaway)

df_cfb$Year_num <- df_cfb$Year
# df_cfb$Year <- paste0(df_cfb$Year, "_cfb")

# Calculate 95% quantile bands by group
quantile_data_cfb <- df_cfb %>%
  mutate(Year = factor(Year)) %>%
  group_by(Year) %>%
  mutate(bin = cut(x, breaks = 10)) %>%  # Bin the x variable
  group_by(Year, bin) %>%
  summarise(
    x_center = mean(x, na.rm = TRUE),       # Bin center
    y_lower = quantile(y, 0.025, na.rm = TRUE),  # 2.5th percentile
    y_mean = mean(y),
    y_upper = quantile(y, 0.975, na.rm = TRUE)   # 97.5th percentile
  ) %>%
  na.omit()



# ## Width: 553; Height: 444
# ggplot(df_cfb %>% mutate(Year=factor(Year)), aes(x = x, y = y, color = Year)) +
#   # geom_point(alpha = 0.5) +  # Scatterplot
#   geom_smooth(data=quantile_data_cfb,
#               aes(x = x_center, y=y_mean, #alpha=0.5,
#                   group = Year, fill = Year),
#               method="loess",
#               lwd=1,
#               se=F) + 
#   geom_ribbon(
#     data = quantile_data_cfb, 
#     aes(x = x_center, ymin = y_lower, ymax = y_upper, group = Year, fill = Year), 
#     alpha = 0.1, inherit.aes = FALSE
#   ) +  # Quantile bands
#   labs(title = "Points Added After Takeaway in College Football",
#        subtitle = "Bootstrap Mean LOESS, with 95% Quantile Bands",
#        x = "Points Scored Without A Takeaway",
#        y = "Extra Points Scored After Takeaway",
#        color = "Year",
#        fill = "Year") +
#   theme_minimal() +
#   scale_x_continuous(breaks=c(0:5))




####
## NFL
####


# load(paste0("my.df.NFL.balanced.",side,".Robj"))
load(paste0("NEW_W_START_POS_INTERACTION_my.df.NFL.not.balanced.Robj"))


df_nfl <- my.df.NFL %>% mutate(Extra = Offense.Avg.Points.With.Takeaway - Offense.Avg.Points.No.Takeaway) %>%
  # filter(Year == 2014) %>%
  filter(Offense.Games.Included > 4) %>%
  rename(y=Extra, x = Offense.Avg.Points.No.Takeaway)

df_nfl$Year_num <- df_nfl$Year
# df_nfl$Year <- paste0(df_nfl$Year, "_nfl")

# Calculate 95% quantile bands by group
quantile_data_nfl <- df_nfl %>%
  mutate(Year = factor(Year)) %>%
  group_by(Year) %>%
  mutate(bin = cut(x, breaks = 10)) %>%  # Bin the x variable
  group_by(Year, bin) %>%
  summarise(
    x_center = mean(x, na.rm = TRUE),       # Bin center
    y_lower = quantile(y, 0.025, na.rm = TRUE),  # 2.5th percentile
    y_mean = mean(y),
    #y_mean = median(y, na.rm=T),
    y_upper = quantile(y, 0.975, na.rm = TRUE)   # 97.5th percentile
  ) %>%
  na.omit()


# ## Width: 553; Height: 444
# ggplot(df_nfl %>% 
#          mutate(Year=factor(Year)), aes(x = x, y = y, color = Year)) +
#   # geom_point(alpha = 0.5) +  # Scatterplot
#   geom_smooth(data=quantile_data_nfl,
#               aes(x = x_center, y=y_mean, #alpha=0.5,
#                   group = Year, fill = Year),
#               method="loess",
#               lwd=1,
#               se=F) + 
#   geom_ribbon(
#     data = quantile_data_nfl, 
#     aes(x = x_center, ymin = y_lower, ymax = y_upper, group = Year, fill = Year), 
#     alpha = 0.1, inherit.aes = FALSE
#   ) +  # Quantile bands
#   labs(title = "Points Added After Takeaway in National Football League",
#        subtitle = "Mean LOESS, with Bootstrap 95% Quantile Bands",
#        x = "Points Scored Without A Takeaway",
#        y = "Extra Points Scored After Takeaway",
#        color = "Year",
#        fill = "Year") +
#   theme_minimal() +
#   scale_x_continuous(breaks=c(0:5))






#######
## COMBINING THE PLOTS
#######



## Width: 500; Height: 424

main_colors <- c("CFB" = "blue", "NFL" = "red")

# Combine datasets
combined_data <- rbind(
  data.frame(df_cfb, League = "CFB"),
  data.frame(df_nfl, League = "NFL")
)

quantile_data <- rbind(
  data.frame(quantile_data_cfb, League = "CFB"),
  data.frame(quantile_data_nfl, League = "NFL")
)


ggplot() +
  # LOESS smooth curves
  geom_smooth(
    data = quantile_data,
    aes(
      x = x_center, y = y_mean,
      group = interaction(League, Year),
      #linetype = Year,
      color = League  # Use League for the color of the smooth curves
    ),
    method = "loess", se = FALSE, size = 1
  ) +
  # Ribbon intervals
  geom_ribbon(
    data = quantile_data,
    aes(
      x = x_center, ymin = y_lower, ymax = y_upper,
      group = interaction(League, Year),
      fill = League  # Use League for the ribbon fill color
    ),
    alpha = 0.1, inherit.aes = FALSE
  ) +
  # Custom scales for the main color (League) and gradation (Year)
  scale_color_manual(
    values = main_colors, name = "League"
  ) +
  scale_fill_manual(
    values = main_colors, name = "League"
  ) +
  # Labels and theme
  labs(
    title = "Points-Per-Drive Added After Turnover (Avg. Start. Pos.)",
    subtitle = "Bootstrap Mean LOESS, with 95% Quantile Bands",
    x = "Points Per Drive Scored Without A Turnover",
    y = "Extra Points Scored Per Drive After Turnover"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
