# Packages

library(tidyverse)
library(knitr)
library(kableExtra)

n_years <- 10 # the number of player seasons we want to look at

# Reading in data (all years)

bat = read_csv('Data/Clean/bat/bat_all.csv')
pitch = read_csv('Data/Clean/pitch/pitch_all.csv')

# Performance data is limited to the 1999-2009 range within which the model is used
# This reads in the four different files we have:
# With playing time minimums and without
# With percentiles in thirds, and with percentiles in halves

performance_all = read_csv('Data/Clean/performance_all.csv')
performance_all_revised = read_csv('Data/Clean/performance_all_revised-pct.csv')
performance_min20 = read_csv('Data/Clean/performance_min20.csv')
performance_min20_revised = read_csv('Data/Clean/performance_min20_revised-pct.csv')

# First: how many people do we have leaving the data year-by-year?
# This could indicate whether there are external trends impacting exits (like the recession) 

yr_cts <- performance_min20 %>% 
  group_by(Year) %>% 
  summarize(count = n_distinct(Name),
            exits = sum(exit),
            pct = exits / count)

# Pretty constant trend over time, sample size much smaller by end

ggplot(yr_cts, aes(x = Year, y = 100 * pct)) +
  geom_line() +
  labs(x = "Year",
       y = "Percent of Population Exiting",
       caption = "Levels: Rookie through MLB") +
  scale_x_continuous(breaks = seq(1998, 2019, by = 4)) +
  scale_y_continuous(breaks = seq(20, 60, by = 5)) +
  theme_classic()

# What happens if we break it out by signal?
# We'll look at this by year of career, not by calendar year

exit_signals <- performance_min20 %>% 
  group_by(yr_unique, signal) %>% 
  summarize(count = n_distinct(Name),
            exits = sum(exit),
            pct = exits / count)

exit_signals_revised_pct <- performance_min20_revised %>% 
  group_by(yr_unique, signal) %>% 
  summarize(count = n_distinct(Name),
            exits = sum(exit),
            pct = exits / count)

# For strictly good/bad signals

ggplot(exit_signals, aes(x = yr_unique, y = 100 * pct, color = as.factor(signal))) +
  geom_line() +
  scale_color_manual(name = "Signal",
                     labels = c("Bad", "Good"),
                     values = c("red", "blue")) +
  labs(x = "Year of Career",
       y = "Percent of Population Exiting",
       caption = "Levels: Rookie through MLB",
       color = "Signal") +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  theme_classic()

# For good, bad, and in-between signals

ggplot(exit_signals_revised_pct, aes(x = yr_unique, y = 100 * pct, 
                                     color = as.factor(signal))) +
  geom_line() +
  scale_color_manual(name = "Signal",
                     labels = c("Bad", "Average", "Good"),
                     values = c("red", "green3", "blue")) +
  labs(x = "Year of Career",
       y = "Percent of Population Exiting",
       caption = "Levels: Rookie through MLB",
       color = "Signal") +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  theme_classic()
