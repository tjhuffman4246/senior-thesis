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

exit_signals_revised_pct_no_mlb <- performance_min20_revised %>% 
  filter(level_high != 7) %>% 
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
  ylim(0, 50) +
  theme_classic()

# For good, bad, and in-between signals, excluding MLB

ggplot(exit_signals_revised_pct_no_mlb, aes(x = yr_unique, y = 100 * pct, 
                                            color = as.factor(signal))) +
  geom_line() +
  scale_color_manual(name = "Signal",
                     labels = c("Bad", "Average", "Good"),
                     values = c("red", "green3", "blue")) +
  labs(x = "Year of Career",
       y = "Percent of Population Exiting",
       caption = "Levels: Rookie through Triple-A",
       color = "Signal") +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 60, by = 10),
                     limits = c(0, 60)) +
  theme_classic()

# Code to get names of players who had strong signals in first 3 seasons...
# but still ended up exiting the system anyways

yr3_strong_exits <- performance %>% 
  filter(yr_unique <= 3) %>% 
  group_by(Name) %>% 
  mutate(cum_strong = sum(signal)) %>% 
  ungroup() %>% 
  filter(yr_unique == 3, cum_strong == 3, exit == 1) %>% 
  select(Name) %>% 
  pull()

# What percent of players who have strong signals in first 3 seasons...
# end up making the major leagues?

strong_start_high_level <- performance %>% 
  group_by(Name) %>% 
  mutate(max_level = max(level_high)) %>% 
  ungroup() %>% 
  filter(yr_unique <= 3) %>% 
  group_by(Name) %>% 
  mutate(cum_strong = sum(signal)) %>% 
  ungroup() %>% 
  filter(yr_unique == 3, cum_strong == 3) %>% 
  group_by(max_level) %>% 
  summarize(ct = n_distinct(Name)) %>% 
  mutate(pct = ct / sum(ct))

# What are the expected number of seasons in each level?

exp_yrs_total <- performance %>% 
  group_by(Name) %>% 
  mutate(total_yrs = max(yr_unique)) %>% 
  ungroup() %>% 
  group_by(Name, level_low) %>% 
  summarize(yrs = n_distinct(yr_unique),
            total_yrs = total_yrs) %>% 
  unique() %>% 
  complete(Name,
           level_low = 1:7,
           fill = list(yrs = 0)) %>% 
  group_by(Name) %>% 
  fill(total_yrs) %>% 
  ungroup() %>% 
  unique() %>% 
  group_by(Name) %>% 
  mutate(total_yrs = replace_na(total_yrs, max(total_yrs, na.rm = T))) %>% 
  ungroup() %>% 
  unique()

# Table of average salaries by year to show for NPV calculations
# salary_total / avg. for level_low = 0 is avg. annual salary

exp_yrs_total %>% 
  group_by(level_low) %>% 
  summarize(avg = mean(yrs)) %>% 
  ungroup() %>% 
  add_column(salary_wk = c(rep(290, 4), 350, 502, 400000/52),
             wks = c(rep(23, 6), 52),
             salary_yr = salary_wk * wks) %>% 
  mutate(salary_total = avg * salary_yr) %>% 
  add_row(level_low = 0,
          avg = sum(.$avg),
          salary_wk = 0,
          wks = 0,
          salary_yr = 0,
          salary_total = sum(.$salary_total))

# If you have a good season in Rookie ball to start...
# what are your expected number of seasons in each level?
