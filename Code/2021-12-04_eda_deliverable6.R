# Packages

library(tidyverse)
library(knitr)
library(kableExtra)

# Reading in data

bat = read_csv('Data/Clean/bat/bat_all.csv')
pitch = read_csv('Data/Clean/pitch/pitch_all.csv')

# Graph: hazard rate graph
# Percent of entire population that exits in that period

# Batters

n_bat <- bat %>% # number of unique batters
  select(Name) %>% 
  pull() %>% 
  unique() %>% 
  length()

exit_distr_bat <- bat %>% # distribution of player exit times
  group_by(Name) %>%
  summarise(n_years = n_distinct(Year)) %>% 
  group_by(n_years) %>% 
  summarise(n_players = n_distinct(Name)) %>% 
  mutate(pct = 100 * n_players / n_bat,
         pos = 'Batters') # 100x multiplier to make it a true percent

# Pitchers

n_pitch <- pitch %>% # number of unique pitchers
  select(Name) %>% 
  pull() %>% 
  unique() %>% 
  length()

exit_distr_pitch <- pitch %>% # distribution of player exit times
  group_by(Name) %>%
  summarise(n_years = n_distinct(Year)) %>% 
  group_by(n_years) %>% 
  summarise(n_players = n_distinct(Name)) %>% 
  mutate(pct = 100 * n_players / n_pitch,
         pos = 'Pitchers') # 100x multiplier to make it a true percent

# Merging the dataframes

exit_distr <- exit_distr_bat %>% 
  bind_rows(exit_distr_pitch)

# Graph

exit_distr %>% 
  ggplot() +
  aes(x = n_years, y = pct, color = pos) +
  geom_line() +
  geom_hline(yintercept = 0,
             linetype = 'dotted',
             col = 'red') +
  labs(x = "Number of Seasons Played",
       y = "Percent Exiting",
       title = "Professional Baseball Hazard Rate, by Year",
       caption = "Levels: Rookie through MLB",
       color = "Position") +
  theme_classic()

# Table

exit_distr %>% 
  select(-n_players) %>% 
  pivot_wider(names_from = n_years,
              values_from = pct) %>% 
  select(pos:`9`) %>% 
  rowwise() %>% 
  mutate(`10+` = 100 - sum(c_across(`1`:`9`))) %>% 
  ungroup()
