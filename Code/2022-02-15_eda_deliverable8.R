# Packages

library(tidyverse)
library(knitr)
library(kableExtra)

n_years <- 10 # the number of player seasons we want to look at

# Reading in data (all years)

bat = read_csv('Data/Clean/bat/bat_all.csv')
pitch = read_csv('Data/Clean/pitch/pitch_all.csv')

# Performance data is limited to the 1999-2009 range within which the model is used

performance_min20 = read_csv('Data/Clean/performance_min20.csv')

# Creating a new file that has all performance, and not just a minimum of 20 PA/BF

bat_performance_all <- bat %>%
  mutate(Level = case_when(Level == "Rookie" ~ 1,
                           Level == "Short-Season A" ~ 2,
                           Level == "A" ~ 3,
                           Level == "Adv A" ~ 4,
                           Level == "AA" ~ 5,
                           Level == "AAA" ~ 6,
                           Level == "MLB" ~ 7)) %>%  # converts level to scalar for computation
  group_by(Name) %>% 
  mutate(yr_unique = cumsum(!duplicated(Year)), # counter of unique year played
         yr_debut = min(Year), # year of debut
         exit = if_else(yr_unique == max(yr_unique), 1, 0)) %>% # whether they exited
  ungroup() %>%
  group_by(Name, Year) %>% 
  # one type of promotion:
  # if they played at multiple levels in the same year, we ASSUME this is a promotion
  # will have to lay out more rigorously why this assumption works
  mutate(promotion = if_else(n_distinct(Level) != 1, 1, 0)) %>%
  ungroup() %>%
  group_by(Name, Year) %>% 
  mutate(Age = max(Age),
         pa_total = sum(PA), # total PA that season, for filtering purposes
         level_high = max(Level),
         level_low = min(Level), # so we know if next level is greater (for promotions)
         yr_unique = max(yr_unique),
         yr_debut = yr_debut,
         exit = max(exit),
         promotion = max(promotion)) %>% 
  ungroup() %>% 
  group_by(Name) %>% 
  # promoted if you play at a strictly higher level next year
  mutate(promotion = if_else(level_high < lead(level_low, default = 0), 1, promotion),
         min_pa = min(pa_total)) %>% 
  ungroup() %>% 
  group_by(Year, Level) %>% 
  # creating OPS threshold for a good signal
  mutate(ops_threshold = quantile(OPS, probs = 1/2, na.rm = TRUE),
         signal = if_else(OPS >= ops_threshold, 1, 0, missing = 0)) %>% 
  ungroup() %>% 
  arrange(Name, Year, Level) %>% # makes it easier to get lowest-level signal (our signal)
  group_by(Name, Year) %>% 
  summarize(age = Age,
            pa_total = pa_total,
            level_high = level_high,
            level_low = level_low,
            yr_unique = yr_unique,
            yr_debut = yr_debut,
            exit = exit,
            promotion = max(promotion),
            signal = first(signal)) %>% 
  unique() %>% # gets unique observations
  filter(yr_unique <= n_years, # takes only the first ten years of someone's career
         yr_debut >= 1999, # so we know that the first observation is a debut
         yr_debut <= 2009) # in order to know whether they exit at the end

pitch_performance_all <- pitch %>%
  mutate(Level = case_when(Level == "Rookie" ~ 1,
                           Level == "Short-Season A" ~ 2,
                           Level == "A" ~ 3,
                           Level == "Adv A" ~ 4,
                           Level == "AA" ~ 5,
                           Level == "AAA" ~ 6,
                           Level == "MLB" ~ 7)) %>%  # converts level to scalar for computation
  group_by(Name) %>% 
  mutate(yr_unique = cumsum(!duplicated(Year)), # counter of unique year played
         yr_debut = min(Year), # year of debut
         exit = if_else(yr_unique == max(yr_unique), 1, 0)) %>% # whether they exited
  ungroup() %>%
  group_by(Name, Year) %>% 
  # one type of promotion:
  # if they played at multiple levels in the same year, we ASSUME this is a promotion
  # will have to lay out more rigorously why this assumption works
  mutate(promotion = if_else(n_distinct(Level) != 1, 1, 0)) %>%
  ungroup() %>%
  group_by(Name, Year) %>% 
  mutate(Age = max(Age),
         bf_total = sum(BF), # total PA that season, for filtering purposes
         level_high = max(Level),
         level_low = min(Level), # so we know if next level is greater (for promotions)
         yr_unique = max(yr_unique),
         yr_debut = yr_debut,
         exit = max(exit),
         promotion = max(promotion)) %>% 
  ungroup() %>% 
  group_by(Name) %>% 
  # promoted if you play at a strictly higher level next year
  mutate(promotion = if_else(level_high < lead(level_low, default = 0), 1, promotion),
         min_bf = min(bf_total)) %>% 
  ungroup() %>% 
  group_by(Year, Level) %>% 
  # creating ERA threshold for a good signal
  # ** if altering this, because lower is better, the quantiles are reversed **
  # i.e., the 75th percentile would actually be probs = 1/4
  mutate(era_threshold = quantile(ERA, probs = 1/2, na.rm = TRUE),
         signal = if_else(ERA <= era_threshold, 1, 0, missing = 0)) %>% 
  ungroup() %>% 
  arrange(Name, Year, Level) %>% # makes it easier to get lowest-level signal (our signal)
  group_by(Name, Year) %>% 
  summarize(age = Age,
            bf_total = bf_total,
            level_high = level_high,
            level_low = level_low,
            yr_unique = yr_unique,
            yr_debut = yr_debut,
            exit = exit,
            promotion = max(promotion),
            signal = first(signal)) %>% 
  unique() %>% # gets unique observations
  filter(yr_unique <= n_years, # takes only the first ten years of someone's career
         yr_debut >= 1999, # so we know that the first observation is a debut
         yr_debut <= 2009) # in order to know whether they exit at the end

performance_all <- bat_performance_all %>% 
  bind_rows(pitch_performance_all) %>% 
  mutate(is_pitcher = if_else(is.na(pa_total), T, F),
         pa_bf = if_else(is.na(pa_total), bf_total, pa_total)) %>% 
  select(-c(pa_total, bf_total))

performance_all %>% 
  write_csv('Data/Clean/performance_all.csv')

# Filtering the batting and pitching data
# Done by taking the names that are in the performance data
# Doing this because the years don't align with previous EDA

names_all <- performance_all %>% 
  ungroup() %>% 
  select(Name) %>% 
  unique() %>% 
  pull()

names_min20 <- performance_min20 %>% 
  ungroup() %>% 
  select(Name) %>% 
  unique() %>% 
  pull()

bat_filtered_all <- bat %>% 
  filter(Name %in% names_all) %>% 
  mutate(pa_bf = PA,
         is_pitcher = F) %>% 
  select(Name, Year, Level, Org, pa_bf, is_pitcher)

pitch_filtered_all <- pitch %>% 
  filter(Name %in% names_all) %>% 
  mutate(pa_bf = BF,
         is_pitcher = T) %>% 
  select(Name, Year, Level, Org, pa_bf, is_pitcher)

bat_filtered_min20 <- bat_min_pa_20 %>% 
  filter(Name %in% names_min20) %>% 
  mutate(pa_bf = PA,
         is_pitcher = F) %>% 
  select(Name, Year, Level, Org, pa_bf, is_pitcher)

pitch_filtered_min20 <- pitch_min_bf_20 %>% 
  filter(Name %in% names_min20) %>% 
  mutate(pa_bf = BF,
         is_pitcher = T) %>% 
  select(Name, Year, Level, Org, pa_bf, is_pitcher)

# Combining data into singular dataframes

summary_stats_all <- bat_filtered_all %>% 
  bind_rows(pitch_filtered_all)

summary_stats_min20 <- bat_filtered_min20 %>% 
  bind_rows(pitch_filtered_min20)

# Number of players, both with minimums and without

n_all <- summary_stats_all %>% 
  select(Name) %>% 
  pull() %>% 
  unique() %>% 
  length()

n_min <- summary_stats_min20 %>% 
  select(Name) %>% 
  pull() %>% 
  unique() %>% 
  length()

# Number of years played, both with minimums and without

n_years_all <- summary_stats_all %>% 
  group_by(Name) %>% 
  summarize(count = n_distinct(Year)) %>% 
  select(count) %>% 
  pull()

n_years_min <- summary_stats_min20 %>% 
  group_by(Name) %>% 
  summarize(count = n_distinct(Year)) %>% 
  select(count) %>% 
  pull()

# Number of teams played for, both with minimums and without

n_teams_all <- summary_stats_all %>% 
  group_by(Name) %>% 
  summarize(count = n_distinct(Org)) %>% 
  select(count) %>% 
  pull()

n_teams_min <- summary_stats_min20 %>% 
  group_by(Name) %>% 
  summarize(count = n_distinct(Org)) %>% 
  select(count) %>% 
  pull()

# Number of levels played at, both with minimums and without

n_levels_all <- summary_stats_all %>% 
  group_by(Name) %>% 
  summarize(count = n_distinct(Level)) %>% 
  select(count) %>% 
  pull()

n_levels_min <- summary_stats_min20 %>% 
  group_by(Name) %>% 
  summarize(count = n_distinct(Level)) %>% 
  select(count) %>% 
  pull()

# Number of PA/BF per year, both with minimums and without

n_pa_bf_all <- summary_stats_all %>% 
  group_by(Name, Year) %>% 
  summarize(pa_bf_total = sum(pa_bf)) %>% 
  select(pa_bf_total) %>% 
  pull()

n_pa_bf_min <- summary_stats_min20 %>% 
  group_by(Name, Year) %>% 
  summarize(pa_bf_total = sum(pa_bf)) %>% 
  select(pa_bf_total) %>% 
  pull()

player_summary = tibble(
  Measure = c("Years Played", "Organizations", "Levels", "PA/BF per Year"),
  M_1  = c(mean(n_years_all), mean(n_teams_all), mean(n_levels_all), mean(n_pa_bf_all)),
  Pct10_1 = c(quantile(n_years_all, .1)[[1]], 
              quantile(n_teams_all, .1)[[1]], 
              quantile(n_levels_all, .1)[[1]], 
              quantile(n_pa_bf_all, .1)[[1]]),
  Pct90_1 = c(quantile(n_years_all, .9)[[1]], 
              quantile(n_teams_all, .9)[[1]], 
              quantile(n_levels_all, .9)[[1]], 
              quantile(n_pa_bf_all, .9)[[1]]),
  SD_1 = c(sd(n_years_all), sd(n_teams_all), sd(n_levels_all), sd(n_pa_bf_all)),
  M_2  = c(mean(n_years_min), mean(n_teams_min), mean(n_levels_min), mean(n_pa_bf_min)),
  Pct10_2 = c(quantile(n_years_min, .1)[[1]], 
              quantile(n_teams_min, .1)[[1]], 
              quantile(n_levels_min, .1)[[1]], 
              quantile(n_pa_bf_min, .1)[[1]]),
  Pct90_2 = c(quantile(n_years_min, .9)[[1]], 
              quantile(n_teams_min, .9)[[1]], 
              quantile(n_levels_min, .9)[[1]], 
              quantile(n_pa_bf_min, .9)[[1]]),
  SD_2 = c(sd(n_years_min), sd(n_teams_min), sd(n_levels_min), sd(n_pa_bf_min))
) # players who have big playing time get injured and then excluded from data?

# Replicating this analysis, but only for players who make the majors
# Getting somebody's most common position (pitcher or hitter)
# Filtering out the seasons where they were not doing that thing

summary_stats_mlb <- summary_stats_all %>% 
  group_by(Name) %>% 
  filter(any(Level == 'MLB')) %>% 
  mutate(is_max = if_else(pa_bf == max(pa_bf), T, F),
         pitcher = (is_max & is_pitcher),
         pitcher = as.logical(max(pitcher))) %>% 
  filter(is_pitcher == pitcher) %>% 
  ungroup() %>% 
  arrange(Name) %>% 
  select(-is_max, -pitcher) %>% 
  inner_join(performance_all %>% 
               select(Name, Year, yr_debut, is_pitcher),
             by = c("Name", "Year", "is_pitcher"))

# Number of players

n_mlb <- summary_stats_mlb %>% 
  select(Name) %>% 
  pull() %>% 
  unique() %>% 
  length()

# Number of years played

n_years_mlb <- summary_stats_mlb %>% 
  group_by(Name) %>% 
  summarize(count = n_distinct(Year)) %>% 
  select(count) %>% 
  pull()

# Number of teams played for

n_teams_mlb <- summary_stats_mlb %>% 
  group_by(Name) %>% 
  summarize(count = n_distinct(Org)) %>% 
  select(count) %>% 
  pull()

# Number of levels played at

n_levels_mlb <- summary_stats_mlb %>% 
  group_by(Name) %>% 
  summarize(count = n_distinct(Level)) %>% 
  select(count) %>% 
  pull()

# Number of PA/BF per year

n_pa_bf_mlb <- summary_stats_mlb %>% 
  group_by(Name, Year) %>% 
  summarize(pa_bf_total = sum(pa_bf)) %>% 
  select(pa_bf_total) %>% 
  pull()

mlb_player_summary = tibble(
  Measure = c("Years Played", "Organizations", "Levels", "PA/BF per Year"),
  M_1  = c(mean(n_years_mlb), mean(n_teams_mlb), mean(n_levels_mlb), mean(n_pa_bf_mlb)),
  Pct10_1 = c(quantile(n_years_mlb, .1)[[1]], 
              quantile(n_teams_mlb, .1)[[1]], 
              quantile(n_levels_mlb, .1)[[1]], 
              quantile(n_pa_bf_mlb, .1)[[1]]),
  Pct90_1 = c(quantile(n_years_mlb, .9)[[1]], 
              quantile(n_teams_mlb, .9)[[1]], 
              quantile(n_levels_mlb, .9)[[1]], 
              quantile(n_pa_bf_mlb, .9)[[1]]),
  SD_1 = c(sd(n_years_mlb), sd(n_teams_mlb), sd(n_levels_mlb), sd(n_pa_bf_mlb))
) 

# Graph of player count by year

# Getting number of unique players in each year, and joining into one dataframe

bat_ct <- bat %>% 
  group_by(Year) %>% 
  summarize(ct = n_distinct(Name),
            pos = 'bat')

pitch_ct <- pitch %>% 
  group_by(Year) %>% 
  summarize(ct = n_distinct(Name),
            pos = 'pitch')

player_ct <- bat_ct %>% 
  rbind(pitch_ct)

# Plotting this via ggplot

ggplot(player_ct, aes(x = Year, y = ct, color = pos)) +
  geom_line() +
  scale_color_manual(name = "Position",
                     labels = c("Batters", "Pitchers"),
                     values = c("red", "blue")) +
  labs(x = "Year",
       y = "Player Count",
       caption = "Levels: Rookie, Short-Season A, Low-A, High-A, Double-A, Triple-A, MLB",
       color = "Position") +
  scale_x_continuous(breaks = seq(1998, 2019, by = 4)) +
  scale_y_continuous(breaks = seq(3000, 4500, by = 250)) +
  theme_classic()

# Graph: hazard rate graph
# Percent of entire population that exits in that period

# Batters

n_bat <- bat %>% # number of unique batters
  filter(Name %in% names_all) %>% 
  select(Name) %>% 
  pull() %>% 
  unique() %>% 
  length()

exit_distr_bat <- bat %>% # distribution of player exit times
  filter(Name %in% names_all) %>% 
  group_by(Name) %>%
  summarise(n_years = n_distinct(Year)) %>% 
  group_by(n_years) %>% 
  summarise(n_players = n_distinct(Name)) %>% 
  mutate(pct = 100 * (n_players / n_bat),
         cum_pct = cumsum(pct),
         pos = 'Batters') # 100x multiplier to make it a true percent

# Pitchers

n_pitch <- pitch %>% # number of unique pitchers
  filter(Name %in% names_all) %>% 
  select(Name) %>% 
  pull() %>% 
  unique() %>% 
  length()

exit_distr_pitch <- pitch %>% # distribution of player exit times
  filter(Name %in% names_all) %>%
  group_by(Name) %>%
  summarise(n_years = n_distinct(Year)) %>% 
  group_by(n_years) %>% 
  summarise(n_players = n_distinct(Name)) %>% 
  mutate(pct = 100 * (n_players / n_pitch),
         cum_pct = cumsum(pct),
         pos = 'Pitchers') # 100x multiplier to make it a true percent

# Merging the dataframes

exit_distr <- exit_distr_bat %>% 
  bind_rows(exit_distr_pitch) %>% 
  add_row(n_years = 0,
          n_players = 0,
          pct = 0,
          cum_pct = 0,
          pos = "Batters",
          .before = 1) %>% 
  add_row(n_years = 0,
          n_players = 0,
          pct = 0,
          cum_pct = 0,
          pos = "Pitchers",
          .before = 2)

# Graph

exit_distr %>% 
  ggplot() +
  aes(x = n_years, y = cum_pct, color = pos) +
  geom_line() +
  geom_hline(yintercept = 0,
             linetype = 'dotted',
             col = 'black') +
  scale_color_manual(name = "Position",
                     labels = c("Batters", "Pitchers"),
                     values = c("red", "blue")) +
  labs(x = "Number of Seasons Played",
       y = "Cumulative Percent Exited",
       caption = "Levels: Rookie through MLB",
       color = "Position") +
  scale_y_continuous(breaks = seq(20, 100, by = 20)) +
  theme_classic()

# Table

exit_distr %>% 
  select(-n_players) %>% 
  pivot_wider(names_from = n_years,
              values_from = cum_pct) # %>% 
  select(pos:`9`) %>% 
  rowwise() %>% 
  mutate(`10+` = 100 - sum(c_across(`1`:`9`))) %>% 
  ungroup()

# T+1 transition matrix for all players in the data (1999-2009 debuts)
  
transition_matrix <- performance_all %>% 
  filter(yr_unique <= 2) %>% 
  arrange(Name) %>% 
  group_by(Name, yr_unique) %>% 
  filter(pa_bf == max(pa_bf)) %>% 
  ungroup() %>% 
  group_by(Name) %>% 
  mutate(next_lev = lead(level_low),
         next_lev = if_else(yr_unique == 2, -1, next_lev),
         next_lev = replace_na(next_lev, 0)) %>% 
  ungroup() %>% 
  filter(yr_unique == 1) %>% 
  group_by(level_low, next_lev) %>% 
  summarize(ct = n_distinct(Name)) %>% 
  ungroup() %>% 
  group_by(level_low) %>% 
  mutate(total = sum(ct),
         pct = 100 * (ct / total))
  
  # group_by(yr_unique, level_low) %>% 
  # summarize(count = n_distinct(Name)) %>% 
  # ungroup() %>% 
  # group_by(yr_unique) %>% 
  # mutate(total = sum(count)) %>% 
  # ungroup() %>%
  # mutate(total_start = max(total),
  #        pct = 100 * (count / total_start)) %>% 
  # ungroup() %>% 
  # add_row(yr_unique = 2,
  #         level_low = 0,
  #         count = .$total_start[1] - min(.$total),
  #         total = min(.$total),
  #         total_start = max(.$total),
  #         pct = 100 * (count / total_start))
