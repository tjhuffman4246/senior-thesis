# Packages

library(tidyverse)
library(knitr)

# Reading in parameter files

params_rookie <- read_csv('Scratch/2022-02-17 Results/Rookie/2022-02-17_result_params_rookie.csv') %>% 
  mutate(level = "Rookie")

params_short_a <- read_csv('Scratch/2022-02-17 Results/Short-Season A/2022-02-17_result_params_short-season-a.csv') %>% 
  mutate(level = "Short-Season A")

params_low_a <- read_csv('Scratch/2022-02-17 Results/A/2022-02-17_result_params_a.csv') %>% 
  mutate(level = "A")

params_high_a <- read_csv('Scratch/2022-02-17 Results/Adv A/2022-02-17_result_params_adv-a.csv') %>% 
  mutate(level = "Adv A")

params_aa <- read_csv('Scratch/2022-02-17 Results/AA/2022-02-17_result_params_aa.csv') %>% 
  mutate(level = "AA")

params_aaa <- read_csv('Scratch/2022-02-17 Results/AAA/2022-02-17_result_params_aaa.csv') %>% 
  mutate(level = "AAA")

# Combining into one dataframe

params_all <- params_rookie %>% 
  bind_rows(params_short_a,
            params_low_a,
            params_high_a,
            params_aa,
            params_aaa) %>% 
  mutate(mu = a / (a + b),
         sigma = sqrt((a * b) / ((a + b)^2 * (a + b + 1)))) %>% 
  add_row(a = mean(.$a),
          b = mean(.$b),
          theta_0 = mean(.$theta_0),
          alpha = mean(.$alpha),
          tau = mean(.$tau),
          level = "MEAN",
          mu = mean(.$mu),
          sigma = mean(.$sigma))

# Adding in summary graph of different level-based implementations

# Reading in movement files

mvmt_rookie <- read_csv('Scratch/2022-02-17 Results/Rookie/2022-02-17_result_probs_rookie.csv') %>% 
  mutate(level = "Rookie")

mvmt_short_a <- read_csv('Scratch/2022-02-17 Results/Short-Season A/2022-02-17_result_probs_short-season-a.csv') %>% 
  mutate(level = "Short-Season A")

mvmt_low_a <- read_csv('Scratch/2022-02-17 Results/A/2022-02-17_result_probs_a.csv') %>% 
  mutate(level = "Low-A")

mvmt_high_a <- read_csv('Scratch/2022-02-17 Results/Adv A/2022-02-17_result_probs_adv-a.csv') %>% 
  mutate(level = "High-A")

mvmt_aa <- read_csv('Scratch/2022-02-17 Results/AA/2022-02-17_result_probs_aa.csv') %>% 
  mutate(level = "Double-A")

mvmt_aaa <- read_csv('Scratch/2022-02-17 Results/AAA/2022-02-17_result_probs_aaa.csv') %>% 
  mutate(level = "Triple-A")

# Combining into one dataframe

mvmt_all <- mvmt_rookie %>% 
  bind_rows(mvmt_short_a,
            mvmt_low_a,
            mvmt_high_a,
            mvmt_aa,
            mvmt_aaa) %>% 
  mutate(level = factor(level, levels = c('Rookie',
                                          'Short-Season A',
                                          'Low-A',
                                          'High-A',
                                          'Double-A',
                                          'Triple-A')))

# All exit predictions/observed

mvmt_all %>% 
  filter(exit) %>% 
  ggplot() +
  aes(x = year, y = 100*pct, color = observed) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,10,5)) +
  scale_y_continuous(breaks = seq(0,40,10)) +
  scale_color_manual(name = "Probability",
                     labels = c("Predicted", "Observed"),
                     values = c("red", "blue")) +
  labs(x = "Number of Seasons Played",
       y = "Exit Probability (Percent)") +
  theme_classic() +
  facet_wrap(vars(level), ncol = 3)

# All promotion predictions/observed

mvmt_all %>% 
  filter(!exit) %>% 
  ggplot() +
  aes(x = year, y = 100*pct, color = observed) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,10,5)) +
  scale_y_continuous(breaks = seq(0,50,10)) +
  scale_color_manual(name = "Probability",
                     labels = c("Predicted", "Observed"),
                     values = c("red", "blue")) +
  labs(x = "Number of Seasons Played",
       y = "Promotion Probability (Percent)") +
  theme_classic() +
  facet_wrap(vars(level), ncol = 3)
