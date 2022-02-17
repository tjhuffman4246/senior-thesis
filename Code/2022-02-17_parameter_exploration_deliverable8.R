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
            params_aaa)
