# Packages

library(tidyverse)
library(knitr)
library(kableExtra)

n_years <- 10 # the number of player seasons we want to look at

# Reading in data (all years)

bat = read_csv('Data/Clean/bat/bat_all.csv')
pitch = read_csv('Data/Clean/pitch/pitch_all.csv')

# Performance data is limited to the 1999-2009 range within which the model is used
# This is using the revised percentiles

performance_min20_revisedpct = read_csv('Data/Clean/performance_min20_revised-pct.csv')
