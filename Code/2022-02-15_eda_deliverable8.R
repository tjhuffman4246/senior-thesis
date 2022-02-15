# Packages

library(tidyverse)
library(knitr)
library(kableExtra)

# Reading in data

bat = read_csv('Data/Clean/bat/bat_all.csv')
pitch = read_csv('Data/Clean/pitch/pitch_all.csv')
performance_min20 = read_csv('Data/Clean/performance_min20.csv')

# Creating a