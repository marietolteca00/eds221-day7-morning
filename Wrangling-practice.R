# Cleaning Enivronment
rm(list=ls())

# Attached packages
library(tidyverse)
library(palmerpenguins)
library(lubridate) # help us work with dates

# Data wranling refresher
# 1. Only include penguins at Briscoe and Dream Islands
# 2. Remove the year and sex variables
# 3. Add a new column called body_mass_kg with penguin mass convert grams to kilograms
# 4. rename the island variable to location

penguins %>%
  filter(island %in% c("Briscoe","Dream")) %>% # 1.
  select(-year,-sex) %>% # 2.
  mutate("body_mass_kg" = body_mass_g/1000) %>%  # 3.
  rename(locaiton=island) # 4.

# 1. Limit to only Adelie penguins
# 2. Remove any observations where flipper_length_mm is NA
# 3. Group the data by sex
# 4. Find the mean, stand deviation, and sample size (n()) of flipper lengths for male and females

penguins %>%
  filter(species == "Adelie" ) %>% # 1.
  filter(!is.na(flipper_length_mm), #2. "!is.na()" means remove rows that are not NA in flipper_length_mm
         !is.na(sex)) %>%  # this will drop NA for sex column
  group_by(sex) %>% # 3.
  summarise(mean = mean(flipper_length_mm), # 4.
            standard_dev = sd(flipper_length_mm),
            sample_size = n())

