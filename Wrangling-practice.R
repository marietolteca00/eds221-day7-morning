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


# Practice with joins

animals <- data.frame(
  stringsAsFactors = FALSE,
          location = c("lagoon", "bluff", "creek", "oaks", "bluff"),
           species = c("bobcat", "coyote", "fox", "squirrel", "bobcat"),
          maturity = c("adult", "juvenile", "adult", "juvenile", "adult")
)

sites <- data.frame(
  stringsAsFactors = FALSE,
          location = c("beach", "lagoon", "bluff", "oaks"),
    full_site_name = c("Goleta Beach","UCSB Lagoon",
                       "Ellwood Mesa","Fremont Campground"),
      jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)


# Practice with Mutating Joins -

# Full join
full_join <- full_join(animals, sites) # Keeps all rows and adds up columns

# Left Join
left_join(animals,sites) # add on columns from site (full_site_name & juridiction)

# Right join
right_join(animals,sites) # 5 columns, add in beach from site

# Inner join
inner_join(animals,sites) # removes creek from animals since it does not match on site

# Practice with Filter joins
semi_join(animals, sites)

# same as this code
animals %>%
  filter(location %in% sites$location)

# Anti joins
anti_join(animals, sites) # if x= sites and y = animals : the output would change


#Practice with lubridate
my_date <- "03-13-1998"
lubridate::mdy(my_date) # Fixed date to ISO 8601, now 1998-03-13

# New format for date
my_date <- "08-Jun-1947"
lubridate::dmy(my_date) # changed ^ my_date to 1947-06-08

# Another Example
my_date <- "19160518"
lubridate::ymd(my_date) # changed ^ my_date to 1916-05-18

lubridate::dmy("09/12/84")

# lubridate will fail if date does not make sense.
lubridate::myd("1942-08-30") #spits out warning message

time <- "2020-08-12 11:18"
time <- ymd_hm(time)

# convert to PDT
with_tz(time, "America/Los_Angeles")

# extract info from dates
# ask for individual columns for time
week(time)
year(time)
day(time)

# Just for fun

start_time <- Sys.time() # Gets time from laptop
end_time <- Sys.time()

end_time - start_time # this tell you how long a script took to run

# Practice lubridate wihtin a data frame

urchin_count <- tribble(
  ~date, ~species, ~size_mm,
  "10/03/2020", "purple", 55,
  "10/04/2020", "red", 48,
  "11/17/2020", "red", 67
)

# using lubridate
urchin_count %>%
  mutate(date= lubridate::mdy(date)) %>%   # corrected our date to Year Month Day
  mutate(year = year(date),     # created a column for only the Year
         month= month(date), # created a column for only the Month
         day = day(date))  # created a column for only the Day

day_1 <- lubridate::ymd("2020-01-06")
day_2 <- ymd("2020-05-18")
day_3 <- ymd("2020-05-19")

# Creating time interval
time_interval <- interval(day_1, day_2)
time_length(time_interval, "week") # checking time in weeks
time_length(time_interval, "year")


# Practice with stringr

#str_detect() - This will detect string patterns
# The return will be TRUE/ FALSE, depending whether the pattern is detected

my_string <- "Teddy loves eating salmon and socks"

# Using str_detect - Does the pattern "love" exist within then string?
my_string %>%
  str_detect("pup")


my_string <- c("burrito", "fish taco", "Taco salad")

# Does the vector element contain the pattern "fish"?
my_string %>%
  str_detect("fish") # goes over the vector of three characters


# More Powerful when combined with dplyr functions

# load in starwars data in console

starwars %>%
  filter(str_detect(name,"Skywalker")) # returns name column that include Skywalker

# Renaming character names from "Sky" to "Fire"
firewalkers <- starwars %>%
  # using mutate
  # using str_replace()
  mutate(name = str_replace(name, pattern = "Sky", replacement = "Fire"))

# cleaning up white space
feedback <- c(" I ate  some nachos", "Wednesday morning    ")

# to remove leading, trailing, and duplicate spaces
# use str_squish()

str_squish(feedback)

# remove only the leading and trailing spaces
# using str_trim()
str_trim(feedback)

# Convert cases

#turning everything to lowercase
str_to_lower(feedback)
#[1] " i ate  some nachos"   "wednesday morning    "

#turning everything to Uppercase
str_to_upper(feedback)
#[1] " I ATE  SOME NACHOS"   "WEDNESDAY MORNING    "

##turning everything to sentence
str_to_sentence(feedback)
#[1] " I ate  some nachos"   "Wednesday morning    "

# #turning everything to Title
str_to_title(feedback)
#[1] " I Ate  Some Nachos"   "Wednesday Morning    "

# Count matches in a string
str_count(feedback, pattern= "nachos")
#[1] 1 0

