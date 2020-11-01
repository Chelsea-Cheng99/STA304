#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS USA, ACS data[...UPDATE ME!!!!!]
# Author: Xi Cheng, Guangyu Du, Shichao Feng and Zhitong Liu[CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: xiaoxi.cheng@mail.utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!

getwd()
#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- read_dta("inputs/data/usa_00003.dta"
                     )
# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_data)

reduced_data <- 
  raw_data %>% 
  select(region,
         stateicp,
         sex, 
         age, 
         race, 
         hispan,
         marst, 
         bpl,
         citizen,
         educd,
         labforce,
         inctot)
rm(raw_data)
         

#### What's next? ####



         