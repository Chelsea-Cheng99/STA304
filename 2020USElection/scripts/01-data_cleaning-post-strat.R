#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS USA, ACS data[...UPDATE ME!!!!!]
# Author: Xi Cheng, Guangyu Du, Shichao Feng and Zhitong Liu[CHANGE THIS TO YOUR NAME!!!!]
# Data: 30 October 2020
# Contact: xiaoxi.cheng@mail.utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!

getwd()
#### Workspace setup ####
library(haven)
library(tidyverse)
library(labelled)
require(magrittr)
# Read in the raw data. 
raw_data_census <- read_dta("inputs/data/usa_00001.dta")
# Add the labels
raw_data_census <- labelled::to_factor(raw_data_census )

# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_data_census)

# based on the most loved forecasting paper
# State, education, sex, age, race, household_income, employment

reduced_data_census <- 
  raw_data_census %>% 
  select(
         # region,
         stateicp,
         sex, 
         age, 
         race, 
        # hispan,
        empstat,
         # marst, 
         # bpl,
         citizen,
         educd,
         # labforce,
         inctot) %>% rename(state = stateicp, gender = sex,
                            employment = empstat, education = educd,
                            household_income = inctot)
# match state names 
namesState <- tibble(state = str_to_lower(state.name), stateid = state.abb)
reduced_data_census %<>%
  left_join(namesState, by = "state") 
# to release the memory 
rm(raw_data_census)
# get variable names, match 
names(reduced_data_census)
#### What's next? ####
# get the not in operator
`%!in%` = Negate(`%in%`)
# the data of post-stratification should match the survey data
# check the state data, 50 states plus a district
reduced_data_census$state %<>% as.factor()
reduced_data_census$state %>% levels()
############### EDA ####################
# make age as numeric, histogram, distribution
reduced_data_census$age %<>% as.numeric()
hist(reduced_data_census$age)
reduced_data_census$race %>% levels()
reduced_data_census$education %>% levels()
# income is numeric, distribution
hist(reduced_data_census$household_income)
mean(reduced_data_census$household_income)

# barplot of citizen, groups 
barplot(table(reduced_data_census$citizen) )
barplot(table(reduced_data_census$gender ) )

print("There are a lot of n/a in citizen, ignore to keep most of the data")
# thus first eligibility, filter data by age and citizenship
reduced_data_census$citizen %>% levels()

# only keep eligible person 
reduced_data_census$employment %>% levels()

# remove employment n/a
reduced_data_census %<>% filter(citizen %!in% c("not a citizen", 
                              "foreign born, citizenship status not reported"))  %>%
                       filter(age > 17) %>% filter(employment != "n/a") %>% select(-citizen) %>%
                  filter(education != "missing") %>% 
   # combine removed levels
  # deal with the employment 
   mutate(employment = forcats::fct_collapse( employment,
                                              "employed" = c("employed", "n/a"
                                                              ) )  )
# now the income data looks good 
hist(reduced_data_census$household_income)
barplot(table(reduced_data_census$employment ) )
# make age into 4 groups 
reduced_data_census %<>% 
  # Maybe make some age-groups?
  # Now make age groups, according to the Rohan guide, do not make too many levels
  mutate(age = ifelse(age < 30, "18-29",
                      ifelse(age < 45, "30-44",
                             ifelse(age < 60, "45-55", "60+")
                      )
  ) ) %>%
# the gender/sex of two datasets are matched 
# now we need do education, income and race 
# first income 
  mutate(household_income = case_when(
    household_income <= 29999 ~ "Less than $29,999",
    household_income >= 30000 & household_income <= 69999 ~ "$30,000 to $69,999",
    household_income >= 70000 & household_income <= 99999 ~ "$70,000 to $99,999",
    household_income >= 100000 & household_income <= 149999 ~ "$100,000 to $149,999",
    household_income >= 150000  ~ "$150,000 and above") ) %>%
# deal with the race, combine into 4 groups 
mutate(race = forcats::fct_collapse(race,
                                     "White" = c("white") ,
                                    "African American" = c("black/african american/negro"),
                                    "Other" = c("american indian or alaska native",
                                                "other race, nec",
                                                "two major races",
                                                "three or more major races"),
                                    "Asian" = c("chinese",
                                                "japanese",
                                                "other asian or pacific islander"))) %>%
# now combine the education 
  mutate(education = case_when(
    education == "missing" ~ "High school and below",
    education == "n/a or no schooling" ~ "High school and below",
    education == "n/a" ~ "High school and below",
    education == "no schooling completed" ~ "High school and below",
    education == "nursery school to grade 4" ~ "High school and below",
    education == "nursery school, preschool" ~ "High school and below",
    education == "kindergarten" ~ "High school and below",
    education == "grade 1, 2, 3, or 4" ~ "High school and below",
    education == "grade 1" ~ "High school and below",
    education == "grade 2" ~ "High school and below",
    education == "grade 3" ~ "High school and below",
    education == "grade 4" ~ "High school and below",
    education == "grade 5, 6, 7, or 8" ~ "High school and below",
    education == "grade 5 or 6" ~ "High school and below",
    education == "grade 5" ~ "High school and below",
    education == "grade 6" ~ "High school and below",
    education == "grade 7 or 8" ~ "High school and below",
    education == "grade 7" ~ "High school and below",
    education == "grade 8" ~ "High school and below",
    education == "grade 9" ~ "High school and below",
    education == "grade 10" ~ "High school and below",
    education == "grade 11" ~ "High school and below",
    education == "grade 12" ~ "High school and below",
    education == "12th grade, no diploma" ~ "High school and below",
    education == "high school graduate or ged" ~ "High school and below",
    education == "regular high school diploma" ~ "High school and below",
    education == "ged or alternative credential" ~ "High school and below",
    education == "some college, but less than 1 year" ~ "Some college",
    education == "1 year of college" ~ "Some college",
    education == "1 or more years of college credit, no degree" ~ "Some college",
    education == "2 years of college" ~ "Some college",
    education == "associate's degree, type not specified" ~ "College",
    education == "associate's degree, occupational program" ~ "College",
    education == "associate's degree, academic program" ~ "College",
    education == "3 years of college" ~ "Some college",
    education == "4 years of college" ~ "Some college",
    education == "bachelor's degree" ~ "College",
    education == "5+ years of college" ~ "Graduate level",
    education == "6 years of college (6+ in 1960-1970)" ~ "Graduate level",
    education == "7 years of college" ~ "Graduate level",
    education == "8+ years of college" ~ "Graduate level",
    education == "master's degree" ~ "Graduate level",
    education == "professional degree beyond a bachelor's degree" ~ "Graduate level",
    education == "doctoral degree" ~ "Graduate level"))

# double check the results 
reduced_data_census$household_income %<>% as.factor()
reduced_data_census$education %<>% as.factor()

# relevel the household_income and education
reduced_data_census$household_income <- factor(reduced_data_census$household_income,
                                      levels(reduced_data_census$household_income)[c(5, 3, 4,1:2)])
reduced_data_census$education <- factor(reduced_data_census$education,
                                        levels(reduced_data_census$education)[c(3, 4,1:2)])

reduced_data_census$household_income %>% levels()
reduced_data_census$education %>% levels()

# doule check the distribution of data, categorical
barplot(table(reduced_data_census$gender  ) )
barplot(table(reduced_data_census$race ) )
barplot(table(reduced_data_census$household_income  ) )
barplot(table(reduced_data_census$employment  ) )
barplot(table(reduced_data_census$age ) )
barplot(table(reduced_data_census$education ) )

# save datasets
write_csv(reduced_data_census, "inputs/data/census.csv")

write_csv(reduced_data_census, "outputs/paper/census.csv")
         