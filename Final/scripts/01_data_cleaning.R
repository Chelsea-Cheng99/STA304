#### Preamble ####
# Purpose: Prepare and clean the survey data  [...UPDATE ME!!!!!]
# Author:  [CHANGE THIS TO YOUR NAME!!!!]
# Data: December 9th 2020
# Contact:  [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from 
# - Don't forget to gitignore it!

getwd()
#### Workspace setup ####
library(haven)
library(forcats)
require(labelled)
library(tidyverse)
require(magrittr)
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data_survey <- read_dta("inputs/data/ns20200625.dta")
# Add the labels  
raw_data_survey <- labelled::to_factor(raw_data_survey)
# Just keep some variables

# based on the most loved forecasting paper
# state, education, sex/gender, age, race, household_income, employment
reduced_data_survey <- 
  raw_data_survey%>% 
  select(
         # interest,
         # registration,
         # vote_2016,
         household_income,
         vote_intention,
         vote_2020,
         employment,
         gender,
         race_ethnicity,
         household_income,
         education,
         state,
         age) 
  # filter based on eligibility and age 
       # filter(age > 17) %>% filter(vote_intention != "No, I am not eligible to vote") %>% 
       # drop_na()
# get the not in operator
`%!in%` = Negate(`%in%`)
# EDA of relevant variables 
reduced_data_survey$vote_intention %>% levels()

reduced_data_survey %<>% filter(age > 17) %>% filter(vote_intention != "No, I am not eligible to vote") %>%
  # filter, 
  # Is vote a binary? If not, what are you going to do?
  # make vote a binary 
  filter(vote_2020 %in% c("Donald Trump", "Joe Biden")) %>%
  mutate(vote = ifelse(vote_2020 == "Joe Biden", 1, 0)) %>% select(-vote_intention, - vote_2020)

# reduced_data_survey %>% head()
# state, education, sex/gender, age, race, household_income, employment
# collapse levels of all the categorical variables 

# deal with the state name 
reduced_data_survey$state %>% head()
reduced_data_survey$education %>% levels()
barplot(table(reduced_data_survey$education ) )
barplot(table(reduced_data_survey$employment ) )
# use the built-in R state name 
namesState <- tibble(state_name = str_to_lower(state.name), state = state.abb)

reduced_data_survey  %<>%
  left_join(namesState, by = "state")  %>%
 mutate(state_name = ifelse(is.na(state_name), "district of columbia", state_name)) %>%
  # rename to match 
  rename(stateid = state) %>% rename(state = state_name, race = race_ethnicity) %>% 
# Maybe make some age-groups?
# Now make age groups, according to the Rohan guide, do not make too many levels
  mutate(age = ifelse(age < 30, "18-29",
                      ifelse(age < 45, "30-44",
                             ifelse(age < 60, "45-55", "60+")
                             )
                      ) ) %>%
# gender math the census 
  mutate(gender = ifelse(gender == "Male", "male", "female")) %>% 
# education 
 mutate(education =
          forcats::fct_collapse(education,
                                "High school and below" =  c("3rd Grade or less", 
                                            "Middle School - Grades 4 - 8",
                                            "Completed some high school",
                                            "High school graduate"),
                                "Some college" = c("Other post high school vocational training",
                                                   "Completed some college, but no degree"),
                                "College" = c("Associate Degree",
                                              "College Degree (such as B.A., B.S.)"),
                                "Graduate level" = c("Completed some graduate, but no degree",
                                               "Masters degree", "Doctorate degree")
                                )
                                ) %>% 
# collapse race into 4 
  mutate(race =
           forcats::fct_collapse(race,
                                 "White" =  c("White"),
                                 "African American" = 
                                   c("Black, or African American"),
                                 "Asian" = c("Asian (Asian Indian)",
                                             "Asian (Chinese)",
                                             "Asian (Filipino)",
                                             "Asian (Japanese)",
                                             "Asian (Korean)",
                                             "Asian (Vietnamese)",
                                             "Asian (Other)"),
                                 "Other" = c("American Indian or Alaska Native",
                                             "Pacific Islander (Native Hawaiian)", 
                                             "Pacific Islander (Guamanian)",
                                             "Pacific Islander (Samoan)",
                                             "Pacific Islander (Other)",
                                             "Some other race")  ) ) %>%
# combine household income into 5 categories. 
  mutate(household_income = forcats::fct_collapse(household_income,
                                                  "Less than $29,999"= c("Less than $14,999",
                                                                         "$15,000 to $19,999",
                                                                         "$20,000 to $24,999",
                                                                         "$25,000 to $29,999"),
                                                  "$30,000 to $69,999" = c("$30,000 to $34,999",
                                                                           "$35,000 to $39,999",
                                                                           "$40,000 to $44,999",
                                                                           "$45,000 to $49,999",
                                                                           "$50,000 to $54,999",
                                                                           "$55,000 to $59,999",
                                                                           "$60,000 to $64,999",
                                                                           "$65,000 to $69,999"),
                                                  "$70,000 to $99,999" = c("$70,000 to $74,999",
                                                                           "$75,000 to $79,999",
                                                                           "$80,000 to $84,999",
                                                                           "$85,000 to $89,999",
                                                                           "$90,000 to $94,999",
                                                                           "$95,000 to $99,999"),
                                                  "$100,000 to $149,999" = c("$100,000 to $124,999",
                                                                             "$125,000 to $149,999"),
                                                  "$150,000 and above" = c("$150,000 to $174,999",
                                                                           "$175,000 to $199,999",
                                                                           "$200,000 to $249,999",
                                                                           "$250,000 and above")
                                                  ) )  %>% filter(employment != "Other:") %>% 
  # collapse employment into 3 levels 
  mutate(employment = 
           forcats::fct_collapse (employment ,
                                  "employed" = c("Full-time employed", "Part-time employed", 
                                                 "Self-employed"),
                                  "not in labor force" = c("Homemaker", "Retired", "Permanently disabled",
                                                           "Student"),
                                  "unemployed" = c("Unemployed or temporarily on layoff", "Other:")
           ) )  %>% drop_na()

  
# reduced_data_survey$employment %>% levels()
# reduced_data_survey$race %>% levels()
# 
# reduced_data_survey$household_income %>% levels()
# make sure the other group has been deleted
# reduced_data_survey[reduced_data_survey$employment == "Other:", ]
reduced_data_survey$employment %>% levels()
reduced_data_survey$state %>% unique()

# barplot to briefly check the distribution of the categorical variables 
barplot(table(reduced_data_survey$gender  ) )
barplot(table(reduced_data_survey$education ) )
barplot(table(reduced_data_survey$employment ) )
barplot(table(reduced_data_survey$race ) )
barplot(table(reduced_data_survey$household_income  ) )
barplot(table(reduced_data_survey$age ) )
  
#
names(reduced_data_survey)
# make states as factor

# double check, make levels factor 
reduced_data_survey$state %<>% as.factor()
reduced_data_survey$state %>% levels()
reduced_data_survey$employment %>% levels()
reduced_data_survey$race %>% levels()
reduced_data_survey$household_income %>% levels()

reduced_data_survey$education %>% levels()

# save datasets 
write_csv(reduced_data_survey, "inputs/data/survey.csv")

write_csv(reduced_data_survey, "outputs/paper/survey.csv")

