# load required 
require(tidyverse)
require(janitor)
require(forcats)

# load full dataset of 2017 gss
gssAll <- read_csv("gss.csv")

# explorative 
# gssAll %>% colnames()
# gssAll$sex %>% head(10)
# gssAll$place_birth_canada %>% head(10)
# gssAll$marital_status %>% head(30)
# gssAll$age %>% head(30)

# select a subset and remove all na rows 
dftmp <- gssAll %>% select(age, sex, place_birth_canada, marital_status) %>% drop_na()
# range(dftmp$age)
# boxplot(dftmp$age)
dim_df <- dim(dftmp)
# ggplot(data = dftmp, aes(sex)) + geom_bar(stat = "count")

# check proportions of categorical variables 
tbl_sex <- janitor::tabyl(dftmp$sex)
female <- tbl_sex[1, 1:2]
male <- tbl_sex[2, 1:2]
tbl_place <- janitor::tabyl(dftmp$place_birth_canada)
tbl_marital <- janitor::tabyl(dftmp$marital_status)

# futhure process 
df <- dftmp %>% drop_na() %>% mutate(Marital = 
                                       # combine levels to make Single, nonsingle 
                          forcats::fct_collapse(marital_status,
                          Single = c("Divorced", "Separated", "Single, never married", "Widowed"),
                          Nonsingle = c("Married", "Living common-law") ))  %>% 
  # remove don't know 
             filter(place_birth_canada != "Don't know") %>% select(-marital_status) %>% 
                       rename(place = place_birth_canada)
                        
# df %>% head(30)
# dim(df)

# check proportions of categorical variables 
tabyl(df$sex)
tabyl(df$place)
tabyl(df$Marital)
# save data, the data used in model 
write_csv(df, "df.csv")
# 1 "Married" 2 "Living common-law" 3 "Widowed" 
# 4 "Separated" 5 "Divorced" 6 "Single, never married" 
# 96 "Valid skip" 97 "Don't know" 98 "Refusal" 
# 99 "Not stated"