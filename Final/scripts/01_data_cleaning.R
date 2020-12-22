#### Preamble ####
# Purpose: Explore the newspaper data, get familiar with the newspaper dataset, the .Rmd itself is enough
# Author:  Xi Cheng
# Data: December 9th 2020
# Contact:  Xi Cheng 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from Newspapers in Times of Low Advertising Revenues, open access
# - Don't forget to gitignore it!
# cite Newspapers in Times of Low Advertising Revenues
# Modified BSD License (https://opensource.org/licenses/BSD-3-Clause) of the paper
# only for exploratory 

getwd()
#### Workspace setup ####
library(haven)
library(forcats)
library(tidyverse)
library(magrittr)
library(broom)
# Read in the raw data (You might need to change this if you use a different dataset)
# the first dataset is for linear regression
dfNews <- read_dta("inputs/data/Angelucci_Cage_AEJMicro_dataset.dta")
# others for figures
df1 <- read_dta("inputs/data/Angelucci_Cage_AEJMicro_Descriptive_evidence_US.dta")
df23 <- read_dta("inputs/data/Angelucci_Cage_AEJMicro_Descriptive_evidence_advertising_industry.dta")
df4 <- read_dta("inputs/data/Angelucci_Cage_AEJMicro_Descriptive_evidence_advertisement.dta")
df5 <- read_dta("inputs/data/Angelucci_Cage_AEJMicro_Descriptive_evidence_television_quality.dta")

# Check names 
colnames(dfNews)
dfNews$year %>% range()
# the linear regression models were based on the scripts of the models 
head(dfNews)
head(df1)
# description of variables 
# rs_cst "Revenues from sales (million \euro)"
# ra_cst "Revenues from advertising (million \euro)"
# rtotal_cst "Total revenues (million \euro)"

table(dfNews$after_national)

################# advertising side ###############
# ln, logarithms of advertising revenues
# advertising revenues (column 1), prices (columns 2 and 3), and quantity (column 4)
lm_ra_cst2 <- lm(ln_ra_cst2 ~ after_national + factor(id_news) + factor(year), data = dfNews)
# results 
tidy(lm_ra_cst2)[1:2, ]
summary(lm_ra_cst2)$df

# ad price revenues /circulation 
# ln_ra_cst2 ln_ads_p1_cst2 ln_ads_p4_cst ln_ads_q
lm_ads_p1_cst2 <- lm(ln_ads_p1_cst2 ~ after_national + factor(id_news) + factor(year), data = dfNews)
tidy(lm_ads_p1_cst2)[1:2, ]
summary(lm_ads_p1_cst2)$df
# ad price 
lm_ads_p4_cst <- lm(ln_ads_p4_cst ~ after_national + factor(id_news) + factor(year), data = dfNews)
tidy(lm_ads_p4_cst)[1:2, ]
summary(lm_ads_p4_cst)$df

# advertising quantity 
lm_ads_q <- lm(ln_ads_q ~ after_national + factor(id_news) + factor(year), data = dfNews)
tidy(lm_ads_q)[1:2, ]
summary(lm_ads_q)$df


########### reader side ############
# ln_ps_cst ln_po_cst ln_qtotal ln_qs_s ln_rs_cst
# subscription price 
lm_ln_ps_cst <- lm(ln_ps_cst ~ after_national + factor(id_news) + factor(year), data = dfNews)
tidy(lm_ln_ps_cst)[1:2, ]
summary(lm_ln_ps_cst)$df

# unite price 
lm_ln_po_cst <- lm(ln_ps_cst ~ after_national + factor(id_news) + factor(year), data = dfNews)
tidy(lm_ln_ps_cst)[1:2, ]
summary(lm_ln_ps_cst)$df

# circulation 

# share of subscribers 

# revenues sales 