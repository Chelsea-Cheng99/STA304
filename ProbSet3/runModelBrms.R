require(brms)
require(tidyverse)
require(bayesplot)

# load the subset of data
df <- read_csv("df.csv")
# use as single level as the base level 
dftest <- df %>% mutate(single = ifelse(Marital == "Single", 0, 1) )

glm01_test <- glm(single ~ age + age*sex , data = dftest, family = "binomial")
summary(glm01_test)
# fit model logistic regression 
# glm, logistic regression with age, age*sex
mylg_age0 <- glm(factor(Marital) ~ age + age*sex , data = df, family = "binomial")
summary(mylg_age0)

######## Compare the results, the single = 0 works the same as the factor(Maritla) ###########

# logistic regression with age, age*sex, age*place 
mylg_age1 <- glm(factor(Marital) ~ age + age*sex + age*place, data = df, family = "binomial")
summary(mylg_age1)

# logistic regression with age, sex, place
mylg_1 <- glm(factor(Marital) ~ age + sex + place , data = df, family = "binomial")
summary(mylg_1)


# TODO: poly(age, 3) and all its correlations 
# mylg_age <- glm(factor(Marital) ~ poly(age,3)  , data = df, family = "binomial")
# summary(mylg_age)
# 
# mylg_age4 <- glm(factor(Marital) ~ poly(age,3)  + poly(age,3)*sex, data = df, family = "binomial")
# summary(mylg_age4)
# 
# 
# mylg_final <- glm(factor(Marital) ~ poly(age,3) + sex + place , data = df, family = "binomial")
# summary(mylg_final)
# 
# mylg_f <- glm(factor(Marital) ~ poly(age,3)  + poly(age,3)*sex + + poly(age,3)*place, data = df, family = "binomial")
# summary(mylg_f)
# plot(mylg_final)

## TODO: Bayesian, only use the sex as the predictor 
# These options help Stan run faster:
  
# rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())

mc_first <- brm(factor(Marital) ~ sex,
                data = df,
                family = bernoulli(),
                seed = 666, 
                warmup = 1000, iter = 1000 + 1000, chains = 4)
summary(mc_first)
save(mc_first, file = "mc_first.Rdata")

# mc_2 <- brm(factor(Marital) ~ age + sex,
#                 data = df,
#                 family = bernoulli(),
#             # prior = c(set_prior("normal(0,20)", class = "b"),
#             #           set_prior("cauchy(0,5)", class = "sd")),
#                 seed = 666, 
#                 warmup = 1000, iter = 1000 + 1000, chains = 4)
# summary(mc_2)

# Bayesian,  use the sex as the predictor and age

mc_3 <- brm(factor(Marital) ~ age + sex,
              data = df,
              family = bernoulli(),
            # with non-informative prior
              prior = c(set_prior("normal(0,10)", class = "b"),
                        set_prior("normal(0,10)", class = "Intercept")),
              seed = 666, 
              warmup = 1000, iter = 1000 + 1000, chains = 4)
summary(mc_3)
# save fitted model 
save(mc_3, file = "mc_3.Rdata")

# leave one out cross validation compare model 
compare1_3 <- loo(mc_first, mc_3)
save(compare1_3, file = "compare1_3.Rdata")
# TODO: Bayesian hierarchical modeling 
# with sex as the group
mc_test <- brm(factor(Marital) ~ age + (1 + age|sex),
             data = df,
             # non-informative prior, for coef, sd, correlation and intercept
             prior = c(set_prior("normal(0,10)", class = "b"),
                       set_prior("cauchy(0,5)", class = "sd"),
                       set_prior("lkj(2)", class = "cor"),
                       set_prior("normal(0,10)", class = "Intercept")),
             family = bernoulli(),
             seed = 666, 
             warmup = 1000, iter = 1000 + 1000, chains = 4)
summary(mc_test)
save(mc_test, file = "mc_test.Rdata")

# TODO:
# with sex as the group, and the place as the group
mc_f <- brm(factor(Marital) ~ age + (1 + age|sex) + (1 + age|place),
               data = df,
            # non-informative priors 
               prior = c(set_prior("normal(0,10)", class = "b"),
                         set_prior("cauchy(0,5)", class = "sd"),
                         set_prior("lkj(2)", class = "cor"),
                         set_prior("normal(0,10)", class = "Intercept")),
               family = bernoulli(),
               seed = 666, 
            # multicores = 2
               warmup = 500, iter = 500 + 1000, chains = 2, cores = 2)
summary(mc_f)
# save fitted model 
save(mc_f, file = "mc_f.Rdata")
# check the posterior distribution 
post <- posterior_samples(mc_f, add_chain = T)
# mcmc_dens_overlay(post, pars = c("sd_sex__age")) +
#   theme(panel.grid = element_blank())

# load saved model data
load("mc_f.Rdata")
load("mc_test.Rdata")

# leave one out cross validation compare model 
compare_f <- LOO(mc_test, mc_f)
save(compare_f, file = "compare_f.Rdata")

# hypothesis test of group-level effects of sd.
hypothesis(mc_f, "Intercept - age > 0", class = "sd", group = "sex")
hypothesis(mc_f, "Intercept - age > 0", class = "sd", group = "place")
# future works 
# mc_lg <- brm(factor(Marital) ~ poly(age,3) + (1 + poly(age,3)|sex),
#                data = df,
#                family = bernoulli(),
#                seed = 666, 
#                warmup = 500, iter = 500 + 2500, chains = 4)
# summary(mc_lg)
# plot(mc_lg)
# 
# mc_lgfinal <- brm( factor(Marital) ~ poly(age,3) + sex + place ,
#                  data = df,
#                  family = bernoulli(),
#                  seed = 666, 
#                  warmup = 500, iter = 500 + 2500, chains = 4)
# summary(mc_lgfinal)
# plot(final_lg)
# mcmc_plot(final_lg)
# 
# 

