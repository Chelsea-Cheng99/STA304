require(brms)
require(tidyverse)
require(bayesplot)
install.packages("stargazer")

df <- read_csv("df.csv")

# fit model logistic regression 
mylg_age0 <- glm(factor(Marital) ~ age + age*sex , data = df, family = "binomial")
summary(mylg_age0)

mylg_age1 <- glm(factor(Marital) ~ age + age*sex + age*place, data = df, family = "binomial")
summary(mylg_age1)

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

## TODO: Bayesian 
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

mc_3 <- brm(factor(Marital) ~ age + sex,
              data = df,
              family = bernoulli(),
              prior = c(set_prior("normal(0,10)", class = "b"),
                        set_prior("normal(0,10)", class = "Intercept")),
              seed = 666, 
              warmup = 1000, iter = 1000 + 1000, chains = 4)
summary(mc_3)
save(mc_3, file = "mc_3.Rdata")

# leave one out cross validation compare model 
compare1_3 <- loo(mc_first, mc_3)
save(compare1_3, file = "compare1_3.Rdata")
# TODO: Bayesian hierarchical modeling 

mc_test <- brm(factor(Marital) ~ age + (1 + age|sex),
             data = df,
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
mc_f <- brm(factor(Marital) ~ age + (1 + age|sex) + (1 + age|place),
               data = df,
               prior = c(set_prior("normal(0,10)", class = "b"),
                         set_prior("cauchy(0,5)", class = "sd"),
                         set_prior("lkj(2)", class = "cor"),
                         set_prior("normal(0,10)", class = "Intercept")),
               family = bernoulli(),
               seed = 666, 
               warmup = 500, iter = 500 + 1000, chains = 2, cores = 2)
summary(mc_f)
# save fitted model 
save(mc_f, file = "mc_f.Rdata")
# check the posterior distribution 
post <- posterior_samples(mc_f, add_chain = T)
mcmc_dens_overlay(post, pars = c("sd_sex__age")) +
  theme(panel.grid = element_blank())

# load saved model data
load("mc_f.Rdata")
load("mc_test.Rdata")

# leave one out cross validation compare model 
compare_f <- loo(mc_test, mc_f)
save(compare_f, file = "compare_f.Rdata")

# hypothesis test of group-level effects 
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

