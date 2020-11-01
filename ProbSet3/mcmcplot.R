require(tidyverse)
# citations of all used packages
# citation("tidyverse")
# citation("forcats")
# citation("janitor")
# citation("brms")
# citation("bayesplot")
# citation("captioner")
# citation("stargazer")
# citation("arsenal")
# citation("knitr")
# citation("tidybayes")
# citation("base")

load("mc_test.Rdata")
# summary results of mc model fitting 
summary(mc_test)
# diagnostic plots of mcmc plot
plot(mc_test)
load("mc_f.Rdata")
summary(mc_f)
plot(mc_f)

