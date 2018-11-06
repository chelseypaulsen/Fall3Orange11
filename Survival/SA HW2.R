###########################
## Survival Analysis HW1 ##
##       Phase 2         ##
##      Orange 11        ##
###########################

rm(list=ls()) #clears existing variables/functions from envi

library(tidyverse)
library(survival)
#install.packages("survminer")
library(survminer)
library(flexsurv)
library(dplyr)


#katrina = read.csv(file='C:\\Users\\jlmic\\Documents\\Survival Analysis\\Data\\katrina.csv')
#katrina = read.csv(file='C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Survival\\data\\katrina.csv')
katrina = read.csv(file='C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Survival Analysis\\2017SA Data\\katrina.csv')
 
#censuring everything but flooding
Surv(time = katrina$hour, event = katrina$reason == 1)
#with(katrina, Surv(time = hour, event = reason == 1)) or can use this.

#fit aft models
#weibull - plot looks ok
fit <- survreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation +
               slope + age, data = katrina, dist = "weibull")
summary(fit)
exp(coef(fit))

fit_wb <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation +
                        slope + age, data = katrina, dist = "weibull")

plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "weibull distribution")

#exponential - doesn't look good
fit_exp <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation +
                         slope + age, data = katrina, dist = "exponential")

plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
     xlab = "hour", ylab = "cumulative hazard",
     main = "exponential distribution")

#lognormal - looks ok
fit_lnorm <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation +
                           slope + age, data = katrina, dist = "lognormal")

plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "lognormal distribution")

# log-logistic -looks ok
fit_llogis <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation +
                            slope + age, data = katrina, dist = "llogis")

plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard",
     main = "log-logistic distribution")

