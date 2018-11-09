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
#install.packages('flexsurv')
library(flexsurv)
library(dplyr)


katrina = read.csv(file='C:\\Users\\jlmic\\Documents\\Survival Analysis\\Data\\katrina.csv')
#katrina = read.csv(file='C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Survival\\data\\katrina.csv')
#katrina = read.csv(file='C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Survival Analysis\\2017SA Data\\katrina.csv')
View(katrina)
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
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Weibull Distribution Cummulative Hazard Plot", xlim=c(0, 50),ylim=c(0,0.25))

#exponential - doesn't look good
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
     main = "log-logistic distribution", xmax=20)

##### Choose Weibull Distribution # Checks OUT BOOM
fit_wb2 <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack , data = katrina, dist = "weibull")

plot(fit_wb2, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "weibull distribution")

# What pumps should we fix?
# First 20 that do not have a servo
# 6 of the lowest pumps had a servo
sorted_katrina <- katrina[order(katrina$hour),]
sorted_servo = sorted_katrina[sorted_katrina$servo == 0,]
head(sorted_servo,20)




bounded_sorted_servo = sorted_servo[which(sorted_servo$hour < 35 & sorted_servo$hour > 26),]
summary(sorted_servo$hour)

bounded_sorted_sevo = tail(bounded_sorted_servo,20)

# How to Fix the 20 pumps? With a servo

summary(fit)
exp(coef(fit))

# Reason 1
#(Intercept)      backup bridgecrane       servo   trashrack   elevation       slope         age 
#92.0653701   1.2755469   0.8031164   1.3852000   0.7934489   1.0537841   0.9417365   1.0608296 

# Standard Error
summary(fit)$table[,2]

# (Intercept)      backup bridgecrane       servo   trashrack   elevation       slope         age  Log(scale) 
# 0.57055346  0.12439606  0.19787896  0.13831128  0.12435523  0.07793087  0.01754107  0.06877616  0.08584394

# Reason 2
# shape       scale      backup bridgecrane       servo   trashrack   elevation       slope         age 
# 5.1741457  14.2477735   1.0170329   1.0169381   0.9310780  40.8804719   0.9972662   1.0460434   1.2053073 

# Reason 3
# shape       scale      backup bridgecrane       servo   trashrack   elevation       slope         age 
# 3.8992925 436.7472926   1.0590973   0.9401684   0.8714111   0.9366070   1.0269645   0.9914426   0.8026416 

# Reason 4
# shape       scale      backup bridgecrane       servo   trashrack   elevation       slope         age 
# 1.8280700   0.0742323   0.8588016   0.8618851   0.7425047   0.6660426   2.3585729   0.8884372   2.3461911 

# How many of each upgrade?
sum(katrina$backup == 1)
sum(katrina$bridgecrane == 1)
sum(katrina$servo == 1)
sum(katrina$trashrack == 1)
