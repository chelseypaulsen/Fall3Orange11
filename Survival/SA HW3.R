###########################
## Survival Analysis HW1 ##
##       Phase 3         ##
##      Orange 11        ##
###########################

rm(list=ls()) #clears existing variables/functions from envi

library(tidyverse)
library(survival)
library(survminer)
library(flexsurv)
library(dplyr)
library(visreg)


#katrina = read.csv(file='C:\\Users\\jlmic\\Documents\\Survival Analysis\\Data\\katrina.csv')
#katrina = read.csv(file='C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Survival\\data\\katrina.csv')
katrina = read.csv(file='C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Survival Analysis\\2017SA Data\\katrina.csv')
#View(katrina)

#censuring everything but motor and surge failures
Surv(time = katrina$hour, event = katrina$reason %in% c(2,3))


#fit aft models -- all aft models are really bad!
#weibull - plot looks REALLY BAD
fit <- survreg(Surv(hour, reason %in% c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                 slope + age, data = katrina, dist = "weibull")
summary(fit)
exp(coef(fit))

fit_wb <- flexsurvreg(Surv(hour, reason %in% c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                        slope + age, data = katrina, dist = "weibull")

plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Weibull Distribution Cumulative Hazard Plot", xlim=c(0, 50), ylim=c(0,0.25))

#exponential - Looks REALLY BAD
fit_exp <- flexsurvreg(Surv(hour, reason %in% c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                         slope + age, data = katrina, dist = "exponential")

plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
     xlab = "hour", ylab = "cumulative hazard",
     main = "exponential distribution")

#lognormal - Looks REALLY BAD
fit_lnorm <- flexsurvreg(Surv(hour, reason %in% c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                           slope + age, data = katrina, dist = "lognormal")

plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "lognormal distribution")

# log-logistic -Looks REALLY BAD
fit_llogis <- flexsurvreg(Surv(hour, reason %in% c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                            slope + age, data = katrina, dist = "llogis")

plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard",
     main = "log-logistic distribution", xmax=20)

##### Conclusion DON'T use an AFT Model #####

fit_cox<- coxph(Surv(hour, reason %in% c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
           slope + age, data = katrina)

print(fit_cox)
summary(fit_cox)
exp(coef(fit_cox))
#     backup bridgecrane       servo   trashrack   elevation       slope         age 
#0.9108555   0.9675719   1.3217306   0.3440834   0.9305280   0.9046099   1.1271265 

# Standard Error
summary(fit_cox)$coefficients[,3]

#Looks Better than AFT
ggsurvplot(survfit(fit_cox), data = katrina, legend = "none", break.y.by = 0.1,
           xlab = "week", ylab = "survival probability")

### concordance
concordance(fit_cox)

#n= 770 
#Concordance= 0.6665 se= 0.01877
#discordant concordant     tied.x     tied.y    tied.xy 
#67487      33767         21       2048          2

####### Check Conditions #########

### plot residuals
# create data frame with the event, time, martingale residuals, deviance
# residuals, and ID variable
resids <- data.frame(event = fit_cox$y[,dim(fit_cox$y)[2]],
                     time = fit_cox$y[,dim(fit_cox$y)[2] - 1],
                     res_m = residuals(fit_cox, type = "martingale"),
                     res_d = residuals(fit_cox, type = "deviance"),
                     ID = 1:length(residuals(fit_cox)))

# martingale vs. time
ggplot(resids, aes(x = time, y = res_m, color = factor(event))) +
  geom_point() +
  labs(x = "hour", y = "martingale residuals", color = "event") +
  scale_color_manual(values = c("purple", "orange"))
# deviance vs. time
ggplot(resids, aes(x = time, y = res_d, color = factor(event))) +
  geom_point() +
  labs(x = "hour", y = "deviance residuals", color = "event") +
  scale_color_manual(values = c("purple", "orange"))
# deviance vs. ID, to see which one is the largest
ggplot(resids, aes(x = ID, y = res_d, color = factor(event))) +
  geom_point() +
  labs(x = "ID", y = "deviance residuals", color = "event") +
  scale_color_manual(values = c("purple", "orange"))
# or you can just find the observation corresponding to the max deviance res.
which.max(resids$res_d) # it's observation 101

### dfbetas
ggcoxdiagnostics(fit_cox, type = "dfbetas")

### checking linearity
# age
visreg(fit_cox, 'age', xlab = "age", ylab = "partial residuals", gg = TRUE,
       band = FALSE) +
  geom_smooth(col = "red", fill = "red") + theme_bw()
# slope
visreg(fit_cox, "slope", xlab = "slope", ylab = "partial residuals",
       gg = TRUE, band = FALSE) +
  geom_smooth(col = "red", fill = "red") + theme_bw()
# elevation
visreg(fit_cox, "elevation", xlab = "elevation", ylab = "partial residuals", gg = TRUE,
       band = FALSE) +
  geom_smooth(col = "red", fill = "red") + theme_bw()


