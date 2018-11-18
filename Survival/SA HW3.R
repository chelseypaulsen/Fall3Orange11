###########################
## Survival Analysis HW1 ##
##       Phase 3         ##
##      Orange 11        ##
###########################

rm(list=ls()) #clears existing variables/functions from envi

library(tidyverse)
library(survival)
library(survminer)
library(dplyr)
library(visreg)
library(flexsurv)


#katrina = read.csv(file='C:\\Users\\jlmic\\Documents\\Survival Analysis\\Data\\katrina.csv')
katrina = read.csv(file='C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Survival\\data\\katrina.csv')
#katrina = read.csv(file='C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Survival Analysis\\2017SA Data\\katrina.csv')
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
     xlab = "Hour", ylab = "Cumulative Hazard", 
     main = "Weibull Distribution Cumulative Hazard Plot", xlim=c(0, 50), ylim=c(0,0.25))

#exponential - Looks REALLY BAD
fit_exp <- flexsurvreg(Surv(hour, reason %in% c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                         slope + age, data = katrina, dist = "exponential")

plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
     xlab = "Hour", ylab = "Cumulative Hazard",
     main = "Exponential Distribution")

#lognormal - Looks REALLY BAD
fit_lnorm <- flexsurvreg(Surv(hour, reason %in% c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                           slope + age, data = katrina, dist = "lognormal")

plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Lognormal Distribution")

# log-logistic -Looks REALLY BAD
fit_llogis <- flexsurvreg(Surv(hour, reason %in% c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                            slope + age, data = katrina, dist = "llogis")

plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard",
     main = "Log-Logistic Distribution", xmax=20)

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
           xlab = "Time (hrs)", ylab = "Survival Probability", censor.size = .5)


# plotting for consistency across time

fit.s.backup <- fit_cox<- coxph(Surv(hour, reason %in% c(2,3)) ~ 
                                  strata(backup) + bridgecrane + servo + trashrack + 
                                  elevation + slope + age, data = katrina)

ggsurvplot(survfit(fit.s.backup), data = katrina, fun='cloglog',
           break.y.by = 0.1, censor.size = .5,
           xlab = "log(Time) (log hrs)", ylab = "log(-logSi(t))",
           legend.labs = c("Backup", "No Backup"))

fit.s.bridgecrane <- fit_cox<- coxph(Surv(hour, reason %in% c(2,3)) ~ 
                                  backup + strata(bridgecrane) + servo + trashrack + 
                                  elevation + slope + age, data = katrina)

ggsurvplot(survfit(fit.s.bridgecrane), data = katrina, fun='cloglog',
           break.y.by = 0.1, censor.size = .5,
           xlab = "log(Time) (log hrs)", ylab = "log(-logSi(t))",
           legend.labs = c("Bridgecrane", "No Bridgecrane"))

fit.s.servo <- fit_cox<- coxph(Surv(hour, reason %in% c(2,3)) ~ 
                                 backup + bridgecrane + strata(servo) + trashrack + 
                                 elevation + slope + age, data = katrina)

ggsurvplot(survfit(fit.s.servo), data = katrina, fun='cloglog',
           break.y.by = 0.1, censor.size = .5,
           xlab = "log(Time) (log hrs)", ylab = "log(-logSi(t))",
           legend.labs = c("No Servo", "Servo"))

fit.s.trashrack <- fit_cox<- coxph(Surv(hour, reason %in% c(2,3)) ~ 
                                  backup + bridgecrane + servo + strata(trashrack) + 
                                  elevation + slope + age, data = katrina)

ggsurvplot(survfit(fit.s.trashrack), data = katrina, fun='cloglog',
           break.y.by = 0.1, censor.size = .5,
           xlab = "log(Time) (log hrs)", ylab = "log(-logSi(t))",
           legend.labs = c("No Trashrack", "Trashrack"))

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
which.max(resids$res_d) # it's observation 555

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
dddd# Def not linear for elevation. Debatable for slope.

###### Bullet #3 #########
#creating id for all rows
katrina$id <- seq(1:nrow(katrina))
katrina$over.run <- 0
samplerow <- katrina[400,]

# UNUSED CODE FOR 12-hr EVAL
# 
# twelve.check <- function(row){
#   #function to check for twelve 1's in a row in the H1:H48 columns
#   hrs.running <- 0
#   overrun=list() 
#   
#   for (hr in 1:48){
#     # previous <- row[1,paste0("h",hr-1,sep="")]
#     current <- row[1,paste0("h",hr,sep="")]
#     
#     if (is.na(current)) {
#       row$end = hr
#       overrun[hr]=NA
#     } else if(current == 1){
#       hrs.running <- hrs.running + 1
#       
#       # nested logic to determine if 
#       if(hrs.running >= 12){
#         overrun[hr]=1
#       }else if (hrs.running < 12){
#         overrun[hr]=0
#       }
#       
#     } else if(current == 0){
#       hrs.running <- 0
#       overrun[hr]=0
#     }
#     
#     
#     print(paste("hr: ", hr,"|   runnning: ", current, "|   overrun: ", overrun[hr]))
#     
#   }
#   # returning 49 so it's easy to distinguish the non-events
#   return(overrun)
#   
# }
# 
# twelve.check(samplerow)
# 
# if (target!=49){
#   rhold <- samplerow[paste0("h", 1:target, sep="")]
#   rtail <- samplerow[paste0("h", (target+1):48, sep="")]
#   restart <- c(as.numeric(rtail), rep(NA, target))
# }
# 
# duplicate.row <- function(df, row){
#   df[nrow(df) + 1,] <- row
#   
# }

overrun = read.csv(file='C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Survival\\data\\over_worked_pumps.csv')

fit_cox2<- coxph(Surv(overrun$Start, overrun$Stop, event = overrun$reason %in% c(2,3)) ~ 
                   backup + bridgecrane + servo + trashrack + elevation +
                  slope + age + Over_Worked, data = overrun)


str(fit_cox2)
summary(fit_cox2)
exp(coef(fit_cox2))

#creating new data, so average values of binary variables aren't used
mean.elev <- mean(katrina$elevation)
mean.slope <- mean(katrina$slope)
mean.age <- mean(katrina$age)
newdata2 <- data.frame(Over_Worked = c("0", "1"), backup = 0, bridgecrane = 0, servo = 0,
                       trashrack = 0, elevation = mean.elev, slope = mean.slope, age = mean.age)
ggsurvplot(survfit(fit_cox2, newdata2), data=newdata2, 
  censor.size = .5,
  legend.title = "Run Time",  legend.labs = c("<12 hrs", ">=12 hrs"),
  xlab = "Time (hrs)", ylab="Survival Probability")
#they look very similar
