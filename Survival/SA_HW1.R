###########################
## Survival Analysis HW1 ##
##      Orange 11        ##
###########################
rm(list=ls())
install.packages('survminer')
library(dplyr)
library(survival)
library(survminer)
katrina = read.csv(file='C:\\Users\\jlmic\\Documents\\Survival Analysis\\Data\\katrina.csv')
#katrina = read.csv(file='C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Survival\\data\\katrina.csv')

View(katrina)
dim(katrina) # 770 x 60

####### SUMMARY STATS ######

# Percentage of survived pumps
sum(katrina$survive == 1) # 316/770 survived

# Percentage of each failure
sum(katrina$reason == 1) # 115/770
sum(katrina$reason == 2) # 112/770
sum(katrina$reason == 3) # 111/170
sum(katrina$reason == 4) # 116/770

sum(316,115,112,111,116) # matches 770

summary(katrina)

View(katrina)

katrina$fail = 1 - katrina$survive
  
# Min = 1
# 1st Quartile = 27
# Median = 45
# Mean = 38.52
# 3rd Quartile = 48
# Max = 48

###### Survival Curves #######


# need fail column for hazard function later
katrina$reason <- as.factor(katrina$reason) #important for merge below
summary(katrina$fail)
sum(katrina$fail == 1) # 454/770 failed, math adds up

failure_codes <- as.data.frame(cbind(reason=c(0, 1, 2, 3, 4), reason_w=c(NA,"flood","motor","surge","jammed")))

katrina <- left_join(katrina, failure_codes, by='reason')

unique(katrina$reason_w)

# Plot survival curve Jacob's code
katrina_fit = survfit(Surv(katrina$hour, katrina$fail == 1) ~ 1, data=katrina)
katrina_fit
summary(katrina_fit)
plot(katrina_fit) #TODO ggplot this

# Stratify
katrina.nofail = filter(katrina, reason == 0)
katrina.flood = filter(katrina, reason == 1)
katrina.motor = filter(katrina, reason == 2)
katrina.surge = filter(katrina, reason == 3)
katrina.jam = filter(katrina, reason == 4)

# Survival Analysis of Stratified
#nofail_fit = survfit(Surv(katrina.nofail$hour, katrina.nofail$fail == 1) ~ 1, data=katrina.nofail) # NO NEED
flood_fit = survfit(Surv(katrina.flood$hour, katrina.flood$fail == 1) ~ 1, data=katrina.flood)
motor_fit = survfit(Surv(katrina.motor$hour, katrina.motor$fail == 1) ~ 1, data=katrina.motor)
surge_fit = survfit(Surv(katrina.surge$hour, katrina.surge$fail == 1) ~ 1, data=katrina.surge)
jam_fit = survfit(Surv(katrina.jam$hour, katrina.jam$fail == 1) ~ 1, data=katrina.jam)

# Plot them!
plot(flood_fit)
plot(motor_fit)
plot(surge_fit)
plot(jam_fit)

length(katrina$reason)

###### Compare Curves ######
katrina_reason <- survfit(Surv(time = hour, event = fail) ~ reason, data = katrina[katrina$reason != 0,])
ggsurvplot(katrina_reason, conf.int = TRUE, palette = "grey")
## DONT BIN THEM AS WATER AND MECHANICAL FAILURES

### log-rank tests ###
compare1 = survdiff(Surv(time = hour, event = fail) ~ reason, rho = 0, data = katrina[katrina$reason != 0,])
compare2 = survminer::pairwise_survdiff(Surv(time = hour, event = fail) ~ reason, rho = 0, data = katrina[katrina$reason != 0,])

### Hazard functions ###
library('muhaz')

katrina$hour2 <- ifelse(katrina$hour == 48 & katrina$fail == 0, 49, katrina$hour)

katrina_haz <- with(katrina, kphaz.fit(hour2, fail))

kphaz.plot(katrina_haz, main = "hazard function")

ggsurvplot(katrina_fit, fun = "cumhaz", palette = "grey")
