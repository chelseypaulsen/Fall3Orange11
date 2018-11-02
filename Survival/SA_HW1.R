###########################
## Survival Analysis HW1 ##
##      Orange 11        ##
###########################

library(dplyr)
library(survival)

katrina = read.csv(file='C:\\Users\\jlmic\\Documents\\Survival Analysis\\Data\\katrina.csv')
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

katrina$fail = katrina$survive == 0
View(katrina)

#if (katrina$survive == 0){
#  katrina$fail = 1
#} else {
#  katrina$fail = 0
#}
#View(katrina)

katrina$fail = 1 - katrina$survive
  
# Min = 1
# 1st Quartile = 27
# Median = 45
# Mean = 38.52
# 3rd Quartile = 48
# Max = 48

###### Survival Curves #######

katrina_fit = survfit(Surv(katrina$hour, katrina$fail == 1) ~ 1, data=katrina)
katrina_fit
summary(katrina_fit)
plot(katrina_fit)

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
