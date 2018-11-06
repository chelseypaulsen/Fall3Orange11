###########################
## Survival Analysis HW1 ##
##      Orange 11        ##
###########################
rm(list=ls()) #clears existing variables/functions from envi

library(tidyverse)
library(survival)
#install.packages("survminer")
library(survminer)

katrina = read.csv(file='C:\\Users\\jlmic\\Documents\\Survival Analysis\\Data\\katrina.csv')
#katrina = read.csv(file='C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Survival\\data\\katrina.csv')

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
median(katrina$hour[katrina$reason!=0]) # median fail hour is 34th hour  
median(katrina$hour[katrina$reason==1]) # 26th hr if fail by flood
median(katrina$hour[katrina$reason==2]) # 45th hr motor
median(katrina$hour[katrina$reason==3]) # 42nd hr surge
median(katrina$hour[katrina$reason==4]) # 25th hr jamming

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

failure_codes <- as.data.frame(cbind(reason=c(0, 1, 2, 3, 4), reason_w=c("no failure","flood","motor","surge","jammed")))

katrina <- left_join(katrina, failure_codes, by='reason')

unique(katrina$reason_w)


# Plot survival curve Jacob's code
katrina_fit = survfit(Surv(katrina$hour, katrina$fail == 1) ~ 1, data=katrina)

summary(katrina_fit)
# plot(katrina_fit)
ggsurvplot(katrina_fit, data = katrina, conf.int = FALSE, legend="none",
           title="Pump Survival Curve",
           xlab="Time (hours)",
           ggtheme = theme_bw())


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
katrina_grp_fit = survfit(Surv(hour, fail) ~ reason, data=katrina[katrina$reason != 0,])
summary(katrina_grp_fit)

ggsurvplot(katrina_grp_fit, conf.int = FALSE, 
           legend="right", title="Pump Survival Curves",
           legend.labs=c("flood","motor","surge","jammed"),
           legend.title="Reason for Failure",
           xlab="Time (hours)",
           ggtheme = theme_bw())
## DONT BIN THEM AS WATER AND MECHANICAL FAILURES


### log-rank tests ###

#unweighted
compare1a = survdiff(Surv(time = hour, event = fail) ~ reason, rho = 0, data = katrina[katrina$reason != 0,])
compare1a
#weighted
compare1b = survdiff(Surv(time = hour, event = fail) ~ reason, rho = 1, data = katrina[katrina$reason != 0,])
compare1b
#default BH adjustment (whatever that is...)
compare2a = pairwise_survdiff(Surv(time = hour, event = fail) ~ reason, rho = 0, data = katrina[katrina$reason != 0,])
compare2a
#Bonferoni adjustment
compare2b = pairwise_survdiff(Surv(time = hour, event = fail) ~ reason, rho = 0, data = katrina[katrina$reason != 0,], p.adjust.method="bonferroni")
compare2b
# Results: all comparisons are significantly different, max is <0.003


### Hazard functions ###
library('muhaz')

katrina$hour2 <- ifelse(katrina$hour == 48 & katrina$fail == 0, 49, katrina$hour)

katrina_haz <- with(katrina, kphaz.fit(hour2, fail))
katrina_grp_haz <- with(katrina[katrina$reason != 0,], kphaz.fit(hour2, fail, strata = as.factor(reason_w)))
summary(katrina_grp_haz$strata) # Ugh... where did Group 5 come from?!
summary(katrina$reason)
help(kphaz.fit)

# hazard Plot
kphaz.plot(katrina_haz, main = "Hazard Function")
#Alternate Hazard Plot
ggplot(as.data.frame(katrina_haz))+
  geom_line(aes(x=time, y=haz), color="#F8766D", size=1)+
  xlab("Time (hrs)") + ylab("Hazard") + ggtitle("Pump Hazard Function")+
  theme_bw()

write.csv(katrina_haz, file="hazardplot.csv")

#Stratified Hazard Plot, but it's not very informative
katrina_haz_1 <- with(katrina[katrina$reason == 1,], kphaz.fit(hour2, fail))
katrina_haz_2 <- with(katrina[katrina$reason == 2,], kphaz.fit(hour2, fail))
katrina_haz_3 <- with(katrina[katrina$reason == 3,], kphaz.fit(hour2, fail))
katrina_haz_4 <- with(katrina[katrina$reason == 4,], kphaz.fit(hour2, fail))

ggplot()+
  geom_line(data=as.data.frame(katrina_haz_1), aes(x=time, y=haz))+
  geom_line(data=as.data.frame(katrina_haz_2), aes(x=time, y=haz))+
  geom_line(data=as.data.frame(katrina_haz_3), aes(x=time, y=haz))+
  geom_line(data=as.data.frame(katrina_haz_4), aes(x=time, y=haz))+
  xlab("Time (hrs)") + ylab("Hazard") + ggtitle("Pump Hazard Functions")+
  scale_color_manual(labels = c("flood", "motor", "jammed", "surge"))
  theme_bw()

#Grouped Hazard Plot
kphaz.plot(katrina_grp_haz, main = "grouped hazard function") 
#Alternate Grouped Hazard Plot
#TODO Resolve error here for grouped hazard plot... because where did Group 5 come from?!
ggplot(as.data.frame(katrina_grp_haz))+
  geom_line(aes(x=time, y=haz, color=factor(strata)), size=1)+
  xlab("Time (hrs)") + ylab("Hazard") + ggtitle("Pump Hazard Function")+
  theme_bw()

ggsurvplot(katrina_grp_fit, fun = "cumhaz")
