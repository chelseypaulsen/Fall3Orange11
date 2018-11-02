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
# Min = 1
# 1st Quartile = 27
# Median = 45
# Mean = 38.52
# 3rd Quartile = 48
# Max = 48

###### Survival Curves #######

katrina_fit <- 


