#--------------------------#
#                          #
#   Simulation and Risk    #
#        Phase 3           #
#       Orange 11          #
#                          #
#--------------------------#
#install.packages('truncnorm')
#install.packages('Rlab')
library(dplyr)
library(truncnorm)
library(Rlab)
library(graphics)
library(quantmod)
library(TTR)
library(ks)
library(scales)

set.seed(112233)
#dist for hydrocarbons
hydrocarbons = rtruncnorm(100000, a=0, b=1, mean = .9, sd = .05)
hist(hydrocarbons)

#dist for reservoir
reservoir = rtruncnorm(100000, a=0, b=1, mean = .8, sd = .1)
hist(reservoir)

#dist of the probability of success
prob_of_sucess = hydrocarbons*reservoir
hist(prob_of_sucess)

#dist of the number of proportion of wet wells
num_wet_wells = rep(0,10000)
prop_wet_wells = rep(0,10000)
for(j in 1:10000){
  #calculates the number of wells that are planned to be drilled
  planned_wells = runif(1, 10,30)
  
  #bernouli dist- 1 means the well was wet, 0 means dry
  sucess=rep(0,planned_wells)
  for(i in 1:planned_wells){
    sucess[i] = rbern(1, prob_of_sucess[i])
  }
  
  #counts the number of wet wells out of our planned wells
  results =data.frame(table(sucess))
  num_wet_wells[j] = results[2,2]
  prop_wet_wells[j] = num_wet_wells[j]/planned_wells
}
hist(prop_wet_wells, breaks=50)
results
sucess
prop_wet_wells
med = median(prop_wet_wells)
# Calculate 5% VaR
VaR.percentile = .05
VaR <- quantile(prop_wet_wells, VaR.percentile, na.rm=TRUE)


# Calcuate 5% ES (CVaR)
# Mean of values below the VaR
bottom5 = prop_wet_wells[prop_wet_wells < VaR]
ES = mean(bottom5, na.rm=TRUE)

# Print Var and ES
print(paste('VaR:',VaR,'ES:',ES))

#Histograms
hist(prop_wet_wells, breaks=50, col = 'cornflowerblue', main='Distribution of the Proportion of Wet Wells', xlab='Proportion')
abline(v = med , col="darkorange3", lwd=2)
abline(v = VaR , col="darkorange3", lwd=2)
abline(v = ES , col="darkorange3", lwd=2)
mtext("Median", at=median(prop_wet_wells)+400 , col="darkorange3")
mtext("VaR", at=VaR , col="darkorange3")
mtext("ES (CVaR)", at=ES , col="darkorange3")
