#--------------------------#
#                          #
#   Simulation and Risk    #
#        Phase 1           #
#       Orange 11          #
#                          #
#--------------------------#
############################
rm(list=ls())

# Needed Libraries for Analysis #
library(graphics)
library(ks)
library("readxl")
library(triangle)
library(beepr) # for the beep sound
library(scales) # not very important, just for dollar function


#reading the data frame. Notice that that the rows are now 1-48 instead of 3-51 in the xlsx file. 
# df <- read_xlsx("C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Simulation and Risk Analysis\\HW1\\Analysis_Data.xlsx",2,col_names=TRUE, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), range='A3:G51')
df <- read_xlsx("C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Simulation and Risk\\data\\Analysis_Data2.xlsx",2,col_names=TRUE, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), range='A1:G49')

#find mean and standard deviation of the 48 observations
data = c(df$"Arithmetic Return - Crude Oil"[32:47],df$"Arithmetic Return - Natural Gas"[32:47],df$"Arithmetic Return - Dry Well"[32:47] )
mu = mean(data)
sigma = sd(data)

#####Simulating the cost using a Normal distribution for 2006-2012 and the given triangular distributions for the other years
set.seed(112358)
#sim.size2 = 10000
sim.size2 = 1E6

# Multiple Input Probability Distributions #
P2019n <- rep(0,sim.size2)
for(i in 1:sim.size2){
  P2006 <- mean(c(2238.6, 1936.2, 2664.6))
  r <- rnorm(n=1, mean=mu, sd=sigma)
  Pt <- P2006*(1 + r)
  
  for(j in 1:5){ 
    r <- rnorm(n=1, mean=mu, sd=sigma)
    Pt <- Pt*(1+r)
    for(j in 1:3){ 
      r=rtriangle(1, -0.22, -0.07, -0.0917)
      Pt <- Pt*(1+r)
      for(j in 1:3){ 
        r=rtriangle(1, 0.02, 0.06, 0.05)
        Pt <- Pt*(1+r)
      }
    }
  }
  P2019n[i] <- Pt
}

mean(P2019n)
sd(P2019n)
median(P2019n)
min(P2019n)
max(P2019n)

hist(P2019n, breaks=50, col = 'cornflowerblue', main='2019 Cost Distribution Using Normal Approximation for 2006-2012', xlab='2019 Cost (Thousand Dollars)')
abline(v = 2279.8 , col="darkorange3", lwd=2)
mtext("2006 Cost", at=2279.8 -400, col="darkorange3")
abline(v = median(P2019n) , col="darkorange3", lwd=2)
mtext("Median", at=median(P2019n)+400 , col="darkorange3")

#removed kernal dist and qq plots from the final code

beep() #indicate when phase is done running so it doesn't take forever
beep()
beep()

############################
#--------------------------#
#                          #
#   Simulation and Risk    #
#        Phase 2           #
#       Orange 11          #
#                          #
#--------------------------#
############################
# General Approach: Build a tiny single row long for one simulation
# This row could have a cell for each term (or smaller calculation step) in the NPV formula. 
#   This will get tricky when we're calcing 15 years of a metric.
# Do the final math for NPV row-wise.
# Then ultimately change the simulation.size to the desired size (10k or whatever massive number).


#####################################
##### OBJECTIVES ####################
##### 1. Cost of a singe dry well ###
##### 2. NPV of a single wet well ### NPV = Net Present Value
#####################################

simulation.size <- 1


#--------------------------------------#
###### Year Zero & Flat Expenses #######
#--------------------------------------#

###### Leased acres per well Costs #######
price.p.acre <- 960
acre.costs <- rnorm(simulation.size, mean=600, sd=50)*price.p.acre

###### Seismic Costs per well Costs ######
price.p.sec <- 43000 
seismic.costs <- rnorm(simulation.size, mean=3, sd=0.35)*price.p.sec

###### Completion Costs #######
# completion.costs <- rnorm(simulation.size, mean=390000, sd=50000)
completion.costs = rnorm(simulation.size, mean=390000, sd=50000)

###### Professional Overhead #######
# salary and benefit cost is best represented by a triangular distribution: 
# Need to incorporate how to determine it being different for different wells

prof.overhead <- rtriangle(simulation.size, a=172000, b=279500, c=215000)


# Drilling Costs # No Clue
drilling = P2019n*1000

# Sum for total "Year 0 Expenses" = init.costs
init.costs <- acre.costs + seismic.costs + completion.costs + prof.overhead + drilling
init.costs

# Build Dry well Distribution
dry_well = rep(0,sim.size2)
for (i in 1:sim.size2){
  acre.costs <- rnorm(simulation.size, mean=600, sd=50)*price.p.acre
  seismic.costs <- rnorm(simulation.size, mean=3, sd=0.35)*price.p.sec
  prof.overhead <- rtriangle(simulation.size, a=172000, b=279500, c=215000)
  drilling = P2019n[i]*1000
  dry_well[i] = acre.costs + seismic.costs  + prof.overhead + drilling
}
hist(dry_well, breaks=50)
median(dry_well)

hist(dry_well, breaks=50, col = 'cornflowerblue', main='Distribution of Cost for a Dry Well', xlab='Cost (Dollars)')

abline(v = median(dry_well) , col="darkorange3", lwd=2)
mtext("Median", at=median(dry_well)+400 , col="darkorange3")

# Costs Histogram needs to include acre, seismic, overhead, drilling distributions


#--------------------------------------#
############# Production Risk ##########
#--------------------------------------#

####### IP= Initial production rate at time zero, with Lognormal dist, HINT in HW doc

IP <- rlnorm(sim.size2, meanlog = 6, sdlog = .28)

####### Rate of decline is Uniformly distributed b/w 15 and 32 percent
# YE = Year End | YB = Year Beginning
decline.rate = runif(sim.size2, min=0.15, max=0.32)
mean(decline.rate)
###### Impose Correlation ######
R <- matrix(data=cbind(1, 0.64, 0.64, 1), nrow=2)
U <- t(chol(R)) #choleski function

# IP standarize # Mean = 420, sd = 120
standardize_IP <- function(x){
  ip.std = (x - mean(x))/sd(x)
  return(ip.std)
}

destandardize_IP <- function(ip.std, x){
  ip.old = (ip.std * sd(x)) + mean(x)
  return(ip.old)
}

# decline.rate standardize # Mean unif = (a+b)/2 | Variance = ((b-a)^2)/12
standardize_DR <- function(x){
  dr.std = (x - mean(x))/sd(x)
  return(dr.std)
}

destandardize_DR <- function(dr.std, x){
  dr.old = (dr.std * sd(x)) + mean(x)
  return(dr.old)
}

# Actually correlate
Both.r <- cbind(standardize_DR(decline.rate), standardize_IP(IP))
SB.r <- U %*% t(Both.r) # they become correlated multiplies in correlation structure # U is cholseki
SB.r <- t(SB.r) # t() = transpose # put it back in the normal form

# Lead with the uniform distribution

final.SB.r <- cbind(destandardize_DR(SB.r[,1], S.r), destandardize_IP(SB.r[,2], B.r))
head(final.SB.r)
final.SB.r[1:15,1:2]
dim(final.SB.r)

#### Going to re-try making the choleski
standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

######################################################################
#changed this part from prior assignment


IP <- rlnorm(sim.size2, meanlog = 6, sdlog = .28)
decline.rate = runif(sim.size2, min=0.15, max=0.32)
Both.r <- cbind(standardize(decline.rate), standardize(IP))
SB.r <- U %*% t(Both.r) # they become correlated multiplies in correlation structure # U is cholseki
SB.r <- t(SB.r) # t() = transpose # put it back in the normal form
# @#@#@#@
final.DR_IP <- cbind(destandardize(SB.r[,1], decline.rate), destandardize(SB.r[,2], IP))

df_oil_vol = matrix(0, 15, sim.size2)
for (j in 1:sim.size2){
  for (i in 1:15){
    if (i == 1) {
      rate_YB = final.DR_IP[j,2]
      rate_YE=(1-final.DR_IP[j,1])*rate_YB
      oil_vol = 365*(rate_YB+rate_YE)/2
    } else {
      rate_YB= rate_YE
      rate_YE=(1-final.DR_IP[j,1])*rate_YB
      oil_vol = 365*(rate_YB+rate_YE)/2
    }
    df_oil_vol[i,j] = oil_vol
  }
}
df_oil_vol[1:15, 1:30]
# End choleski retry

######################################################################


ann.prod = df_oil_vol
ann.prod[1:15,1:10]
dim(ann.prod)
test = ann.prod[15,]
hist(test)
median(test)


# Make for loop for calculating the equations
#--------------------------------------#
############# Revenue Risk ##########
#--------------------------------------#
#df2 = read_xlsx("C:\\Users\\jlmic\\Documents\\Simulation and Risk\\Data\\Analysis_Data.xlsx",1,col_names=TRUE,range='A3:D35')
#df2 = read_xlsx("C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Simulation and Risk Analysis\\HW1\\Analysis_Data.xlsx",1,col_names=TRUE,range='A3:D35')
df2 = read_xlsx("C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Simulation and Risk\\data\\Analysis_Data2.xlsx",1,col_names=TRUE,range='A1:D33')
df2 = df2[1:15,]


######## Price of a barrel of crude oil ########
# Make an empty dataframe for the 15 years and oil prices

oil = matrix(, 15, sim.size2)
for (j in 1:sim.size2){
  for (i in 1:nrow(df2) ) {
    oil.price <- rtriangle(simulation.size, a=df2$`Low Oil Price`[i], b=df2$`High Oil Price`[i], c=df2$`AEO2018 Reference`[i])
    oil[i,j] = oil.price
  }
}
final_year = colSums(oil)
hist(final_year, breaks=50, col = 'cornflowerblue', main='Oil Price Normal Approximation for 2019-2033')#, xlab='2019 Cost (Thousand Dollars)')


######## Net Revenue Interest ########
# annual revenues before the royalty payments are simply (Oil Price X Annual Production)
# This calculation is done per well for the entire life of the well.
NRI = matrix(, 15, sim.size2)
for (i in 1:sim.size2) {
  NRI[,i] = rnorm(simulation.size, mean=0.75, sd=0.02)
}

Annual_Rev = oil*ann.prod # Annual Revenue as a distribution
dim(Annual_Rev)
Annual_Rev[1:15,1:15]
Dil.Rev = Annual_Rev*NRI # Diluted Revenue as a distribution
Dil.Rev[1:15,1:10]
# Operating costs per barrel 
# The expenses would be the same for every well in a given year,
# but could change from year to year with this distribution
# assign the distribution of operational costs

OperationCost = matrix(0,15,sim.size2) 
#Didnt have the outside loop
for (j in 1:sim.size2) {
  for (i in 1:15){
    op.costs = rnorm(n=1, mean=2.25, sd=0.3)
    OperationCost[i,j] = ann.prod[i,j]*op.costs
  }
}
dim(OperationCost)
OperationCost[1:15,1:10]
hist(OperationCost[15,])

# Calculating the Professional Overhead # change for 10000 And DO NOT COMPOUND
PO = matrix(0,15,sim.size2)
for(j in 1:sim.size2){
  prof.overhead = rtriangle(n=1, a=172000, b=279500, c=215000)
  for(i in 0:15){
    PO[i,j] <- prof.overhead
  }
}
PO[1:15,1:10]

# Calculated FNR
FNR = ((Dil.Rev - OperationCost - PO)*(1-.046)) #0.046 = Severance Tax
dim(FNR)  
PO[1:15,1:10]
Dil.Rev[1:15,1:10]
OperationCost[1:15,1:10]
FNR[1:15,1:10]
# #--------------------------------------#
# ############# Net Present Value Calculation ##########
# #--------------------------------------#

# need to fix each cost to be a distribution
df_init_costs = rep(0,10)
for (j in 1:10) {
  acre.costs <- rnorm(simulation.size, mean=600, sd=50)*price.p.acre
  seismic.costs <- rnorm(simulation.size, mean=3, sd=0.35)*price.p.sec
  completion.costs = rnorm(simulation.size, mean=390000, sd=50000)
  drilling = P2019k[runif(simulation.size,1,sim.size2)]*1000
  print(drilling)
  df_init_costs[j] = sum(acre.costs, seismic.costs, completion.costs, drilling)
}

df_init_costs[1:10]

# weighted average cost of capital (constant)


### NET PRESENT VALUE ###
df_total = matrix(0,15,sim.size2)

# define WACC function
WACC = NULL
for (i in 1:15) {
  WACC = rbind(WACC, (1+.1)^i)
}


# apply WACC factor onto the FNR
for (i in 1:nrow(FNR)) {
  df_total[i,] = FNR[i,]/WACC[i]
}

# sum all years together for final NPV
year15 = colSums(df_total)
NPV = year15 - df_init_costs
NPV
median(NPV)
mean(NPV)

# Make Histogram
hist(NPV, breaks=50, col = 'cornflowerblue', main='Distribution of Simulated Predicted Net Present Values for years 2019 to 2033', xlab='Net Present Value (Dollars)')

abline(v = median(NPV) , col="darkorange3", lwd=2)
mtext("Median", at=median(NPV)+400 , col="darkorange3")

beep()
beep()
beep()

############################
#--------------------------#
#                          #
#   Simulation and Risk    #
#        Phase 3           #
#       Orange 11          #
#                          #
#--------------------------#
############################
library(truncnorm)
library(Rlab)

#set seed
set.seed(112358)

#dist for hydrocarbons
hydrocarbons = rtruncnorm(sim.size2, a=0, b=1, mean = .9, sd = .05)
hist(hydrocarbons, col = 'cornflowerblue', main='Histogram of Probability of Hydrocarbons Being Present', xlab='Probability')
abline(v =   0.8985832 , col="darkorange3", lwd=2)
mtext("Median = 0.899", at=0.899 , col="darkorange3")

median(hydrocarbons)

#dist for reservoir
reservoir = rtruncnorm(sim.size2, a=0, b=1, mean = .8, sd = .1)
hist(reservoir, col = 'cornflowerblue', main='Histogram of Probability of Reservoir Being Developed in Rock Formation', xlab='Probability')
abline(v =  0.7983274 , col="darkorange3", lwd=2)
mtext("Median = 0.798", at=0.7983274 , col="darkorange3")

median(reservoir)

#dist of the probability of sucess
prob_of_sucess = hydrocarbons*reservoir
hist(prob_of_sucess, col = 'cornflowerblue', main='Histogram of Probability of a Well Being Wet', xlab='Probability')
abline(v =  0.713916 , col="darkorange3", lwd=2)
mtext("Median = 0.714", at=0.713916 , col="darkorange3")

median(prob_of_sucess)

# removed proportion calculations and VaR/ES/Histograms from previous assignment

beep()
beep()
beep()

############################
#--------------------------#
#                          #
#   Simulation and Risk    #
#        Phase 4           #
#       Orange 11          #
#                          #
#--------------------------#
############################

######## Bullet 1 ########
# Simulate the distribution of Net Present Value from the entire project (all of the wells).

#dist of the proportion of wet wells
NPV_total = rep(0,sim.size2)
for(j in 1:sim.size2){
  #calculates the number of wells that are planned to be drilled
  planned_wells = runif(1, 10,30)
  
  #calculate cost of each wet/dry well in planned wells
  NPV_well=rep(0,planned_wells)
  for(i in 1:planned_wells){
    sucess = rbern(1, prob_of_sucess[i])#bernouli dist- 1 means the well was wet, 0 means dry
    if (sucess ==1){
      NPV_well[i]= sample(NPV,1) # taking a random number from NPV
    } 
    else{
      NPV_well[i]= -1*sample(dry_well,1) #taking a random number from dry_well and making it negative
    }
    NPV_total[j]=sum(NPV_well)#adding all the costs/profits of wet wells and dry wells together
  }
}

median(NPV_total)
med_NPV <- median(NPV_total)
min(NPV_total)
max(NPV_total)
mean(NPV_total)
summary(NPV_total)


beep()
beep()
beep()


######## Bullet 2 ########
#??? Calculate the expected return from the scenario, as well as measures of risk - such as Value at Risk and Expected Shortfall.
#used median for expected return

# Calculate 5% VaR
VaR.percentile = .05
VaR <- quantile(NPV_total, VaR.percentile, na.rm=TRUE)
VaR

# Calcuate 5% ES (CVaR)
# Mean of values below the VaR
bottom5 = NPV_total[NPV_total < VaR]
ES = mean(bottom5, na.rm=TRUE)
# Print Var and ES
print(paste('VaR:',VaR,'ES:',ES))

# Histogram
hist(NPV_total, col = 'cornflowerblue',
     main='Histogram of the Total NPV for the Project', xlab='NPV (Dollars)')
abline(v =  med_NPV , col="darkorange3", lwd=2)
mtext("Median = 222M", at=mean(NPV_total) , col="darkorange3")
abline(v =  VaR , col="darkorange3", lwd=2)
mtext("VaR", at=VaR+8000000 , col="darkorange3")
abline(v =  ES , col="darkorange3", lwd=2)
mtext("ES", at=ES-8000000, col="darkorange3")

# Automated histogram of full distribution (incase anything changes), w/ simplified axis
hist(NPV_total/1000000, breaks=40, col = 'cornflowerblue',
     main='Distribution of Simulated Total NPVs', xlab='NPV (USD, Millions)')
abline(v =  med_NPV/1000000 , col="darkorange3", lwd=2)
mtext(paste("Median = $",round(med_NPV/1000000,1),"M", sep=""), at=med_NPV/1000000 , col="darkorange3")
abline(v =  VaR/1000000 , col="darkorange3", lwd=2)
mtext("VaR", at=VaR/1000000+8, col="darkorange3")
abline(v =  ES/1000000 , col="darkorange3", lwd=2)
mtext("ES", at=ES/1000000-8, col="darkorange3")


############ Confidence Intervals of VaR and ES ###########
n.bootstraps <- 10000
sample.size <- 10000

VaR.boot <- rep(0,n.bootstraps) #empty vectors of zeros to record VaR
ES.boot <- rep(0,n.bootstraps)

# this is the bootstrap
for(i in 1:n.bootstraps){
  bootstrap.sample <- sample(NPV_total, size=sample.size) # a sample (w/o replacement) from the bigger simulation
  VaR.boot[i] <- quantile(bootstrap.sample, VaR.percentile, na.rm=TRUE)
  ES.boot[i] <- mean(bootstrap.sample[bootstrap.sample < VaR.boot[i]], na.rm=TRUE)
}

hist(VaR.boot) # a sneak peak of the 95% conf interval.
# calculating the 95% confidence interval
VaR.boot.U <- quantile(VaR.boot, 0.975, na.rm=TRUE)
VaR.boot.L <- quantile(VaR.boot, 0.025, na.rm=TRUE)
ES.boot.U <- quantile(ES.boot, 0.975, na.rm=TRUE)
ES.boot.L <- quantile(ES.boot, 0.025, na.rm=TRUE)
dollar(quantile(VaR.boot, c(0.025, 0.975), na.rm=TRUE))
dollar(quantile(ES.boot, c(0.025, 0.975), na.rm=TRUE))

Var_Label <- paste("VaR=$",round(VaR/1000000,1),"M", sep="")
ES_Label <- paste("ES=$",round(ES/1000000,1),"M", sep="")

# Histogram of tail (with confidence intervals)
NPV_tail <- NPV_total[which(NPV_total<VaR)] # filtering to anything less than VaR 
hist(NPV_tail/1000000, breaks=40, xlim=c(min(NPV_tail)/1000000, 1.25*max(NPV_tail)/1000000),
     col = 'cornflowerblue',
     main='Distribution of the Lowest 5% \n of Simulated NPV', xlab='NPV (USD, Millions)')
# VaR with confidence intervals
abline(v =  VaR/1000000 , col="darkorange3", lwd=2)
mtext(Var_Label, at=VaR/1000000+2, col="darkorange3")
abline(v = VaR.boot.L/1000000, col="darkorange", lwd=2, lty="dashed")
abline(v = VaR.boot.U/1000000, col="darkorange", lwd=2, lty="dashed")
#ES with confidence intervals
abline(v =  ES/1000000 , col="darkorange3", lwd=2)
mtext(ES_Label, at=ES/1000000-2, col="darkorange3")
abline(v = ES.boot.L/1000000, col="darkorange", lwd=2, lty="dashed")
abline(v = ES.boot.U/1000000, col="darkorange", lwd=2, lty="dashed")
legend("topleft", c("Mean Value", "95% Confidence Interval"), 
       col=c("darkorange3", "darkorange"), lty=c("solid", "dashed"), 
       y.intersp=.6, cex = 0.8, bty = "n") #y.intersp tightens legend vertically, cex shrinks font and bty turns off box outline


######## Bullet 3 ########
# ??? Make a recommendation on whether the company should invest in the scenario described based on your above numbers.

#I mean they are pretty much always making money so, yeah they should invest. 


###### EXTRA STUFF ########
sum(NPV_total<0)
#82 times out of 1M they lose money


