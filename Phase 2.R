#--------------------------#
#                          #
#   Simulation and Risk    #
#        Phase 1           #
#       Orange 11          #
#                          #
#--------------------------#

# Needed Libraries for Analysis #
library(graphics)
library(ks)
# install.packages('triangle')
library(triangle)
library("readxl")

#reading the data frame. Notice that that the rows are now 1-48 instead of 3-51 in the xlsx file. 
#df <- read_xlsx("C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Simulation and Risk Analysis\\HW1\\Analysis_Data.xlsx",2,col_names=TRUE, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), range='A3:G51')
df <- read_xlsx("C:\\Users\\jlmic\\Documents\\Simulation and Risk\\Data\\Analysis_Data.xlsx",2,col_names=TRUE, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), range='A3:G51')

#find mean and standard deviation of the 48 observations
data = c(df$"Arithmetic Return - Crude Oil"[32:47],df$"Arithmetic Return - Natural Gas"[32:47],df$"Arithmetic Return - Dry Well"[32:47] )
mu = mean(data)
sigma = sd(data)

#QQ-plot of the 48 observations 
qqnorm(data, pch = 1, frame = FALSE)
qqline(data, col = "steelblue", lwd = 2)
#hist(data, breaks=50, main='48 obs Distribution', xlab='Final Value')

#####Simulating the cost using a Normal distribution for 2006-2012 and the given triangular distributions for the other years
set.seed(112358)

# Multiple Input Probability Distributions #
P2019n <- rep(0,100000)
for(i in 1:100000){
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




#Building Kernel Density function for the 48 differences
Density.data <- density(data, bw="SJ-ste")
Density.data
Est.data<- rkde(fhat=kde(data, h=0.07935), n=1000)


#####Simulating the cost using a Kernel Density Dist for 2006-2012 and the given triangular distributions for the other years
set.seed(112358)

#getting the bandwith for the kernel desity function for 2006-2012
set.seed(112358)
P2006 <- mean(c(2238.6, 1936.2, 2664.6)) 
r <- sample(Est.data, 1000) 
Pt <- P2006*(1 + r)
Density.Pt <- density(Pt, bw="SJ-ste")
# h=104.6

# Multiple Input Probability Distributions #
P2019k <- rep(0,100000)
for(i in 1:100000){
  P2006 <- mean(c(2238.6, 1936.2, 2664.6)) 
  r <- sample(Est.data, 1)
  Pt <- P2006*(1 + r)
  Pt <- rkde(fhat=kde(Pt, h=104.6), n=1)
  
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
  P2019k[i] <- Pt
}

mean(P2019k)
sd(P2019k)
median(P2019k)

hist(P2019k, breaks=50, col = 'cornflowerblue', main='2019 Cost Distribution Using Kernel Density Estimate', xlab='2019 Cost (Thousand Dollars)')
abline(v = 2279.8 , col="darkorange3", lwd=2)
mtext("2006 Cost", at=2279.8 -600, col="darkorange3")
abline(v = median(P2019k) , col="darkorange3", lwd=2)
mtext("Median", at=median(P2019k)+600 , col="darkorange3")


#--------------------------------------#
###### PHASE 2                   #######
#--------------------------------------#

# General Approach: Build a tiny single row long for one simulation
# This row could have a cell for each term (or smaller calculation step) in the NPV formula. 
#   This will get tricky when we're calcing 15 years of a metric.
# Do the final math for NPV row-wise.
# Then ultimately change the simulation.size to the desired size (10k or whatever massive number).
# TODO Incorporate wet well/ dry well logic


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
well_type = 'wet' # checked for 'wet' and 'dry'
# Determine well logic (wet or dry)
if (well_type == 'wet') {
  completion.costs = rnorm(simulation.size, mean=390000, sd=50000)
} else {
  completion.costs = 0
}

###### Professional Overhead #######
# salary and benefit cost is best represented by a triangular distribution: 
# Need to incorporate how to determine it being different for different wells
num.years = 0
if (num.years == 0) {
  prof.overhead <- rtriangle(simulation.size, a=172000, b=279500, c=215000)*(1)
} else {
  prof.overhead <- rtriangle(simulation.size, a=172000, b=279500, c=215000)*(1+num.years)
}

# Sum for total "Year 0 Expenses" = init.costs
init.costs <- acre.costs + seismic.costs + completion.costs + prof.overhead
init.costs



#--------------------------------------#
############# Production Risk ##########
#--------------------------------------#

####### IP= Initial production rate at time zero, with Lognormal dist, HINT in HW doc
simulation.size2 = 10000
IP <- rlnorm(simulation.size2, meanlog = 6, sdlog = .28)


####### Rate of decline is Uniformly distributed b/w 15 and 32 percent
# YE = Year End | YB = Year Beginning
decline.rate = runif(simulation.size2, min=0.15, max=0.32)
# rate_YE=(1-decline.rate)*rate_YB  ### NOT SURE HOW TO APPLY THESE EQUATIONS
# oil_vol = 365*(rate_YB+rate_YE)/2

###### Impose Correlation ######
R <- matrix(data=cbind(1, 0.64, 0.64, 1), nrow=2)
U <- t(chol(R)) #choleski function

# IP standarize # Mean = 420, sd = 120
standardize_IP <- function(x){
  ip.std = (x - 420)/120
  return(ip.std)
}

destandardize_IP <- function(ip.std, x){
  ip.old = (ip.std * 120) + 420
  return(ip.old)
}

# decline.rate standardize # Mean unif = (a+b)/2 | Variance = ((b-a)^2)/12
standardize_DR <- function(x){
  dr.std = (x - 23.5)/4.907
  return(dr.std)
}

destandardize_DR <- function(dr.std, x){
  dr.old = (dr.std * 4.907) + 23.5
  return(dr.old)
}

# Actually correlate
Both.r <- cbind(standardize_IP(IP), standardize_DR(decline.rate))
SB.r <- U %*% t(Both.r) # they become correlated multiplies in correlation structure # U is cholseki
SB.r <- t(SB.r) # t() = transpose # put it back in the normal form

final.SB.r <- cbind(destandardize_IP(SB.r[,1], S.r), destandardize_DR(SB.r[,2], B.r))
final.SB.r

# Not sure how to manage this for loop for the equations
for (i in 1:15){
  if (i == 1) {
    rate_YE=(1-decline.rate)*IP  ### NOT SURE HOW TO APPLY THESE EQUATIONS
    oil_vol = 365*(IP+rate_YE)/2
  } else { 
    rate_YE=(1-decline.rate)*rate_YB  ### NOT SURE HOW TO APPLY THESE EQUATIONS
    oil_vol = 365*(rate_YB+rate_YE)/2
  }
}
# Make for loop for calculating the equations
#--------------------------------------#
############# Revenue Risk ##########
#--------------------------------------#
df2 = read_xlsx("C:\\Users\\jlmic\\Documents\\Simulation and Risk\\Data\\Analysis_Data.xlsx",1,col_names=TRUE,range='A3:D35')
df2 = df2[1:15,]

######## Price of a barrel of crude oil ########
# Make an empty dataframe for the 15 oil prices
oil = rep(0,15)
for (i in 1:nrow(df2) ) {
  oil.price <- rtriangle(simulation.size2, a=df2$`Low Oil Price`, b=df2$`High Oil Price`, c=df2$`AEO2018 Reference`)
  oil[i] = oil.price
}
View(oil) # THis makes sense now
hist(oil, breaks=50, col = 'cornflowerblue')#, main='2019 Cost Distribution Using Normal Approximation for 2006-2012', xlab='2019 Cost (Thousand Dollars)')



  
######## Net Revenue Interest ########
# annual revenues before the royalty payments are simply (Oil Price X Annual Production)
# This calculation is done per well for the entire life of the well.
nri = rnorm(simulation.size, mean=0.75, sd=0.02)
ann.rev = oil.price*ann.prod
ann.rev.diluted = ann.rev*nri


#--------------------------------------#
############# Operating Expenses ##########
#--------------------------------------#

# Operating costs per barrel 
# The expenses would be the same for every well in a given year,
# but could change from year to year with this distribution
op.costs = rnorm(simluation.size, mean=2.25, sd=0.3)

# Taxes (constant), applied after NRI
t = 0.046
ann.tax = ann.rev.diluted*t

# Net sales (AKA FNR or net revenue for the year)
net.sales = ann.rev.diluted-(ann.tax + op.costs) 

#TODO Incoporate overhead

#--------------------------------------#
############# Net Present Value Calculation ##########
#--------------------------------------#

# weighted average cost of capital (constant)
wacc <- 0.1 # Constant every year
denom <- 1 + wacc

######## Net Present Value ########
NPV = init.costs + fnr.yr1/(denom^1) + fnr.yr2/(denom^2) ...


