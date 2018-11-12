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
library(triangle)
library("readxl")

#reading the data frame. Notice that that the rows are now 1-48 instead of 3-51 in the xlsx file. 
#df <- read_xlsx("C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Simulation and Risk Analysis\\HW1\\Analysis_Data.xlsx",2,col_names=TRUE, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), range='A3:G51')
df <- read_xlsx("C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Simulation and Risk\\data\\Analysis_Data.xlsx",2,col_names=TRUE, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), range='A3:G51')

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

simulation.size <- 1

#--------------------------------------#
###### Year Zero & Flat Expenses #######
#--------------------------------------#

###### Leased acres per well Costs #######
# Leased acres per well
price.p.acre <- 9601
acre.costs <- rnorm(simulation.size, mean=600, sd=50)*price.p.sec

###### Seismic Costs #######
price.p.sec <- 43000 
seismic.costs <- rnorm(simulation.size, mean=3, sd=0.35)*price.p.sec

###### Completion Costs #######
completion.costs <- rnorm(simulation.size, mean=390000, sd=50000)



###### Professional Overhead #######
# salary and benefit cost is best represented by a triangular distribution, 
overhead <- rtriangle(simulation.size, a=172000, b=279500, c=215000)
# Constant across the lifetime of a well, but potentially different for different wells. 
# These costs are incurred during Year 0 as well for drilling, but stop after Year 0 if the well is dry.
# TODO incorporate this cost into operating expenses.

init.costs <- completion.costs+seismic.costs+acre.costs+overhead

#--------------------------------------#
############# Production Risk ##########
#--------------------------------------#

# IP= Initial production rate at time zero, with Lognormal dist, HINT in HW doc
IP <- rlnorm(simulation.size, meanlog = 6, sdlog = 0.28)

# rate of decline is Uniformly distributed b/w 15 and 32 percent
decline.rate = runif(n=number, min=0.15, max=0.32)
rate=(1-decline.rate)*rate.prior
vol = 365*(rate.prior+rate)/2
# TODO imposed a correlation coefficient of 0.64 between the IP and the decline rate
# TODO Build array for next 15(?) years of production rates

#--------------------------------------#
############# Revenue Risk ##########
#--------------------------------------#

######## Price of a barrel of crude oil ########
# TODO Map Analysis_Data.xlsx data to lo, hi, expected variables below for 15yrs (2019-2033)
oil.price <- rtriangle(simulation.size, a=lo, b=hi, c=expected)
ann.rev = oil.price*ann.prod

######## Net Revenue Interest ########
# annual revenues before the royalty payments are simply (Oil Price X Annual Production)
# This calculation is done per well for the entire life of the well.
nri = rnorm(simulation.size, mean=0.75, sd=0.02)
ann.rev.diluted = ann.rev*nri


#--------------------------------------#
############# Operating Expenses ##########
#--------------------------------------#

# TODO Build for operating expenses for every year
# TODO ?Incoporate logic for a dry well?

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


