library(splines)
library(factoextra)
library(tidyverse)
library(knitr)
library(readr)
rm(list=ls())

options(digits=3)
options(scipen=999)

load("C:\\Users\\jlmic\\Documents\\Clustering\\Data\\final_data.Rdata")
#load("C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Clustering\\data\\final_data.Rdata")

times <- seq(1,295)/100 # Observations in 1/100th of a second
X <- bs(times,intercept=TRUE,df=60) #create a spline to 
#model the data
betas <- matrix(0,ncol=60,nrow = 6792)
###########################################################
# run a linear regression on each data set
# here I am manipulating my data you I can cluster
###########################################################
for (ii in 1:6792){
  temp <- lm(as.numeric(final_data[ii,6:300])~X-1) #-1 removes the natural intercept
  betas[ii,]  <- coefficients(temp)
}
cdata <- cbind(final_data[,1:5],betas)

#CONVERT EVERTHING TO 'numbers'
cdata$AGE <- as.numeric(cdata$AGE)
cdata$EVER_SMOKE <- as.numeric(cdata$EVER_SMOKE)
cdata$ASTHMA <- as.numeric(cdata$ASTHMA)
cdata$POVERTY_RATIO <- as.numeric(cdata$POVERTY_RATIO)

#### TASKS ####
# a) Perform a principal components analysis on columns 2 through 65. List the standard
# deviations for the first 5 components. 
pca <- princomp(cdata[,2:65])
pca$sdev[1:5]
# Comp.1   Comp.2   Comp.3   Comp.4   Comp.5 
# 45.21175 31.29010 22.37540 17.32997 13.04714 

# b) Using all pca scores compute the optimal number of clusters using kmeans using both
# "wss" and the "silhouette" method. What is the optimal number of components using each 
# method. Why may this number be different?
fviz_nbclust(pca$scores, kmeans, method = "wss")
# optimally 3-5 clusters?
fviz_nbclust(pca$scores, kmeans, method = "silhouette")
# optimally 2 clusters
# WSS only acccounts for distances within clusters. The silhoutte also takes into account distances b/w clusters. 

# c) Run the command "set.seed(12345)" and run a k-means clustering algorithm using the pca scores.

set.seed(12345)
kmeans <- kmeans(pca$scores, 4)
cdata$cluster <- kmeans$cluster

# Graph the mean spirometry

# Plot the clusters (Look at cluster 3)
ggplot(data.frame(pca$scores)) +
  geom_point(aes(x=pca$scores[,1], y=pca$scores[,2], color=as.factor(kmeans$cluster)), alpha=0.2)

clust3 <- data.frame(pca$scores[which(kmeans$cluster==3),1:64])
ggplot(clust3) +
  geom_point(aes(x=Comp.1, y=Comp.2), alpha=0.2)

# find the mean values of col 2:65
cdata_clust1 = subset(cdata, cluster==1)
cdata_clust1 = cdata_clust1[,2:65]
clust1_mean = colMeans(cdata_clust1)

cdata_clust2 = subset(cdata, cluster==2)
cdata_clust2 = cdata_clust2[,2:65]
clust2_mean = colMeans(cdata_clust2)

cdata_clust3 = subset(cdata, cluster==3)
cdata_clust3 = cdata_clust3[,2:65]
clust3_mean = colMeans(cdata_clust3)
View(clust3_mean)

cdata_clust4 = subset(cdata, cluster==4)
cdata_clust4 = cdata_clust4[,2:65]
clust4_mean = colMeans(cdata_clust4)

cdata_mean = cbind(clust1_mean, clust2_mean, clust3_mean, clust4_mean)
View(cdata_mean)
# C.b) clust3 has Poverty_ratio:higher, 1:higher, 2:lower, 3:higher
# C.b) 4:lower, 5:higher, 6:WAYlower, 7:higher, 8:higher, 10:higher
# C.b) To Physician: clust3 has higher poverty than the other groups

# D.a)
install.packages('mclust')
library(mclust)
set.seed(12345)
m_bic = mclustBIC(cdata[,10:20], modelNames='VVV', G=1:20)
plot(m_bic)
summary(m_bic)

# Yes, this number(15) is WAY different than the above clusters of 4.
# This is different, because it uses a different selection criterion (BIC)
# D.b)
set.seed(12345)
mc_clust = Mclust(cdata[,10:20], modelNames='VVV', G=6)
mc_class = mc_clust$classification
cdata$MC_clust = mc_class
View(cdata)



###### Comparing Clusters #######
#looking at counts/proportions
table(cdata$cluster)
prop.table(table(cdata$cluster))

#just looking at a few means
cdata %>%
  group_by(cluster) %>%
  summarise(mean.age=mean(AGE), mean.smoke=mean(EVER_SMOKE), mean.ASTHMA=mean(ASTHMA), mean.pov=mean(POVERTY_RATIO))  

#looking at all means
means <- cdata %>%
  group_by(cluster) %>%
  summarise_all("mean")

#plotting a variable by cluster
ggplot(cdata)+
  geom_density(aes(x=AGE, color=as.factor(cluster)))

#plotting a small handful of the variables
long = gather(cdata, id.vars= colnames(cdata[1:16]))
ggplot(long) +
  geom_density(aes(x=value, color=as.factor(cluster))) +
  facet_wrap(~key, scales='free')
summary(group_by(cdata, cluster))

# a) Compute the graph of mean spirometry for the 4 clusters (all 4 on one graph). 
spiro1 <- as.numeric(means[1,7:66]) #-1 removes the natural intercept
spiro2 <- as.numeric(means[2,7:66])
spiro3 <- as.numeric(means[3,7:66])
spiro4 <- as.numeric(means[4,7:66])

plot(times,X%*%spiro1,ylim=c(0,100),
     type='l',lwd=2,col=1,
     xlab="Time", ylab="Spirometry ML")
lines(times,X%*%spiro2,lwd=2,col=2)
lines(times,X%*%spiro3,lwd=2,col=3)
lines(times,X%*%spiro4,lwd=2,col=4)
legend("topright", c("1", "2", "3", "4"), col=c(1,2,3,4), lwd=2)

sfun1 <- splinefun(times,X%*%spiro1)
sfun2 <- splinefun(times,X%*%spiro2)
sfun3 <- splinefun(times,X%*%spiro3)
sfun4 <- splinefun(times,X%*%spiro4)
integrate(sfun1,min(times),max(times))
integrate(sfun2,min(times),max(times))
integrate(sfun3,min(times),max(times))
integrate(sfun4,min(times),max(times))
# group 3 has the greatest lung capacity, closely followed by group 4 

trial <- pca$x %*% t(pca$rotations) # not working

plot(times,X%*%beta1,ylim=c(0,100),type='l')

# b) Look at cluster 3. Plot the graph of this cluster and give the mean values (on the original scale) for columns 2-65. 
# What makes this cluster different from # the other clusters?  
# Describe this cluster so a physician can better understand important characteristics of these clusters. 
# A: Cluster 3 has higher poverty ratio than other clusters. 

# c) Looking at clusters 1,2, and 4 which clusters has the largest lung capacity?
#   which one has the least lung capacity? Describe these three groups in terms of 
# the curves as well as the additional variables that are available in the data 
# frame cdata. Provide figures with your descriptions. 

# NOW look at the data using MCLUST type 'set.seed(12345)': 
#   
#   a) Using mclustbic() and columns 10-20 of cdata (NOT the principal component values).
# estimate the optimal number of  cluster components using the BIC and only with 
# modelNames='VVV' and G = 1:20. Show a graph of the estimate. Is this number different than 
# the ones given above, why? (This will take a while). 
# b) Now using G = 6 and modelNames='VVV' and the same columns, provide a graph of each cluster's mean curve (USING ALL OF THE DATA COLUMNS). 
# Put all plots on one graph. 
# 
# c) Using all of the data compare cluster 4 with cluster 3 from the kmeans() cluster what can you 	
# 
# say about the similarities between these two clusters, what are the differences? Which estimate 
# 
# makes more sense? What do you trust more? What are the benefits of using mixture modeling over
# 
# kmeans, what are the issues?
# 
# d) Are there any clusters similar to the k-means clusters? Describe each cluster.  
library(mclust)
set.seed(12345)
mc_clust = Mclust(cdata[,10:20], modelNames='VVV', G=6)
mc_class = mc_clust$classification
cdata$MC_clust = mc_class

means_mcclust <- cdata %>%
  group_by(MC_clust) %>%
  summarise_all("mean")
View(means_mcclust)

spiro1 <- as.numeric(means_mcclust[1,7:66]) #-1 removes the natural intercept
spiro2 <- as.numeric(means_mcclust[2,7:66])
spiro3 <- as.numeric(means_mcclust[3,7:66])
spiro4 <- as.numeric(means_mcclust[4,7:66])
spiro5 <- as.numeric(means_mcclust[5,7:66])
spiro6 <- as.numeric(means_mcclust[6,7:66])

plot(times,X%*%spiro1,ylim=c(0,100),
     type='l',lwd=2,col=1,
     xlab="Time", ylab="Spirometry ML")
lines(times,X%*%spiro2,lwd=2,col=2)
lines(times,X%*%spiro3,lwd=2,col=3)
lines(times,X%*%spiro4,lwd=2,col=4)
lines(times,X%*%spiro5,lwd=2,col=5)
lines(times,X%*%spiro6,lwd=2,col=6)
legend("topright", c("1", "2", "3", "4", "5", "6"), col=c(1,2,3,4,5,6), lwd=2)

sfun1 <- splinefun(times,X%*%spiro1)
sfun2 <- splinefun(times,X%*%spiro2)
sfun3 <- splinefun(times,X%*%spiro3)
sfun4 <- splinefun(times,X%*%spiro4)
integrate(sfun1,min(times),max(times))
integrate(sfun2,min(times),max(times))
integrate(sfun3,min(times),max(times))
integrate(sfun4,min(times),max(times))

View(cdata)
