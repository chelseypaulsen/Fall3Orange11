library(splines)
library(factoextra)
library(tidyverse)
library(knitr)
library(readr)
rm(list=ls())

options(digits=3)
options(scipen=999)

load("C:\\Users\\jlmic\\Documents\\Clustering\\Data\\final_data.Rdata")

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

ggplot(data.frame(pca$scores)) +
  geom_point(aes(x=pca$scores[,1], y=pca$scores[,2], color=as.factor(kmeans$cluster)), alpha=0.2)

clust3 <- data.frame(pca$scores[which(kmeans$cluster==3),1:64])
ggplot(clust3) +
  geom_point(aes(x=Comp.1, y=Comp.2), alpha=0.2)


colMeans(cdata[which(cdata$cluster==1),])
colMeans(cdata[which(cdata$cluster==2),])
colMeans(cdata[which(cdata$cluster==3),])
colMeans(cdata[which(cdata$cluster==4),])

cdata %>%
  group_by(cluster) %>%
  summarise(mean.age=mean(AGE), mean.smoke=mean(EVER_SMOKE), mean.ASTHMA=mean(ASTHMA), mean.pov=mean(POVERTY_RATIO))  


summary(group_by(cdata, cluster))

pca$loadings[,1:2]

trial <- pca$x %*% t(pca$rotations) # not working

