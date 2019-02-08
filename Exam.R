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

pca$loadings[,1:2]

trial <- pca$x %*% t(pca$rotations) # not working

