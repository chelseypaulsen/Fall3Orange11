############################
# Machine Learning Project #
#      Orange Team 11      #
############################
rm(list=ls())

library(tidyverse)
library(caret)

options(digits=4)
load("C:/Users/jlmic/Documents/Machine Learning/Data/MLProjectData.RData")
#df_ML = read.csv('C:\\Users\\jlmic\\Documents\\Machine Learning\\Data\\MLProjectData.csv')
df_ML = read.csv('C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Machine Learning\\data\\MLProjectData.csv')
#df_ML= read.csv('C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Machine Learning\\Project\\MLProjectData.csv')
dim(df_ML)

# column groups
cat.col = c('cat1','cat2','cat3','cat4','cat5','cat6','cat7','cat8','cat9','cat10','cat11','cat12','cat13','cat14','cat15',
            'cat16','cat17','cat18','cat19','cat20','cat21','cat22','cat23','cat24','cat25','cat26')
num.col <- paste('num',seq(1:59), sep='')
log.col <- paste('cat',seq(3:26), sep='')

# Split 'dataset
set.seed(8) # The greatest number there ever was
intrain<-createDataPartition(y=df_ML$target,p=0.7,list=FALSE)

df_ML_train<-df_ML[intrain,]
df_ML_validate<-df_ML[-intrain,]

dim(df_ML_train)
dim(df_ML_validate)

############################################################################
#Exploration#
############################################################################
library(corrplot)

summary(df_ML) # no missing values to worry about

# Basic plots for exploring if we want them
# ggplot(df_ML_train) +
#   geom_histogram(aes(x=target, fill= cat13))
# 
# ggplot(df_ML_train) +
#   geom_point(aes(x=num29, y=target, color= cat1), alpha=0.1)
# 
# ggplot(df_ML_train) +
#   geom_histogram(aes(x=target)) +
#   facet_wrap(~ cat1) 


#Correlation matrix
#corrplot(cor(df_ML[c(num.col,"target")]))
# num2 and num8-11 are strongly correlated
# num35-42 are strongly correlated with one another
# num44-54 are strongly correlated with one another
# num35-54 have stron negative correlations with num44=54
# essentially no correlation b/w any numeric variable and the target... WTF? Is the target just random numbers?
#corrplot.mixed(cor(df_ML[c(num.col[10:20],"target")])) # weird negative correlation b/w num15-17 and num18-20

# # Frequency tables
# attach(df_ML)
# MLtable <- table(cat1, cat2)
# prop.table(MLtable)
# rm(MLtable)
# # almost even distribution across categorical columns
# 
# # "ID", "target"
# # Histograms by variable, definitely some numeric columns with the same distributions
# df_ML %>%
#   keep(is.numeric) %>% 
#   gather() %>% 
#   ggplot(data=., aes(value)) +
#     facet_wrap(~ key, scales = "free") +
#     geom_histogram()
# 
# df_ML %>%
#   select(which(sapply(.,class)=="numeric"),target,ID) %>% 
#   gather() %>%
#   ggplot(data=id, aes(x=ID, y=value)) +
#     facet_wrap(~ key, scales = "free") +
#     geom_point()


############################################################################
#Random Forest#
############################################################################

##################################
# Build Random Forest #
##################################
library(randomForest)
library(ModelMetrics)

# Tune for best mtry
# accuracy=vector()
# mtry=seq(2,86,by=2)
# i=1
# for(m in mtry){
#   print(i)
#   rf = randomForest(target ~ ., data=df_ML_train,mtry=m, ntree=50)
#   test_pred =  predict(rf,df_ML_validate)
#   #accuracy[i] =  sum(test_pred!=df_ML_train$target)/nrow(df_ML_train)
#   abserror = abs(df_ML_validate$target - test_pred)
#   accuracy[i] = mean(abserror)
#   i=i+1
# }
# plot(mtry, accuracy)
# 
# # Tune for ntree
# accuracy=vector()
# ntree=seq(25,200,by=25)
# i=1
# for(n in ntree){
#   print(i)
#   rf = randomForest(target ~ ., data=df_ML_train,mtry=22, ntree=n)
#   test_pred =  predict(rf,df_ML_validate)
#   #accuracy[i] =  sum(test_pred!=df_ML_train$target)/nrow(df_ML_train)
#   abserror = abs(df_ML_validate$target - test_pred)
#   accuracy[i] = mean(abserror)
#   i=i+1
# }
# plot(ntree, accuracy)

# CHOSEN MODEL mtry=24, ntree=175 - MAE = 0.39/0.95

rf_ML = randomForest(target ~ ., data=df_ML_train, importance=TRUE, mtry=22, ntree=175)

# RF on Training Data #
rf_train_pred = predict(rf_ML, df_ML_train)

rf_train_abserror = abs(df_ML_train$target - rf_train_pred)

MAE = mean(rf_train_abserror)
print(MAE)

# RF on Validate Data #
rf_valid_pred = predict(rf_ML, df_ML_validate)

rf_valid_abserror = abs(df_ML_validate$target - rf_valid_pred)

MAE = mean(rf_valid_abserror)
print(MAE)

############################################################################
############################################################################
#XGBoost#
############################################################################
############################################################################
library('Matrix')
#install.packages('xgboost')
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

############# Copied from above ###############################################################################

df_XG_train<-df_ML[intrain,]
df_XG_validate<-df_ML[-intrain,]

##############################################################################################################

#Changing all categorical vars to factors
df_XG_train[cat.col] = lapply(df_XG_train[cat.col], factor)
df_XG_validate[cat.col] = lapply(df_XG_validate[cat.col], factor)

sparse_train = sparse.model.matrix(target ~ . -target , data=df_XG_train)
sparse_valid = sparse.model.matrix(target  ~ . -target , data=df_XG_validate)
train_label = as.numeric(df_XG_train$target)[df_XG_train$target] 

# # Tune for eta
# accuracy_xgb=vector()
# eta=seq(.05,1,by=.05)
# i=1
# for (e in eta) {
#   print(i)
#   xgb <- xgboost(data = sparse_train,
#                      label = train_label,
#                      eta = e,
#                      max_depth = 20,
#                      gamma = 0,
#                      nround=100,
#                      subsample = 0.75,
#                      colsample_bytree = 0.75,
#                      objective = "reg:linear",
#                      nthread = 3,
#                      eval_metric = 'rmse',
#                      verbose =0)
#   ptrain = predict(xgb, sparse_train)
#   pvalid = predict(xgb, sparse_valid)
#   abserror = abs(df_XG_validate$target - pvalid)
#   accuracy_xgb[i] = mean(abserror)
#   i=i+1
# }
# plot(eta, accuracy_xgb)
# 
# # Tune for max_depth
# accuracy_xgb=vector()
# depth = seq(1,15, by=1)
# i=1
# for (d in depth) {
#   print(i)
#   xgb <- xgboost(data = sparse_train,
#                  label = train_label,
#                  eta = 0.5,
#                  max_depth = d,
#                  gamma = 0,
#                  nround=100,
#                  subsample = 0.75,
#                  colsample_bytree = 0.75,
#                  objective = "reg:linear",
#                  nthread = 3,
#                  eval_metric = 'rmse',
#                  verbose =0)
#   ptrain = predict(xgb, sparse_train)
#   pvalid = predict(xgb, sparse_valid)
#   abserror = abs(df_XG_validate$target - pvalid)
#   accuracy_xgb[i] = mean(abserror)
#   i=i+1
# }
# plot(depth, accuracy_xgb)
# 
# # Tune Gamma
# accuracy_xgb=vector()
# gam = seq(0,1, by=.1)
# i=1
# for (g in gam) {
#   print(i)
#   xgb <- xgboost(data = sparse_train,
#                  label = train_label,
#                  eta = 0.5,
#                  max_depth = 5,
#                  gamma = g,
#                  nround=100,
#                  subsample = 0.75,
#                  colsample_bytree = 0.75,
#                  objective = "reg:linear",
#                  nthread = 3,
#                  eval_metric = 'rmse',
#                  verbose =0)
#   ptrain = predict(xgb, sparse_train)
#   pvalid = predict(xgb, sparse_valid)
#   abserror = abs(df_XG_validate$target - pvalid)
#   accuracy_xgb[i] = mean(abserror)
#   i=i+1
# }
# plot(gam, accuracy_xgb)

##############################################
# Final XGB Model - MAE = 1.01, 0.94
##############################################
xgb <- xgboost(data = sparse_train,
               label = train_label,
               eta = 0.05,
               max_depth = 5,
               gamma = 0.1,
               nround=100,
               subsample = 0.75,
               colsample_bytree = 0.75,
               objective = "reg:linear",
               nthread = 3,
               eval_metric = 'rmse',
               verbose =0)

# Predict
ptrain = predict(xgb, sparse_train)
pvalid = predict(xgb, sparse_valid)

# MAE

XG_train_abserror = abs(df_XG_train$target - ptrain)

MAE = mean(XG_train_abserror)
print(MAE)

XG_valid_abserror = abs(df_XG_validate$target - pvalid)

MAE = mean(XG_valid_abserror)
print(MAE)



############################################################################
############################################################################
# Support Vector Machine (SVM) #
############################################################################
############################################################################

df_SVM_train<-df_ML[intrain,]
df_SVM_validate<-df_ML[-intrain,]

###########################################################################
############       STANDARDIZE THE CONTINUOUS VARIABLES        ############
###########################################################################
###########################################################################
SVM_train = df_SVM_train[,1:59]
SVM_trainscale = scale(SVM_train, center=T, scale=T)
SVM_trainscale2 = cbind(SVM_trainscale, df_SVM_train[,60:86])

mean_trainscale = colMeans(SVM_train)
sd_trainscale = apply(SVM_train, 2, sd)
  
SVM_validscale = scale(df_SVM_validate[,1:59], center=mean_trainscale, scale=sd_trainscale)
SVM_validscale2 = cbind(SVM_validscale, df_SVM_validate[,60:86])

#install.packages("e1071")
library(e1071)
###########################################################################
###########################################################################
############     TUNE THE SVM HYPERPARAMETERS GAMMA AND C - Takes an eternity     ############
###########################################################################
###########################################################################
# TuneSVM = tune.svm(target~., data=SVM_trainscale, kernel='radial', gamma=2^(-5:-.5), cost=2^(-1:3))
# summary(TuneSVM)
# plot(TuneSVM)

# - Detailed performance results:
#   gamma cost error dispersion
# 1  0.031  0.5   2.2       0.36
# 2  0.062  0.5   2.2       0.36
# 3  0.125  0.5   2.1       0.35
# 4  0.250  0.5   2.1       0.35
# 5  0.500  0.5   2.1       0.35
# 6  0.031  1.0   2.2       0.37
# 7  0.062  1.0   2.2       0.36
# 8  0.125  1.0   2.1       0.35
# 9  0.250  1.0   2.1       0.35
# 10 0.500  1.0   2.1       0.35
# 11 0.031  2.0   2.3       0.38
# 12 0.062  2.0   2.3       0.37
# 13 0.125  2.0   2.2       0.35
# 14 0.250  2.0   2.1       0.35
# 15 0.500  2.0   2.1       0.35
# 16 0.031  4.0   2.4       0.39
# 17 0.062  4.0   2.3       0.38
# 18 0.125  4.0   2.2       0.36
# 19 0.250  4.0   2.1       0.35
# 20 0.500  4.0   2.1       0.36
# 21 0.031  8.0   2.5       0.40
# 22 0.062  8.0   2.3       0.38
# 23 0.125  8.0   2.2       0.36
# 24 0.250  8.0   2.1       0.35
# 25 0.500  8.0   2.1       0.36

############################################################################
############################################################################
## USE THE BEST HYPERPARAMETERS TO BUILD FINAL MODEL AND TEST PERFORMANCE ##
############################################################################
############################################################################

#### Final SVM Model - MAE = 0.34/0.93 ####
bestgamma=0.5
bestc=1
svm1=svm(target~., data=SVM_trainscale2, kernel='radial', gamma=bestgamma, cost=bestc)

# Predict
pred=predict(svm1,SVM_trainscale2)
pred2=predict(svm1,SVM_validscale2)

# MAE

SVM_train_abserror = abs(df_SVM_train$target - pred)

MAE = mean(SVM_train_abserror)
print(MAE)

SVM_valid_abserror = abs(df_SVM_validate$target - pred2)

MAE = mean(SVM_valid_abserror)
print(MAE)


############################################################################
## Neural Network ## - This is not working
############################################################################
#install.packages('neuralnet')
#install.packages('fastDummies')

library(neuralnet)
library(fastDummies)
library(Hmisc)
library(beepr)
# Make normalized neural network dataset
df_NN = as.data.frame(scale(df_ML[,c(1:59,86)], center=T, scale=T)) # mean and sd scaling
df_NN = cbind(df_NN, df_ML[,60:85])
df_NN = dummy_cols(df_NN)
df_NN = df_NN[,c(1:60,87:103)]

# clustering variables for easier NN computation with more hidden layers (not yet implemented)
NN_vc <- varclus(as.matrix(df_NN[,c(1:59)]))
is.numeric(as.matrix(df_NN[,c(1:59)]))
plot(NN_vc)
groups <- cutree(NN_vc$hclust, 16)

#showing clustered variables
plot(NN_vc, cex = 0.6)
rect.hclust(NN_vc$hclust, k = 16, border=2:4)
groups$clusters

set.seed(8) # The greatest number there ever was
intrain <- createDataPartition(y=df_ML$target,p=0.7,list=FALSE)
df_NN_train<-df_NN[intrain,]
df_NN_validate<-df_NN[-intrain,]
dim(df_NN_train)
f = as.formula(target~num1+num2+num3+num4+num5+num6+num7+num8+num9+num10+num11+num12+num13+num14+num15+num16+num17+num18+num19+num20+
                 num21+num22+num23+num24+num25+num26+num27+num28+num29+num30+num31+num32+num33+num34+num35+num36+num37+num38+num39+num40+
                 num41+num42+num43+num44+num45+num46+num47+num48+num49+num50+num51+num52+num53+num54+num55+num56+num57+num58+num59+
                 cat1_E+cat1_A+cat1_C+cat1_D+cat1_B+cat2_D+cat2_G+cat2_L+cat2_B+cat2_F+cat2_H+cat2_C+cat2_K+cat2_E+cat2_I+cat2_A+cat2_J)

nnet1 = neuralnet(f, data=df_NN_train, hidden=1, threshold = 0.01, stepmax = 3e+05) #run time will vary based on hidden layers 
beep(sound = 3)
nnet1$weights
results1 = compute(nnet1, df_NN_validate[,!(names(df_NN_validate)=="target")]) # This line is breaking
nnet1Pred=results1$net.result


# mean absolute error
mean(abs(df_NN_validate$target - nnet1Pred)) 
# 0.681 w/ 1 hidden and threshold at 0.01
# 0.713 w/ 3 hidden and threshold at 0.05 (ran 12 min)
# No convergences w/ 2 or 3 hidden, threshold=0.01, and stepmax= 3e+05 (ran 20 min for each)

save(nnet1, df_NN_train, df_NN_validate, df_NN, nnet1Pred, file="nnet.Rdata")

############################################################################
############################################################################
# What about just the mean of the target? MAE = 0.97/0.93
############################################################################
############################################################################
mean = mean(df_ML$target)
df_MEAN_train<-df_ML[intrain,]
df_MEAN_validate<-df_ML[-intrain,]

# MAE
df_MEAN_train$Prediction = mean
df_MEAN_train$abserror = abs(df_MEAN_train$target - df_MEAN_train$Prediction)

MAE = mean(df_MEAN_train$abserror)
print(MAE)

df_MEAN_validate$Prediction = mean
df_MEAN_validate$abserror = abs(df_MEAN_validate$target - df_MEAN_validate$Prediction)

MAE = mean(df_MEAN_validate$abserror)
print(MAE)

############################################################################
############################################################################
# Ensemble #
############################################################################
############################################################################
# Training
ensemble_train = cbind(rf_train_pred, ptrain, pred)
ensemble_train = as.data.frame(ensemble_train)
names(ensemble_train) = c("RF", "XG", "SVM")
ensemble_train$mean = rowMeans(ensemble_train)

ensemble_train$abserror = abs(df_ML_train$target - ensemble_train$mean)

MAE = mean(ensemble_train$abserror)
print(MAE)

# Validation
ensemble_valid = cbind(rf_valid_pred, pvalid, pred2)
ensemble_valid = as.data.frame(ensemble_valid)
names(ensemble_valid) = c("RF", "XG", "SVM")
ensemble_valid$mean = rowMeans(ensemble_valid)

ensemble_valid$abserror = abs(df_ML_validate$target - ensemble_valid$mean)

MAE = mean(ensemble_valid$abserror)
print(MAE)

############################################################################
############################################################################
# Predict on test #
############################################################################
############################################################################
testData = test.data
# testData_csv = read.csv(file = 'C:\\Users\\jlmic\\Documents\\Machine Learning\\Data\\testData.csv')

testData_scale = scale(testData[,1:59], center=mean_trainscale, scale=sd_trainscale) 
testData_scale2 = cbind(testData_scale, testData[,60:85])

# unique(testData$cat2) # Checked all unique values of all categorical variables and there are none in testdata that aren't in validation
# unique(df_ML_train$cat2)

dim(SVM_validscale2)
dim(testData_scale2)

# SVM
bestgamma=0.5
bestc=1
svm1=svm(target~., data=SVM_trainscale2, kernel='radial', gamma=bestgamma, cost=bestc)
test_svm=predict(svm1,testData_scale2) 

# XGB
testData_sparse = sparse.model.matrix( ~ ., data=testData)

test_xgb = predict(xgb, testData_sparse) # 

# RF
test_rf = predict(rf_ML, testData)

# Combine and ensemeble
final_test = cbind(test_svm, test_xgb, test_rf)
final_test = as.data.frame(final_test)
final_test$mean = rowMeans(final_test)
View(final_test)

# Make final submission predictions
Row = seq(1,77,by=1)
Prediction = final_test$mean
Orange11 = cbind(Row, Prediction)
View(Orange11)
write.csv(Orange11, file = "Orange11.csv")
