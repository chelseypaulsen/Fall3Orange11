############################
# Machine Learning Project #
#      Orange Team 11      #
############################
rm(list=ls())

library(tidyverse)
library(caret)
options(digits=2)

df_ML = read.csv('C:\\Users\\jlmic\\Documents\\Machine Learning\\Data\\MLProjectData.csv')
#df_ML = read.csv('C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Machine Learning\\data\\MLProjectData.csv')
#df_ML= read.csv('C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Machine Learning\\Project\\MLProjectData.csv')

# Be careful to not include this ID column in analysis
#df_ML$ID <- seq(1,dim(df_ML)[1])

# column groups
cat.col = c('cat1','cat2','cat3','cat4','cat5','cat6','cat7','cat8','cat9','cat10','cat11','cat12','cat13','cat14','cat15',
            'cat16','cat17','cat18','cat19','cat20','cat21','cat22','cat23','cat24','cat25','cat26')
num.col <- paste('num',seq(1:59), sep='')
log.col <- paste('cat',seq(3:26), sep='')

# Split 'Training' dataset
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

# accuracy=vector()
# mtry=seq(1,16)
# i=1
# for(m in mtry){
#   print(i)
#   rf = randomForest(target ~ ., data=df_ML_train,mtry=m, ntree=50)
#   test_pred =  predict(rf,df_ML_train)
#   accuracy[i] =  sum(test_pred!=df_ML_train$target)/nrow(df_ML_train)
#   i=i+1
# }
# plot(mtry, accuracy)

# all mtrys are the same?

# CHOSEN MODEL
rf_ML = randomForest(target ~ ., data=df_ML_train, importance=TRUE, ntree=100)

##################################
# RF on Training Data #
##################################

rf_train_pred = predict(rf_ML, df_ML_train)

df_ML_train$Prediction = rf_train_pred
df_ML_train$abserror = abs(df_ML_train$target - df_ML_train$Prediction)

MAE = mean(df_ML_train$abserror)
print(MAE)

##################################
# RF on Validate Data #
##################################

rf_valid_pred = predict(rf_ML, df_ML_validate)

df_ML_validate$Prediction = rf_valid_pred
df_ML_validate$abserror = abs(df_ML_validate$target - df_ML_validate$Prediction)

MAE = mean(df_ML_validate$abserror)
print(MAE)
# MAE2 = mae(df_ML_validate$target, df_ML_validate$Prediction)
##################################
# RF on test Data for Submission #
##################################

df_ML_test = select(df_ML_test, -X)
View(df_ML_test)


############################################################################
#XGBoost#
############################################################################
library('Matrix')
install.packages('xgboost')
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

############# Copied from above ###############################################################################

df_XG <- df_ML
df_XG_train <- df_ML_train
df_XG_validate <- df_ML_validate
View(df_XG_train)
dim(df_XG_train)
##############################################################################################################



#All vars need to be numeric (or factors)

#Changing all categorical vars to factors
df_XG_train[cat.col] = lapply(df_XG_train[cat.col], factor)
df_XG_validate[cat.col] = lapply(df_XG_validate[cat.col], factor)
#Checking if it worked
# str(df_XG_train)
# str(df_XG_validate)


sparse_train = sparse.model.matrix(target ~ . -target , data=df_XG_train)
sparse_valid = sparse.model.matrix(target  ~ . -target , data=df_XG_validate)
train_label = as.numeric(df_XG_train$target)[df_XG_train$target] 


# tune and run the model
xgb <- xgboost(data = sparse_train,
                   label = train_label,
                   eta = 0.05,
                   max_depth = 20,
                   gamma = 0,
                   nround=100,
                   subsample = 0.75,
                   colsample_bytree = 0.75,
                   objective = "reg:linear",
                   nthread = 3,
                   eval_metric = 'rmse',
                   verbose =0)
ptrain = predict(xgb, sparse_train)
pvalid = predict(xgb, sparse_valid)
# cor(ptrain,df_XG_train$target) ###not really sure what the correlation tells us

########## NEED TO DO MODEL ASSESSMENT AND PREDICTIONS HOW SHE SPECIFIED ########### 
df_XG_train$Prediction = ptrain
df_XG_train$abserror = abs(df_XG_train$target - df_XG_train$Prediction)

MAE = mean(df_XG_train$abserror)
print(MAE)

df_XG_validate$Prediction = pvalid
df_XG_validate$abserror = abs(df_XG_validate$target - df_XG_validate$Prediction)

MAE = mean(df_XG_validate$abserror)
print(MAE)

############################################################################
#Grid Search - SVM#
###########################################################################

set.seed(8) # The greatest number there ever was
intrain<-createDataPartition(y=df_ML$target,p=0.7,list=FALSE)
df_SVM_train<-df_ML[intrain,]
df_SVM_validate<-df_ML[-intrain,]
###########################################################################
############       STANDARDIZE THE CONTINUOUS VARIABLES        ############
###########################################################################
###########################################################################

SVM_trainscale = scale(df_SVM_train[,1:59], center=T, scale=T)
SVM_trainscale = cbind(SVM_trainscale, df_SVM_train[,60:86])
SVM_validscale = scale(df_SVM_validate[,1:59], center=T, scale=T)
SVM_validscale = cbind(SVM_validscale, df_SVM_validate[,60:86])
#install.packages("e1071")
library(e1071)
###########################################################################
###########################################################################
############     TUNE THE SVM HYPERPARAMETERS GAMMA AND C - Takes an eternity     ############
###########################################################################
###########################################################################
TuneSVM = tune.svm(target~., data=SVM_trainscale, kernel='radial', gamma=2^(-5:-3), cost=2^(-1:2))
summary(TuneSVM)
plot(TuneSVM)

############################################################################
############################################################################
## USE THE BEST HYPERPARAMETERS TO BUILD FINAL MODEL AND TEST PERFORMANCE ##
############################################################################
############################################################################

bestgamma=0.0625
bestc=4
svm1=svm(target~., data=SVM_trainscale, kernel='radial', gamma=bestgamma, cost=bestc)
pred=predict(svm1,SVM_trainscale)
pred=predict(svm1,SVM_validscale)
# MAE
SVM_trainscale$Prediction = pred
SVM_trainscale$abserror = abs(SVM_trainscale$target - SVM_trainscale$Prediction)

MAE = mean(SVM_trainscale$abserror)
print(MAE)

SVM_validscale$Prediction = pred
SVM_validscale$abserror = abs(SVM_validscale$target - SVM_validscale$Prediction)

MAE = mean(SVM_validscale$abserror)
print(MAE)


