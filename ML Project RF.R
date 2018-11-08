############################
# Machine Learning Project #
#      Orange Team 11      #
############################
library(dplyr)

df_ML = read.csv('C:\\Users\\jlmic\\Documents\\Machine Learning\\Data\\MLProjectData.csv')
View(df_ML)

# Split 'Training' dataset
library(caret)
intrain<-createDataPartition(y=df_ML$target,p=0.7,list=FALSE)
df_ML_train<-df_ML[intrain,]
df_ML_validate<-df_ML[-intrain,]

dim(df_ML_train)
dim(df_ML_validate)

# Assign Random Forest Model
library(randomForest)
rf_ML = randomForest(target ~ ., data=df_ML_train, importance=TRUE, ntree=50)
print(rf_ML)

# Check Variable Importance
importance(rf_ML) # all important

## Predict on training data and calculate MAE##
rf_train_pred = predict(rf_ML, df_ML_train)

View(rf_train_pred)

df_ML_train$Prediction = rf_train_pred
df_ML_train$abserror = abs(df_ML_train$target - df_ML_train$Prediction)
dim(df_ML_train)

MAE = mean(df_ML_train$abserror)
print(MAE)

##################################
# Predict on Validate Data #
##################################

dim(df_ML_validate)

rf_valid_pred = predict(rf_ML, df_ML_validate)

View(rf_valid_pred)

df_ML_validate$Prediction = rf_valid_pred
df_ML_validate$abserror = abs(df_ML_validate$target - df_ML_validate$Prediction)
dim(df_ML_validate)

MAE = mean(df_ML_validate$abserror)
print(MAE)

##################################
# Predict on test Data for Submission#
##################################

df_ML_test = select(df_ML_test, -X)
View(df_ML_test)
