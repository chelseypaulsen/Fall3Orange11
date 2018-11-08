############################
# Machine Learning Project #
#      Orange Team 11      #
############################
library(dplyr)

df_ML = read.csv('C:\\Users\\jlmic\\Documents\\Machine Learning\\Data\\MLProjectData.csv')
View(df_ML)

# Split 'Training' dataset
library(caret)
intrain<-createDataPartition(y=sub_train$classe,p=0.7,list=FALSE)
training<-m_train[intrain,]
testing<-m_train[-intrain,]

dim(df_ML_train)

# Assign Random Forest Model
library(randomForest)
rf_train = randomForest(target ~ ., data=df_ML_train, importance=TRUE, ntree=50)
print(rf_ML)

# Check Variable Importance
importance(rf_ML) # all important

## Predict on training data and calculate MAE##
rf_train_pred = predict(rf_ML, df_ML_train)

View(rf_train_pred)

df_ML_train$Prediction = rf_ML_pred
df_ML_train$abserror = abs(df_ML_train$target - df_ML_train$Prediction)
View(df_ML_train)

MAE = mean(df_ML_train$abserror)
print(MAE)

# Predict on Test Data
df_ML_test = read.csv('C:\\Users\\jlmic\\Documents\\Machine Learning\\Data\\testData.csv')
View(df_ML_test)

rf_ML_pred = predict(rf_ML, df_ML_test)

View(rf_ML_pred)

df_ML_train$Prediction = rf_ML_pred
df_ML_train$abserror = abs(df_ML_train$target - df_ML_train$Prediction)
View(df_ML_train)

MAE = mean(df_ML_train$abserror)
print(MAE)


