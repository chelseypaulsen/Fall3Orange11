############################
# Machine Learning Project #
#      Orange Team 11      #
############################
library(dplyr)

df_ML_train = read.csv('C:\\Users\\jlmic\\Documents\\Machine Learning\\Data\\MLProjectData.csv')
View(df_ML)

library(randomForest)
rf_ML = randomForest(target ~ ., data=df_ML_train, importance=TRUE, ntree=50)
print(rf_ML)

# Check Variable Importance
importance(rf_ML) # all important

## Predict on training data and calculate MAE##
rf_ML_pred = predict(rf_ML, df_ML_train)

View(rf_ML_pred)

df_ML_train$Prediction = rf_ML_pred
df_ML_train$abserror = abs(df_ML_train$target - df_ML_train$Prediction)
View(df_ML_train)

MAE = mean(df_ML_train$abserror)
print(MAE)





