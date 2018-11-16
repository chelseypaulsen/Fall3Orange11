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



############################################################################
#XGBoost#
############################################################################
library('Matrix')
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

############# Copied from above ###############################################################################
#df_xG = read.csv('C:\\Users\\jlmic\\Documents\\Machine Learning\\Data\\MLProjectData.csv')
df_XG= read.csv('C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Machine Learning\\Project\\MLProjectData.csv')
#View(df_XG)

# Split 'Training' dataset
intrain<-createDataPartition(y=df_XG$target,p=0.7,list=FALSE)
df_XG_train<-df_XG[intrain,]
df_XG_validate<-df_XG[-intrain,]

##############################################################################################################

#All vars need to be numeric (or factors)

#Changing all categorical vars to factors
col = c('cat1','cat2','cat3','cat4','cat5','cat6','cat7','cat8','cat9','cat10','cat11','cat12','cat13','cat14','cat15',
        'cat16','cat17','cat18','cat19','cat20','cat21','cat22','cat23','cat24','cat25','cat26')
df_XG_train[col] = lapply(df_XG_train[col], factor)
df_XG_validate[col] = lapply(df_XG_validate[col], factor)
#Checking if it worked
str(df_XG_train)
str(df_XG_validate)


sparse_train = sparse.model.matrix(target ~ . -target , data=df_XG_train)
sparse_valid = sparse.model.matrix(target  ~ . -target , data=df_XG_validate)
train_label = as.numeric(df_XG_train$target)[df_XG_train$target] 


# tune and run the model
xgb <- xgboost(data = sparse_train,
                   label = train_label,
                   eta = 0.05,
                   max_depth = 15,
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
cor(ptrain,df_XG_train$target) ###not really sure what the correlation tells us

print(xgb)

########## NEED TO DO MODEL ASSESSMENT AND PREDICTIONS HOW SHE SPECIFIED ########### 
