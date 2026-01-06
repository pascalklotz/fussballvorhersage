library(readr)
#install.packages("caret")
library(caret)
library(xgboost)
library(dplyr)
#install.packages('rvest')
#install.packages('xml2')
#install.packages("tidyverse")
library(tidyverse)

setwd("/home/pascalkloetzer/git/fussballvorhersage/fussballvorhersage")
getwd()
all_data <- read_csv2("data/loaded_data.csv")
# 
#train <- na.omit(train)

a <- 1:(round(nrow(all_data)*0.875,0))
all_data <- remove_rownames(all_data)

train <- all_data[a,]
test <- all_data[-a,]


pp <- preProcess(train, method = "range")


train_scale <- predict(pp, train)


test_scale <- predict(pp,test)




X_train <- train_scale %>% select(-c(Spielid, Hometeam, Awayteam, FTR))
y_train <- train$FTR
X_test <- test_scale %>% select(-c(Spielid, Hometeam, Awayteam, FTR))
y_test <- test$FTR

saveRDS(pp, file = "data/preprocess.RDS")
write_csv2(train, file = "data/train_data.csv")


tune_control <- caret::trainControl(
  method = "repeatedcv", # cross-validation
  repeats = 3,
  number = 10, # with n folds 
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results ,
)


model <- caret::train(
  x = X_train,
  y = y_train,
  trControl = tune_control,
  method = "lm",
  metric = "RMSE"
)
model
RMSE(y_train, predict(model,X_train))
RMSE(y_test, predict(model,X_test))
write.table(data.frame(Model = "lm",RMSE_train = RMSE(y_train, predict(model,X_train)),RMSE_test = RMSE(y_test, predict(model,X_test))),
          file = "result.csv", col.names = F, row.names =  F, sep = ";", append = T)


#install.packages("randomForest")
library(randomForest)
tune_grid <- expand.grid(
  mtry = c(3,5,sqrt(ncol(X_train)),10)
)



model <- caret::train(
  x = X_train,
  y = y_train,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "rf",
  metric = "RMSE"
  
)
model
best_model <- model
RMSE(y_train, predict(model,X_train))
RMSE(y_test, predict(model,X_test))
model$bestTune
write.table(data.frame(Model = "rf",RMSE_train = RMSE(y_train, predict(model,X_train)),RMSE_test = RMSE(y_test, predict(model,X_test)), besttune = model$bestTune),
            file = "result.csv", col.names = F, row.names =  F, sep = ";", append = T)

best_model <- model



tune_grid <- expand.grid(
  mtry = c(2,4,c(sqrt(ncol(X_train)))),
  maxdepth = 2:10
)
model <- caret::train(
  x = X_train,
  y = y_train,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "rfRules",
  metric = "RMSE"
  
)
model
best_model <- model
RMSE(y_train, predict(model,X_train))
RMSE(y_test, predict(model,X_test))
model$bestTune
write.table(data.frame(Model = "rfRules",RMSE_train = RMSE(y_train, predict(model,X_train)),RMSE_test = RMSE(y_test, predict(model,X_test)), besttune = model$bestTune),
            file = "result.csv", col.names = F, row.names =  F, sep = ";", append = T)




#install.packages("randomForest")
library(randomForest)
tune_grid <- expand.grid(
  cp=c(0.02,0.03,0.04,0.05,0.06,0.07,0.1)
)

model <- caret::train(
  x = X_train,
  y = y_train,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "rpart",
  metric = "RMSE"
  
)
model
RMSE(y_train, predict(model,X_train))
RMSE(y_test, predict(model,X_test))
write.table(data.frame(Model = "rpart",RMSE_train = RMSE(y_train, predict(model,X_train)),RMSE_test = RMSE(y_test, predict(model,X_test)), besttune = model$bestTune),
            file = "result.csv", col.names = F, row.names =  F, sep = ";", append = T)













library(randomForest)
tune_grid <- expand.grid(
  maxdepth = c(1,2,3)
)

model <- caret::train(
  x = X_train,
  y = y_train,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "rpart2",
  metric = "RMSE"
  
)
model
RMSE(y_train, predict(model,X_train))
RMSE(y_test, predict(model,X_test))
write.table(data.frame(Model = "rpart2",RMSE_train = RMSE(y_train, predict(model,X_train)),RMSE_test = RMSE(y_test, predict(model,X_test)), besttune = model$bestTune),
            file = "result.csv", col.names = F, row.names =  F, sep = ";", append = T)











#install.packages("randomForest")
library(randomForest)
tune_grid = expand.grid(nrounds = c(50,100),
                        max_depth = c(2,3,4),
                        eta = c(0.2,0.6,1),
                        min_child_weight = c(1),
                        subsample = c(0.4,0.7),
                        gamma = c(0.2,0.6),
                        colsample_bytree = c(0.3,0.4)
)
tune_grid = expand.grid(nrounds = c(50),
                        max_depth = c(2),
                        eta = c(0.2),
                        min_child_weight = c(1),
                        subsample = c(0.4),
                        gamma = c(0.2),
                        colsample_bytree = c(0.4)
)

model <- caret::train(
  x = X_train,
  y = y_train,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  metric = "RMSE"
)
model
RMSE(y_train, predict(model,X_train))
RMSE(y_test, predict(model,X_test))
best_model <- model







tune_grid = expand.grid(
  layer1 = c(5,10,20,50),
  layer2 = c(5,10,20,50),
  layer3 = c(5,10,20,50)
)

model <- caret::train(
  x = X_train,
  y = y_train,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "mlpML",
  metric = "RMSE"
)
model
RMSE(y_train, predict(model,X_train))
RMSE(y_test, predict(model,X_test))
best_model <- model
write.table(data.frame(Model = "xgb",RMSE_train = RMSE(y_train, predict(model,X_train)),RMSE_test = RMSE(y_test, predict(model,X_test)), besttune = model$bestTune),
            file = "result.csv", col.names = F, row.names =  F, sep = ";", append = T)












saveRDS(best_model,"data/final_model.rds")
