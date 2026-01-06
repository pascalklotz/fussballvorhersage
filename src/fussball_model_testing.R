library(dplyr)
library(tidyr)
library(caret)
#install.packages("xgboost")
library(xgboost)
#library(rpart)
#library(ggplot2)
library(readr)
#install.packages("randomForest")

#library(randomForest)
library("readxl")

setwd("/home/pascal/fussballvorhersage")
getwd()

dat <- read_csv("data/loaded_data.csv")
dat <- dat[!is.na(dat$Result),]



missing_percent <- rowSums(is.na(dat)) / ncol(dat)

# Subset data to include only rows with less than 80% missing values
dat <- dat[missing_percent < 0.1, ]



dat <- dat %>% select(-c(saison, spieltag, teamname, Opponent, Result_Goals) )


# Get the total number of rows
n_rows <- nrow(dat) * 1

split <- 0.7
# Calculate the row number to split at
split_row <- min(floor(round(n_rows / 9 * split,0) * 9),n_rows)


#for(i_pca in 1:100){
# Exclude the last 20% of rows
dat_train <- dat[1:split_row,]
X_train <- select(dat_train,-c(Result,Spielid))
y_train <- dat_train$Result
dat_test <- dat[(split_row + 1):n_rows,]
X_test <- select(dat_test,-c(Result,Spielid))
y_test <- dat_test$Result



# Check for missing values
sum(is.na(X_train))

#preproc <- preProcess(X_train, method = c("medianImpute","center", "scale","pca"), pcaComp = i_pca)
preproc <- preProcess(X_train, method = c("medianImpute","center", "scale"))
X_train <- predict(preproc, newdata = X_train)
sum(is.na(X_train))
sum(is.na(y_train))
X_test <- predict(preproc, newdata = X_test)
sum(is.na(X_test))



# Benutzerdefinierte Metrikfunktion
customMetric <- function(data, lev = NULL, model = NULL) {
  # print(data)
  predictions <- round(data$pred)
  observations <- data$obs
  
  # Punkte vergeben
  points <- ifelse((predictions) == observations, 3, 0)
  points[(predictions) != observations & sign(predictions) == sign(observations)] <- 1
  
  # Durchschnittliche Punktzahl berechnen
  print(mean(points))
  c(Points = mean(points))
  
}

customMetric_10 <- function(data, lev = NULL, model = NULL) {
  # print(data)
  predictions <- round(data$pred)
  observations <- data$obs
  
  # Punkte vergeben
  points <- ifelse((predictions) == observations, 10, 0)
  points[(predictions) != observations & sign(predictions) == sign(observations)] <- 1
  
  # Durchschnittliche Punktzahl berechnen
  print(mean(points))
  c(Points = mean(points))
  
}


control_custom <- trainControl(method = "repeatedcv"
                               , number = 5
                               , repeats = 3
                               , summaryFunction = customMetric)

control_custom_basic <- trainControl(method = "repeatedcv"
                               , number = 5
                               , repeats = 3)

control_custom_10p <- trainControl(method = "repeatedcv"
                        , number = 5
                        , repeats = 3
                        , summaryFunction = customMetric_10)

control_custom_5_5 <- trainControl(method = "repeatedcv"
                               , number = 5
                               , repeats = 5
                               , summaryFunction = customMetric)

control_custom_5_1 <- trainControl(method = "repeatedcv"
                                   , number = 5
                                   , repeats = 1
                                   , summaryFunction = customMetric)

control_custom_10 <- trainControl(method = "repeatedcv"
                               , number = 10
                               , repeats = 3
                               , summaryFunction = customMetric)

control_custom_7 <- trainControl(method = "repeatedcv"
                                  , number = 10
                                  , repeats = 3
                                  , summaryFunction = customMetric)
#control <- trainControl(method = "LOOCV")

#control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, verboseIter = TRUE)

# # Define a new metric function that rounds the raw metric values
rounded_mae <- function (data, lev = NULL, model = NULL) {
  raw_mae <- MAE(data, lev = lev, model = model)
  round(raw_mae,0) # round the raw MAE values to 0 decimal places
}



control <- trainControl(method = "repeatedcv"
                        , number = 5
                        , repeats = 3
                        , verboseIter = TRUE
                        , classProbs = TRUE)


control_2 <- trainControl(method = "repeatedcv"
                        , number = 10
                        , repeats = 3
                        , verboseIter = TRUE
                        , classProbs = TRUE)

# # Define a new metric function that rounds the raw metric values
rounded_mae <- function (data, lev = NULL, model = NULL) {
  raw_mae <- MAE(data, lev = lev, model = model)
  round(raw_mae,0) # round the raw MAE values to 0 decimal places
  
}

tuneGrid1 <- expand.grid(nrounds = c(100,300)
                         , max_depth = c(2,4)
                         , eta = 0.1
                         , gamma = c(0,0.5)
                         , colsample_bytree = 1
                         , min_child_weight = 1
                         , subsample = 1
)

rpart2_grid <- expand.grid(maxdepth = c(2,3,4,5,6))


tuneGrid_xgb <- expand.grid(nrounds = c(100,300,500)
                        , max_depth = c(2,4,6)
                        , eta = 0.1
                        , gamma = c(0,0.5)
                        , colsample_bytree = 1
                        , min_child_weight = 1
                        , subsample = 1
)

rf_tunegrid <- expand.grid(mtry = c(2, 4, 6))
rf_tunegrid_2 <- expand.grid(mtry = c(2:8))
rf_tunegrid_3 <- expand.grid(mtry = c(2:5))
rf_tunegrid_4 <- expand.grid(mtry = c(2:4))

svm_tune <- expand.grid(sigma = c(0.1, 1, 10), C = c(0.1, 1, 10))
gbm_tune <- expand.grid(n.trees = c(50, 100, 200),
                        interaction.depth = c(1, 3, 5),
                        shrinkage = c(0.1, 0.01),
                        n.minobsinnode = c(5, 10, 15))
plsr_tunegrid <- expand.grid(ncomp = c(2, 4, 6))
enet_tunegrid <- expand.grid(alpha = seq(0, 1, 0.1), lambda = c(0.1, 0.5, 1))
knn_tunegrid <- expand.grid(k = seq(1, 20, by = 1))
treebag_tune_grid <- expand.grid(
  nbagg = seq(50, 200, by = 50),
  bag.fraction = seq(0.5, 1, by = 0.1)
)
ada_tune_grid <- expand.grid(
  n.trees = c(50, 100, 200),
  learning.rate = c(0.1, 0.05, 0.01)
)
nnet_tune_grid <- expand.grid(
  size = seq(1, 10, by = 1),
  decay = c(0, 0.1, 0.01)
)
metric <- c("rounded_mae", "RMSE", "MAE")

algo <- c("rpart2", "xgbTree", "rf","svmRadial", "gbm", "pls","glmnet", "knn", "treebag", "ada", "nnet")

test <- track_model(X_train, y_train, X_test, y_test, method = algo[11]
                    , trControl= control_custom_basic, tuneGrid =nnet_tune_grid
                    , append = TRUE, desc = 'rollmean_4')

track <- read_csv("model_tracker.csv")

View(track)
