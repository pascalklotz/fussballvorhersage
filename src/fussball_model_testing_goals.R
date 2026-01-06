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
dat <- dat[!is.na(dat$ResultxG),]
dat <- dat[!is.na(dat$Result),]


missing_percent <- rowSums(is.na(dat)) / ncol(dat)

# Subset data to include only rows with less than 80% missing values
dat <- dat[missing_percent < 0.1, ]



dat <- dat %>% select(-c(saison, spieltag, teamname, Opponent) )


# Get the total number of rows
n_rows <- nrow(dat) * 1

split <- 1
# Calculate the row number to split at
split_row <- min(floor(round(n_rows / 9 * split,0) * 9),n_rows)


#for(i_pca in 1:100){
# Exclude the last 20% of rows
dat_train <- dat[1:split_row,]
X_train <- select(dat_train,-c(Result,Spielid, Result_Goals, ResultxG, Result_GoalsxG))
y_train <- dat_train$Result_GoalsxG
dat_test <- dat[(split_row + 1):n_rows,]
X_test <- select(dat_test,-c(Result,Spielid, Result_Goals, ResultxG, Result_GoalsxG))
y_test <- dat_test$Result_Goals



# Check for missing values
sum(is.na(X_train))

#preproc <- preProcess(X_train, method = c("medianImpute","center", "scale","pca"), pcaComp = i_pca)
preproc <- preProcess(X_train, method = c("medianImpute","center", "scale"))
X_train <- predict(preproc, newdata = X_train)
sum(is.na(X_train))
sum(is.na(y_train))
X_test <- predict(preproc, newdata = X_test)
sum(is.na(X_test))



saveRDS(preproc, "models/preprocessed_model.RDS")



customMetric <- function(data, lev = NULL, model = NULL) {
  # print(data)
  predictions <- round(data$pred)
  observations <- data$obs
  
  # Punkte vergeben
  points <- ifelse(abs(predictions - observations) <= 1, 1, 0)
  points <- ifelse(predictions == observations, 3, 0)
  
  # Durchschnittliche Punktzahl berechnen
  print(mean(points))
  c(Points = mean(points))
  
}


control_custom <- trainControl(method = "repeatedcv"
                               , number = 5
                               , repeats = 3
                               #, summaryFunction = customMetric
                               )



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

tuneGrid2 <- expand.grid(maxdepth = c(2,3,4,5,6))


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

test <- track_model_goals(X_train, y_train, X_test, y_test, method = algo[3]
                    , trControl= control_custom, tuneGrid =rf_tunegrid_2 
                    , append = TRUE, desc = 'goals')

track <- read_csv("model_tracker.csv")

View(track)
