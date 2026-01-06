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


control <- trainControl(method = "repeatedcv"
                        , number = 5
                        , repeats = 3
                        , verboseIter = TRUE)
#control <- trainControl(method = "LOOCV")

#control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, verboseIter = TRUE)

# Define a new metric function that rounds the raw metric values
# rounded_mae <- function (data, lev = NULL, model = NULL) {
#   raw_mae <- MAE(data, lev = lev, model = model)
#   round(raw_mae,0) # round the raw MAE values to 0 decimal places
# }


tuneGrid <- expand.grid(mtry = c(2:7))
  
  #expand.grid(mtry = c(2, 4, 6))

# Define the regression model
model <- train(
  x = X_train, 
  y = y_train, 
  method = "rf",
  trControl = control, 
  metric = 'RMSE'
  , tuneGrid = tuneGrid
)


#https://topepo.github.io/caret/available-models.html
# Print the model results
print(model)

importance <- as.data.frame(varImp(model)$importance)
#print(importance)

top10 <- importance %>%
  filter(Overall >= 25) 

# plot variable importance
# ggplot(top10, aes(x = reorder(rownames(top10), Overall), y = Overall)) + 
#   geom_bar(stat = "identity", fill = "steelblue") +
#   xlab("Predictor Variable") + 
#   ylab("Importance Score") + 
#   ggtitle("Variable Importance Plot") +
#   coord_flip()



flop10 <- importance %>%
  filter(Overall <= 25) 
# 
# # plot variable importance
# ggplot(flop10, aes(x = reorder(rownames(flop10), Overall), y = Overall)) + 
#   geom_bar(stat = "identity", fill = "steelblue") +
#   xlab("Predictor Variable") + 
#   ylab("Importance Score") + 
#   ggtitle("Variable Importance Plot") +
#   coord_flip()
#summary(model$finalModel)

# Use the model to predict on the test set
dat_train$predictions <- predict(model, newdata = X_train)

dat_train$quality <- abs(round(dat_train$predictions) - dat_train$Result)
print(sum(dat_train$quality) / nrow(dat_train))

# Use the model to predict on the test set
dat_test$predictions <- predict(model, newdata = X_test)

dat_test$quality <- abs(round(dat_test$predictions) - y_test)
print(sum(dat_test$quality) / nrow(dat_test))


saveRDS(model, file = "models/my_model_goals.rds")

