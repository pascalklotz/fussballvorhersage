library(dplyr)
library(tidyr)
#install.packages(c("caret"))
library(caret)
#install.packages("catboost")

#install.packages("xgboost")
library(xgboost)
#library(rpart)
#library(ggplot2)
library(readr)
#install.packages("randomForest")
#install.packages("devtools", repos="http://cran.us.r-project.org")
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

split <- 1
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



saveRDS(preproc, "models/preprocessed_model.RDS")




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



control <- trainControl(method = "repeatedcv"
                        , number = 5
                        , repeats = 3
                        , summaryFunction = customMetric)
#control <- trainControl(method = "LOOCV")

#control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, verboseIter = TRUE)

# # Define a new metric function that rounds the raw metric values
rounded_mae <- function (data, lev = NULL, model = NULL) {
  raw_mae <- MAE(data, lev = lev, model = model)
  round(raw_mae,0) # round the raw MAE values to 0 decimal places
}


tuneGrid_rf <- expand.grid(nrounds = c(100,300,500)
                        , max_depth = c(2,4,6)
                        , eta = 0.1
                        , gamma = c(0,0.5)
                        , colsample_bytree = 1
                        , min_child_weight = 1
                        , subsample = 1
)

tuneGrid <- expand.grid(iterations = seq(100, 1000, by = 100),
                        depth = seq(3, 10, by = 1),
                        learning_rate = seq(0.01, 0.3, by = 0.01))

tuneGrid_xgb <- expand.grid(
  nrounds = seq(100, 500, by = 200),
  max_depth = c(2, 3, 4, 5),
  eta = c(0.01,0.1,0.3),
  gamma = seq(0, 1, by = 0.5),
  colsample_bytree = seq(0.5, 1, by = 0.25),
  min_child_weight = c(1, 2),
  subsample = c(0.5, 1)
)
  
tuneGrid_rf <- expand.grid(mtry = c(2:7))


# Define the regression model
model <- train(
                x = X_train, 
                y = y_train, 
                method = 'rf',
               #method = "xgbTree",
               trControl = control
               , tuneGrid = tuneGrid_rf
               #, method = 'catboost'
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


dat_train$quality <- ifelse(round(dat_train$predictions) == dat_train$Result, 3, 0)
dat_train$quality[round(dat_train$predictions) != dat_train$Result & sign(round(dat_train$predictions)) == sign(dat_train$Result)] <- 1


print(sum(dat_train$quality) / nrow(dat_train))

# Use the model to predict on the test set
dat_test$predictions <- predict(model, newdata = X_test)

dat_test$quality <- ifelse(round(dat_test$predictions) == y_test, 3, 0)
dat_train$quality[round(dat_test$predictions) != y_test & sign(round(dat_test$predictions)) == sign(y_test)] <- 1


print(sum(dat_test$quality) / nrow(dat_test))


saveRDS(model, file = "models/my_model.rds")

