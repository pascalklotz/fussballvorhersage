library(dplyr)
library(tidyr)
library(caret)
library(readr)
#install.packages("h2o")

library(h2o)

# Initialize and connect to an h2o cluster
h2o.init()

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



# Create and train a deep learning model
model <- h2o.deeplearning(
  x = x_train,
  y = "target_variable",
  hidden = c(64, 64),
  epochs = 50
)

