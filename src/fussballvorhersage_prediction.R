library(readr)
library(caret)
library(xgboost)

all_data <- read_csv2("C:/Users/kloet2/Documents/pascal/Fussballvorhersage/loaded_data.csv")

#train <- na.omit(train)


nrounds <- 1000

tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 200),
  eta = c(0.025, 0.1, 0.3),
  max_depth = c(2, 4, 6),
  gamma = c(0,  0.1,  0.7,  1.0),
  colsample_bytree = c(0.4,  0.8, 1.0),
  min_child_weight = c(1,  3),
  subsample = c(0.5,  1.0)
)



tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 4, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)


xgb_base <- caret::train(
  x = all_data[,-(1:4)],
  y = all_data$FTR,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)

xgb_base$bestTune

