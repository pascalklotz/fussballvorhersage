library(readr)
library(caret)
library(xgboost)

prod <- read_csv2("C:/Users/kloet2/Documents/pascal/Fussballvorhersage/prod_data.csv")
all_data <-  read_csv2("C:/Users/kloet2/Documents/pascal/Fussballvorhersage/loaded_data.csv")
#train <- na.omit(train)


nrounds <- 1000

default_grid <- expand.grid(
  nrounds = 200,
  eta = 0.025,
  max_depth = 2,
  gamma = 0,
  colsample_bytree = 0.4,
  min_child_weight = 3,
  subsample = 0.5
)



train_control <- caret::trainControl(
  method = "none", # cross-validation
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)


xgb_base <- caret::train(
  x = all_data[,-(1:4)],
  y = all_data$FTR,
  trControl = train_control,
  tuneGrid = default_grid,
  method = "xgbTree",
  verbose = TRUE
)

show_prod <- prod[,1:4]
show_prod$FTR <- round(predict(xgb_base, prod),0)
