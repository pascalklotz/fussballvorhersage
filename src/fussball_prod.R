library(readr)
#install.packages("caret")
library(caret)
#install.packages("xgboost")
library(xgboost)
library(bigrquery)

setwd("/home/pascalkloetzer/git/fussballvorhersage/fussballvorhersage")
getwd()

prod <- read_csv2("data/prod_data.csv")
all_data <-  read_csv2("data/loaded_data.csv")
#train <- na.omit(train)

model <- readRDS("data/final_model.rds")
prep <- readRDS("data/preprocess.RDS")

prod_prep <- predict(prep,prod)
#prod_prep
show_prod <- prod[,1:4]
show_prod$FTR <- round(predict(model, prod_prep),0)
show_prod


#save results to csv
hist <- read_csv2("data/history.csv")
hist <- rbind(hist, show_prod)
write_csv2(hist, file = "data/history.csv")
