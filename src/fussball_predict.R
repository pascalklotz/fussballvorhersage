library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(readr)
#install.packages("readxl")
library(readxl)
#library(caret)


setwd("/home/pascal/fussballvorhersage")
getwd()

dat <- read_csv("data/loaded_data.csv")
dat_all <- dat
dat <- dat[is.na(dat$Result),]


missing_percent <- rowSums(is.na(dat)) / ncol(dat)

# Subset data to include only rows with less than 80% missing values
dat <- dat[missing_percent < 0.6, ]

preproc <- readRDS("models/preprocessed_model.RDS")

dat_prep <- predict(preproc, newdata = dat)
dat_prep_all <- predict(preproc, newdata = dat_all)


model <- readRDS("models/my_model.rds")
model_goals <- readRDS("models/my_model_goals.rds")

result <- dat_prep %>% select(Spielid, teamname, Opponent) %>% 
  mutate(Result = (predict(model, dat_prep))) %>% 
  mutate(Result_Goals = (predict(model_goals, dat_prep)))

print(result)
# model_basic <- readRDS("models/my_model_basic.rds")
# model_goals_basic <- readRDS("models/my_model_goals_basic.rds")
# 
# result_analyse<- dat_all %>% 
#   mutate(Result_analyse = (predict(model, dat_prep_all))) %>% 
#   mutate(Result_Goals_analyse = (predict(model_goals, dat_prep_all))) %>%
#   select(Result_analyse, Result_Goals_analyse,everything())
# 
# write_csv(result_analyse, file = "data/analyse_data.csv")
