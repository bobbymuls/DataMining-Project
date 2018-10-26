rm(list = ls())
setwd("C:/Users/Bobby/Desktop/Uni Stuff/Data Mining/Project Stuff")
library(e1071)
library(caTools)

dataset = read.csv("ks_project_2018_new.csv")
dataset$X.1 = NULL
drops = c("X",
          "name",
          "launched_year",
          "launched_month",
          "launched_day",
          "deadline_year",
          "deadline_month",
          "deadline_day",
          "currency",
          "usd.pledged",
          "usd_goal_real",
          "category"
)
dataset = dataset[,!names(dataset) %in% drops]
dataset$main_category = as.numeric(dataset$main_category)
dataset$country = as.numeric(dataset$country)

dataset[-7] = scale(dataset[-7])

split = sample.split(dataset$state, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

fit = svm(formula = state ~ .,
          data = training_set,
          type = 'C-classification',
          kernel = 'radial',
          gamma = 10)

y_pred = predict(fit, newdata = test_set, type = "class")
cm = table(y_pred, test_set$state)
accuracy = mean(y_pred == test_set$state) #82.12%
