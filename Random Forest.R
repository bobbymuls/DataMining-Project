rm(list = ls())
setwd("C:/Users/Bobby/Desktop/Uni Stuff/Data Mining/Project Stuff")
library(randomForest)
library(caTools)

dataset = read.csv("ks_project_2018_new.csv")
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

fit = randomForest(state ~ main_category+
                     goal+
                     pledged+
                     backers+
                     usd_pledged_real,
                   data = training_set,
                   mtry = 3,
                   ntree = 50,
                   importance = TRUE)

importance(fit)
varImpPlot(fit)

y_pred = predict(fit, newdata = test_set, type = "class")
cm = table(y_pred, test_set$state)
accuracy = mean(y_pred == test_set$state) #99.9%
