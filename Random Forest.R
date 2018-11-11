rm(list = ls())
library(tree)
library(caTools)
library(tictoc)

training_set = read.csv("ks_project_2018_train.csv")
test_set = read.csv("ks_project_2018_test.csv")

#Remove unnecessary variables
drops = c("X",
          "name",
          "launched_year",
          "launched_day",
          "deadline_year",
          "deadline_month",
          "deadline_day",
          "pledged",
          "currency",
          "usd.pledged",
          "usd_goal_real",
          "category"
)
#Remove unwanted variables in both datasets
training_set = training_set[,!names(training_set) %in% drops]
test_set = test_set[,!names(test_set) %in% drops]

#Computing mean and sd of training set
train_mean = apply(training_set[-c(1,2,7)], 2, mean)
train_sd = apply(training_set[-c(1,2,7)], 2, sd)

#Standardizing the training set
training_set[-c(1,2,7)] = scale(training_set[-c(1,2,7)],
                                center = train_mean,
                                scale = train_sd)

#Standardizing the test set
test_set[-c(1,2,7)] = scale(test_set[-c(1,2,7)],
                            center = train_mean,
                            scale = train_sd)

tic()
fit = randomForest(state ~ .,
                   data = training_set,
                   mtry = 6,
                   ntree = 50,
                   importance = TRUE)
toc() #3.21 sec

importance(fit)
varImpPlot(fit)

y_pred = predict(fit, newdata = test_set, type = "class")
cm = table(y_pred, test_set$state)
accuracy = mean(y_pred == test_set$state) #99.03%
