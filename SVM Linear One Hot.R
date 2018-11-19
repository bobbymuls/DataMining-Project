rm(list = ls())
library(e1071)
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
          "category",
          "goal"
)

#One hot encoding the training set
training_set = training_set[,!names(training_set) %in% drops]
one_hot_var_train = model.matrix(state ~ ., data = training_set)
one_hot_var_train = as.data.frame(one_hot_var_train)
state = training_set$state
training_set = cbind(state, one_hot_var_train)
training_set$`(Intercept)` = NULL
colnames(training_set)[7] = 'main_categoryFilmVideo'

#One hot encoding the test set
test_set = test_set[,!names(test_set) %in% drops]
one_hot_var_test = model.matrix(state ~ ., data = test_set)
one_hot_var_test = as.data.frame(one_hot_var_test)
state = test_set$state
test_set = cbind(state, one_hot_var_test)
test_set$`(Intercept)` = NULL
colnames(test_set)[7] = 'main_categoryFilmVideo'

#Computing mean and sd of training set
train_mean = apply(training_set[-1], 2, mean)
train_sd = apply(training_set[-1], 2, sd)

#Standardizing the training set
training_set[-1] = scale(training_set[-1],
                         center = train_mean,
                         scale = train_sd)

#Standardizing the test set
test_set[-1] = scale(test_set[-1],
                     center = train_mean,
                     scale = train_sd)

tic()
fit = svm(state ~.,
          data = training_set,
          kernel = 'linear',
          cost = 0.1)
toc()

# tic()
# svm_lin_tune = tune(svm,
#                     state ~.,
#                     data = training_set,
#                     kernel = 'linear',
#                     ranges = list(cost = c(0.01, 0.1, 1, 10, 100)))
# toc()

y_pred = predict(fit, newdata = test_set, type = "class")
cm = table(y_pred, test_set$state)
accuracy = mean(y_pred == test_set$state) #83.54%
