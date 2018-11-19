rm(list = ls())
library(e1071)
library(caTools)
library(tictoc)

training_set = read.csv("ks_project_2018_train.csv")
test_set = read.csv("ks_project_2018_test.csv")
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
#Removing unwanted variables
training_set = training_set[,!names(training_set) %in% drops]
test_set = test_set[,!names(test_set) %in% drops]

#Binary encoding of training set part 1
bin_main_cat_train = as.data.frame(matrix(
  as.integer(intToBits(as.integer(as.factor(training_set$main_category)))),
  ncol = 32,
  nrow = length(training_set$main_category),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(training_set$main_category)) + 1)/log(2))])
colnames(bin_main_cat_train) = c("MC_b1", "MC_b2", "MC_b3", "MC_b4")

bin_country_train = as.data.frame(matrix(
  as.integer(intToBits(as.integer(as.factor(training_set$country)))),
  ncol = 32,
  nrow = length(training_set$country),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(training_set$country)) + 1)/log(2))])
colnames(bin_country_train) = c("C_b1", "C_b2", "C_b3", "C_b4", "C_b5")

drops_2 = c("main_category",
            "country")

#Binary encoding of training set part 2
training_set = training_set[,!names(training_set) %in% drops_2]
training_set = cbind(training_set, bin_country_train, bin_main_cat_train)

#Binary encoding of test set part 1
bin_main_cat_test = as.data.frame(matrix(
  as.integer(intToBits(as.integer(as.factor(test_set$main_category)))),
  ncol = 32,
  nrow = length(test_set$main_category),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(test_set$main_category)) + 1)/log(2))])
colnames(bin_main_cat_test) = c("MC_b1", "MC_b2", "MC_b3", "MC_b4")

bin_country_test = as.data.frame(matrix(
  as.integer(intToBits(as.integer(as.factor(test_set$country)))),
  ncol = 32,
  nrow = length(test_set$country),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(test_set$country)) + 1)/log(2))])
colnames(bin_country_test) = c("C_b1", "C_b2", "C_b3", "C_b4", "C_b5")
test_set = test_set[,!names(test_set) %in% drops_2]
test_set = cbind(test_set, bin_country_test, bin_main_cat_test)

#Standardizing both training and test set
train_mean = apply(training_set[-4], 2, mean)
train_sd = apply(training_set[-4], 2, sd)
training_set[-4] = scale(training_set[-4],
                         center = train_mean,
                         scale = train_sd)
test_set[-4] = scale(test_set[-4],
                     center = train_mean,
                     scale = train_sd)

#Running the SVM
tic()
fit = svm(state ~.,
          data = training_set,
          kernel = 'linear',
          cost = 10)
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
accuracy = mean(y_pred == test_set$state) #83.24%
