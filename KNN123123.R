library(data.table)
library(mltools)
library(caTools)
library(e1071)
library(class)

rm(list = ls())
setwd("~/Desktop")
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
one_hot_var_train = model.matrix(state ~ .-1, data = training_set)
one_hot_var_train = as.data.frame(one_hot_var_train)
state = training_set$state
training_set = cbind(state, one_hot_var_train)
training_set$`(Intercept)` = NULL
colnames(training_set)[8] = 'main_categoryFilmVideo'

#One hot encoding the test set
test_set = test_set[,!names(test_set) %in% drops]
one_hot_var_test = model.matrix(state ~ .-1, data = test_set)
one_hot_var_test = as.data.frame(one_hot_var_test)
state = test_set$state
test_set = cbind(state, one_hot_var_test)
test_set$`(Intercept)` = NULL
colnames(test_set)[8] = 'main_categoryFilmVideo'

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


x = 1
j = 400
errRate = rep(1,400)

for(i in x:j){
  
  dataset_pred =  knn(training_set[,-1],test_set[,-1],training_set$state,k=i)
  cm = table(dataset_pred,test_set$state)
  
  errorrate = (cm[1,2]+cm[2,1])/ nrow(test_set)
  
  errRate[i] = errorrate
}

plot(x:j, errRate[1:400],
     main = 'Error Rate vs. K Value',
     xlab = 'K Value',
     ylab = 'Error Rate',
     col = "blue"
) 

bestk = which.min(errRate)
print(bestk)

dataset_pred =  knn(training_set[,-1],test_set[,-1],training_set$state,k=bestk)
matrix = table(dataset_pred,test_set$state)

errorrate = (matrix[1,2]+matrix[2,1])/ nrow(test_set)
print(errorrate)
