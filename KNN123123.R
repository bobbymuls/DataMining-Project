library(data.table)
library(mltools)
library(caTools)
library(e1071)
library(class)

rm(list = ls())
setwd("~/Desktop")
dataset = read.csv("ks_project_2018_new.csv")

drops = c("X",
          "name",
          "category",
          "launched_year",
          "launched_month",
          "launched_day",
          "deadline_year",
          "deadline_month",
          "deadline_day",
          "currency",
          "usd.pledged",
          "usd_goal_real",
          "main_category",
          "country"
)

drops2 = c("backers",
           "goal",
           "pledged",
           "usd_pledged_real",
           "duration",
           "textlength"
)

dataset$state = factor(dataset$state,
                       levels = c('failed', 'successful'),
                       labels = c(0, 1))

dataset_h1 = one_hot(as.data.table(dataset$main_category,dataset$country))

normalize = function(x)
{
  return ((x - min(x)) / (max(x)-min(x)))
}

dataset = dataset[,!names(dataset) %in% drops]

dataset2 = as.data.frame(lapply(dataset[,c(1,2,3,4,6,7)],normalize))

state = dataset[,!names(dataset) %in% drops2]

dataset = cbind(state,dataset_h1,dataset2)

split = sample.split(dataset$state, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

x = 200

errRate = rep(0,x)

for(i in 1:x){
  
  dataset_pred =  knn(training_set[,-1],test_set[,-1],training_set$state,k=i)
  cm = table(dataset_pred,test_set$state)
  
  errorrate = (cm[1,2]+cm[2,1])/ nrow(test_set)
  
  errRate[i] = errorrate
}

plot(1:x, errRate,
     main = 'Error Rate vs. K Value',
     xlab = 'K Value',
     ylab = 'Error Rate',
     col = "blue"
) 
