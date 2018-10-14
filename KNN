library(caTools)
library(e1071)
library(class)

rm(list = ls())
setwd("~/Desktop")
dataset = read.csv("ks_project_2018.csv")

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
          "usd_goal_real"
)

dataset = dataset[,!names(dataset) %in% drops]

dataset$main_category = as.numeric(factor(dataset$main_category,
                                          levels = c('Art','Comics','Crafts','Dance','Design','Fashion','Film & Video','Food','Games','Journalism','Music','Photography','Publishing','Technology','Theater'),
                                          labels = c(1, 2, 3,4,5,6,7,8,9,10,11,12,13,14,15)))

dataset$country = as.numeric(factor(dataset$country,
                                    levels = c('GB', 'AU', 'US','CA','NO','IT','DE','IE','MX','ES','SE','FR','NZ','CH','AT','BE','DK','HK','NL', 'LU','SG','JP'),
                                    labels = c(1, 2, 3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)))

dataset$state = factor(dataset$state,
                       levels = c('failed', 'successful'),
                       labels = c(0, 1))

summary(dataset)

split = sample.split(dataset, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

dataset_pred =  knn(training_set[,-7],test_set[,-7],training_set[,7],k=20)

cm = table(dataset_pred,test_set$state)
cm

