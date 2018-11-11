rm(list = ls())

library(neuralnet)
library(data.table)
library(mltools)
library(caTools)

dataset = read.csv("ks_project_2018_new.csv")

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

#One hot encoding the dataset
dataset = dataset[,!names(dataset) %in% drops]
one_hot_var_data = model.matrix(state ~ .-1, data = dataset)
one_hot_var_data = as.data.frame(one_hot_var_data)
state = dataset$state
dataset = cbind(state, one_hot_var_data)
dataset$`(Intercept)` = NULL
colnames(dataset)[8] = 'main_categoryFilmVideo'

# Convert string state to numeric 0 and 1
dataset$state = as.numeric(as.character(factor(dataset$state,
                                               levels = c('failed', 'successful'),
                                               labels = c(0, 1))))
#str(dataset)
set.seed(4510)

#10 fold cross validation
K = 10
accuracy = NULL
costNNCV = NULL

#Writing as a formula to insert into the neuralnet package
n = names(dataset)
f = as.formula(paste("state ~", paste(n[!n %in% "state"], collapse = " + ")))

#Fitting Neural Network
for(k in 1:K){
  index = sample(seq_len(nrow(dataset)),round(0.9*nrow(dataset)))
  trainNNCV = dataset[index,]
  testNNCV = dataset[-index,]
  
  #Computing mean and sd of training set
  train_mean = apply(trainNNCV[-1], 2, mean)
  train_sd = apply(trainNNCV[-1], 2, sd)
  
  #Standardizing the training set for each K
  trainNNCV[-1] = scale(trainNNCV[-1],
                           center = train_mean,
                           scale = train_sd)
  
  #Standardizing the test set for each K
  testNNCV[-1] = scale(testNNCV[-1],
                       center = train_mean,
                       scale = train_sd)
  
  NNCV <- neuralnet(f,data = trainNNCV,hidden = 30,stepmax = 1e6,linear.output = F,lifesign = 'full')
  
  #Prediction
  mypredictNNCV = compute(NNCV,testNNCV[,-1])$net.result
  
  #MSE
  costNNCV[k] = sum((testNNCV$state - mypredictNNCV)^2)/nrow(testNNCV)
  
  #Accuracy of result
  mypredictroundedNNCV = sapply(mypredictNNCV,round,digits=0) #to round off as State is either 0 or 1
  #To compute confusion matrix
  confmat = table(mypredictroundedNNCV, testNNCV$state)
  misclassificationrate = (1-sum(diag(confmat))/sum(confmat))*100
  accuracy[k] = 100-misclassificationrate
  
}
#Computing the accuracy of the neural network
mean(accuracy)
#98.88%
