rm(list = ls())

library(neuralnet)
library(data.table)
library(mltools)
library(caTools)

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

#One hot encoding the training set
training_set = training_set[,!names(training_set) %in% drops]
one_hot_var_train = model.matrix(state ~ .-1, data = training_set)
one_hot_var_train = as.data.frame(one_hot_var_train)
state = training_set$state
training_set = cbind(state, one_hot_var_train)
training_set$`(Intercept)` = NULL
colnames(training_set)[8] = 'main_categoryFilmVideo'

# Convert string state to numeric 0 and 1
training_set$state = as.numeric(as.character(factor(training_set$state,
                                                    levels = c('failed', 'successful'),
                                                    labels = c(0, 1))))
#str(training_set)

#One hot encoding the test set
test_set = test_set[,!names(test_set) %in% drops]
one_hot_var_test = model.matrix(state ~ .-1, data = test_set)
one_hot_var_test = as.data.frame(one_hot_var_test)
state = test_set$state
test_set = cbind(state, one_hot_var_test)
test_set$`(Intercept)` = NULL
colnames(test_set)[8] = 'main_categoryFilmVideo'

# Convert string state to numeric 0 and 1
test_set$state = as.numeric(as.character(factor(test_set$state,
                                                levels = c('failed', 'successful'),
                                                labels = c(0, 1))))
#str(test_set)

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

#Writing as a formula to insert into the neuralnet package
n = names(training_set)
f = as.formula(paste("state ~", paste(n[!n %in% "state"], collapse = " + ")))

#5 fold cross validation
K = 5
accuracy = NULL
costNNCV = NULL
neuronMSE = NULL
neuronaccuracy = NULL
#Fitting Neural Network to select parameters for the neural network
neuronsize = c(25,30,35) #or any other vector with possible neuron sizes
len = length(neuronsize)
for (i in 1:len) {
  for(k in 1:K){
    index = sample(seq_len(nrow(training_set)),round(0.7*nrow(training_set)))
    #Pseudo-training and pseudo-test set (70-30 split)
    trainNNCV = training_set[index,]
    testNNCV = training_set[-index,]
    
    NNCV <- neuralnet(f,data = trainNNCV,hidden = neuronsize[i],
                      learningrate.factor=list(minus=c(0.5), plus=c(1.2)), stepmax = 1e6,linear.output = F,lifesign = 'full')
    
    #Prediction
    mypredictNNCV = compute(NNCV,testNNCV[,-1])$net.result
    
    #MSE (cross validation error)
    costNNCV[k] = sum((testNNCV$state - mypredictNNCV)^2)/nrow(testNNCV)
    
    #Accuracy of result
    mypredictroundedNNCV = sapply(mypredictNNCV,round,digits=0) #to round off as State is either 0 or 1
    #To compute confusion matrix
    confmat = table(mypredictroundedNNCV, testNNCV$state)
    misclassificationrate = (1-sum(diag(confmat))/sum(confmat))*100
    accuracy[k] = 100-misclassificationrate
  }
  #Mean cost & accuracy
  neuronaccuracy[i] =  mean(accuracy)
  neuronMSE[i] = mean(costNNCV)
}
#Return the vectors of each neuron corresponding MSE/accuracy
#neuronaccuracy
#neuronMSE
#To select the neuron size that returns the lowest MSE
(optimalneuron = neuronsize[match(min(neuronMSE),neuronMSE)])

#Running the neuralnet with tuned parameters
NN <- neuralnet(f,data = training_set,hidden = optimalneuron,learningrate.factor=list(minus=c(0.5), plus=c(1.2)),stepmax = 1e6,linear.output = F,lifesign = 'full')

#Prediction
mypredictNN = compute(NN,test_set[,-1])$net.result

#Accuracy of result
mypredictroundedNN = sapply(mypredictNN,round,digits=0) #to round off as State is either 0 or 1
#To compute confusion matrix
confmat = table(mypredictroundedNN, test_set$state)
misclassificationrate = (1-sum(diag(confmat))/sum(confmat))*100
(accuracy = 100-misclassificationrate) #Returns accuracy of model on predicting test set

