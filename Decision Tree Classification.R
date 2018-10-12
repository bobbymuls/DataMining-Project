rm(list = ls())
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caTools)

dataset = read.csv("ks_project_2018.csv")

split = sample.split(dataset$state, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Fitting the Model
fit = rpart(state ~ category+
              main_category+
              country+
              goal+
              pledged+
              backers+
              usd_pledged_real+
              duration+
              textlength,
            data = training_set,
            control = rpart.control(minsplit = 2))

#Looking at the decision tree graph
fancyRpartPlot(fit)

#Showing the xerror of the fitting
plotcp(fit)

#Creating cptable with number of split > 0
cptable=as.data.frame(fit$cptable)
counter=0
for(i in 1:nrow(cptable))
{
  if(cptable[i-counter,2]==0)
  {
    cptable=cptable[-(i-counter),]
    counter=counter+1
  }
}

#Pruning the tree
pfit = prune(fit,
             cp=cptable[which.min(cptable[,"xerror"]),"CP"])

#Plotting the pruned tree
fancyRpartPlot(pfit)

#Predicting the test set
y_pred = predict(pfit, newdata = test_set)
state_pred = rep(0, nrow(test_set)) #Create a vector of "failed" and "successful"
for(i in 1:nrow(test_set)){
  if(y_pred[i,][1] > y_pred[i,][2]){
    state_pred[i] = "failed"
  }
  else{
    state_pred[i] = "successful"
  }
}

#Create confusion matrix
cm = table(state_pred, test_set$state)
true_predict = sum(cm[1,1], cm[2,2])
accuracy = true_predict / nrow(test_set) #96% accurate
