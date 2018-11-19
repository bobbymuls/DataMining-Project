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
          "category",
          "goal"
)
#Remove unwanted variables in both datasets
training_set = training_set[,!names(training_set) %in% drops]
test_set = test_set[,!names(test_set) %in% drops]

#Computing mean and sd of training set
train_mean = apply(training_set[-c(1,2,6)], 2, mean)
train_sd = apply(training_set[-c(1,2,6)], 2, sd)

#Standardizing the training set
training_set[-c(1,2,6)] = scale(training_set[-c(1,2,6)],
                                center = train_mean,
                                scale = train_sd)

#Standardizing the test set
test_set[-c(1,2,6)] = scale(test_set[-c(1,2,6)],
                            center = train_mean,
                            scale = train_sd)

tic()
tree_fit = tree(state~.,
                data = training_set)
cv_tree_fit = cv.tree(tree_fit)
toc() #4.66 sec

min_cv_tree_fit = cv_tree_fit$size[which.min(cv_tree_fit$dev)]
pruned_tree = prune.tree(tree_fit, best = min_cv_tree_fit)
p_tree_pred = predict(pruned_tree, test_set)
p_tree_pred_vec = rep(0, nrow(p_tree_pred))
for(i in 1:length(p_tree_pred_vec)){
  if(p_tree_pred[i,1] > p_tree_pred[i,2]){
    p_tree_pred_vec[i] = "failed"
  }else{
    p_tree_pred_vec[i] = "successful"
  }
}
table(p_tree_pred_vec, test_set$state)
mean(p_tree_pred_vec == test_set$state) #91.9%
