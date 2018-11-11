library(glmnet)
library(caret)
library(ROCR)
library(lmtest)

set.seed(4510)

ks.train <- read.csv("ks_project_2018_train.csv")
ks.test <- read.csv("ks_project_2018_test.csv")

# Remove unnecessary variables
drops = c("name",
          "category",
          "launched_year",
          "launched_day",
          "deadline_year",
          "deadline_month",
          "deadline_day",
          "currency",
          "goal",
          "pledged",
          "usd.pledged",
          "usd_pledged_real"
          )

ks.train <- ks.train[,!names(ks.train) %in% drops]
ks.test <- ks.test[,!names(ks.test) %in% drops]

# Convert string state to numeric 0 and 1
ks.train$state <- as.numeric(ks.train$state)
for (i in 1:nrow(ks.train)){
  if (ks.train$state[i] == 1){
    ks.train$state[i] = 0
  }
  else if (ks.train$state[i] == 2){
    ks.train$state[i] = 1
  }
}

ks.test$state <- as.numeric(ks.test$state)
for (i in 1:nrow(ks.test)){
  if (ks.test$state[i] == 1){
    ks.test$state[i] = 0
  }
  else if (ks.test$state[i] == 2){
    ks.test$state[i] = 1
  }
}

# Convert state and launched_month to factor
ks.train$state <- as.factor(ks.train$state)
ks.train$launched_month <- as.factor(ks.train$launched_month)
ks.test$state <- as.factor(ks.test$state)
ks.test$launched_month <- as.factor(ks.test$launched_month)

#Computing mean and sd for numerical variables in training set
ks.mean <- apply(ks.train[,c(4, 5, 7, 8)], 2, mean)
ks.sd <- apply(ks.train[,c(4, 5, 7, 8)], 2, sd)

#Standardizing the training set
ks.train[,c(4, 5, 7, 8)] <- scale(ks.train[,c(4, 5, 7, 8)], center = ks.mean, scale = ks.sd)
#Standardizing the test set
ks.test[,c(4, 5, 7, 8)] <- scale(ks.test[,c(4, 5, 7, 8)], center = ks.mean, scale = ks.sd)

View(ks.train)
View(ks.test)

#-----------------------------------------------------------------------------------------------
# A full logistic regression model was built with State as the response variable against the rest of the 
# 7 predictor variables - Main Category, Country, Launched Month, Duration, Backers, Goal and Textlength. 
# Step-wise variable selection method was used to identify the most important variables in the logistic regression model.

nullmodel <- glm(state~1, data = ks.train, family = "binomial")
fullmodel <- glm(state~., data = ks.train, family = "binomial")

# Backward step-wise selection
backward.model <- step(fullmodel, direction = "backward")

# Forward step-wise selection
forward.model <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = 'forward')

# Both backward and forward step-wise selection methods give us the same final model.
# The logistic regression model is chonsen with the lowest AIC of 18933.97 as criterion.

# The final model from backward and forward step-wise selection is as below
fit.glm <- glm(state ~ main_category + country + backers + usd_goal_real + duration + textlength, data = ks.train, family = "binomial")
summary(fit.glm)
#-----------------------------------------------------------------------------------------------
#10-fold CV
#ctrl <- trainControl(method = "cv", number = 10)
#
#fit.model <- train(state ~ main_category + country + backers + usd_goal_real + duration + textlength, data = ks.train, 
#                   method = "glm", 
#                   family = "binomial", 
#                   trControl = ctrl)
#
#pred <- predict(fit.model, newdata = ks.test)
#confusionMatrix(data = pred, ks.test$state)
# Accuracy : 0.8974
#
# Classification Rate
#accuracy <- table(pred, t(ks.test[,"state"]))
#(1-sum(diag(accuracy))/sum(accuracy))*100 # Calculate misclassification rate
# 10.25641%
#-----------------------------------------------------------------------------------------------
# Parameters tuning & 10-fold CV
grid.glmnet <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1), lambda = seq(.01, .2, length = 20))
ctrl.glmnet <- trainControl(method = "cv", number = 10)
fit.glmnet <- train(state ~ main_category + country + backers + usd_goal_real + duration + textlength, data = ks.train,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = grid.glmnet,
                    trControl = ctrl.glmnet)

fit.glmnet
### 1.0    0.01    0.7859376  0.5396868744
### Accuracy was used to select the optimal model using the largest value.
### The final values used for the model were alpha = 1 and lambda = 0.01.

trellis.par.set(caretTheme())
plot(fit.glmnet, scales = list(x = list(log = 2)))

pred.glmnet <- predict(fit.glmnet, newdata = ks.test)
confusionMatrix(data = pred.glmnet, ks.test$state)
# Accuracy : 0.7889 
accuracy.glmnet <- table(pred.glmnet, t(ks.test[,"state"]))
(1-sum(diag(accuracy.glmnet))/sum(accuracy.glmnet))*100 # Calculate misclassification rate
# 21.11162%
#-----------------------------------------------------------------------------------------------