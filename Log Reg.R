library(readxl)
library(glmnet)
library(caret)
library(ROCR)
library(lmtest)

ks <- read_excel("kslatest.xlsx")
ks$state <- as.factor(ks$state)
ks$launched_month <- as.factor(ks$launched_month)

View(ks)
attach(ks)

#Creation of training (70%) and test (30%) set
set.seed(4510)
train <- sample(nrow(ks), 0.70 * nrow(ks))
ks.train <- ks[train, ]
ks.test <- ks[-train, ]
#-----------------------------------------------------------------------------------------------
### A full logistic regression model was built with State as the response variable against the rest of the 
### 7 predictor variables - Main Category, Country, Launched Month, Duration, Backers, Goal and Textlength. 
### Step-wise variable selection method was used to identify the most important variables in the logistic regression model.

nullmodel <- glm(state~1, data = ks.train, family = "binomial")
fullmodel <- glm(state~., data = ks.train, family = "binomial")

#Backward step-wise selection
backward.model <- step(fullmodel, direction = "backward")

#Forward step-wise selection
forward.model <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = 'forward')

### Both backward and forward step-wise selection methods give us the same final model.
### The logistic regression model is chonsen with the lowest AIC of 18871.39 as criterion.

##### Likelihood ratio test
##### mod.fit.one <- glm(state ~., data=ks.train, family="binomial")
##### mod.fit.two <- glm(state ~ main_category + country + backers + usd_goal_real + duration + textlength, data=ks.train, family="binomial")
##### anova(mod.fit.one, mod.fit.two, test ="Chisq")
##### lrtest(mod.fit.one, mod.fit.two)

#The final model from backward and forward step-wise selection is as below
fit.glm <- glm(state ~ main_category + country + backers + usd_goal_real + duration + textlength, data = ks.train, family = "binomial")
summary(fit.glm)
#-----------------------------------------------------------------------------------------------
#10-fold CV
ctrl <- trainControl(method = "cv", number = 10)

fit.model <- train(state ~ main_category + country + backers + usd_goal_real + duration + textlength, data = ks.train, 
                   method = "glm", 
                   family = "binomial", 
                   trControl = ctrl)

pred <- predict(fit.model, newdata = ks.test)
confusionMatrix(data = pred, ks.test$state)
### Accuracy : 0.8974

### Classification Rate
accuracy <- table(pred, t(ks.test[,"state"]))
(1-sum(diag(accuracy))/sum(accuracy))*100 # Calculate misclassification rate
### 10.25641%
#-----------------------------------------------------------------------------------------------
#Parameters tuning & 10-fold CV
grid.glmnet <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1), lambda = seq(.01, .2, length = 20))
ctrl.glmnet <- trainControl(method = "cv", number = 10)
fit.glmnet <- train(state ~ main_category + country + backers + usd_goal_real + duration + textlength, data = ks.train,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = grid.glmnet,
                    trControl = ctrl.glmnet)
fit.glmnet
### 1.0    0.01    0.7916955  0.5527473229
### Accuracy was used to select the optimal model using the largest value.
### The final values used for the model were alpha = 1 and lambda = 0.01.

trellis.par.set(caretTheme())
plot(fit.glmnet, scales = list(x = list(log = 2)))

pred.glmnet <- predict(fit.glmnet, newdata = ks.test)
confusionMatrix(data = pred.glmnet, ks.test$state)
# Accuracy : 0.7842 
accuracy.glmnet <- table(pred.glmnet, t(ks.test[,"state"]))
(1-sum(diag(accuracy.glmnet))/sum(accuracy.glmnet))*100 # Calculate misclassification rate
# 21.57992%
#-----------------------------------------------------------------------------------------------
# ROC Curve & AUC
pr <- prediction(pred.glmnet, ks.test$state)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc






