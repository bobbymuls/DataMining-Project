ibrary(glmnet)
library(readxl)
library(caret)

ks <- read_excel("ks.xlsx")
ks$state <- as.factor(ks$state)
ks$launched_year <- as.factor(ks$launched_year)
ks$launched_month <- as.factor(ks$launched_month)

View(ks)
attach(ks)

#Creation of training (75%) and test (25%) set
set.seed(4510)
train <- sample(nrow(ks), 0.75 * nrow(ks))
ks.train <- ks[train, ]
ks.test <- ks[-train, ]

### A full logistic regression model was built with State as the response variable against the rest of the 
### 9 predictor variables - Main Category, Country, Launched Year, Launched Month, Duration, Backers,Amount Pledged, Goal and Textlength. 
### Step-wise variable selection method was used to identify the most important variables in the logistic regression model.

nullmodel <- glm(state~1, data=ks.train, family="binomial")
fullmodel <- glm(state~., data=ks.train, family="binomial")

#Backward step-wise selection
backward.model <- step(fullmodel, direction="backward")

#Forward step-wise selection
forward.model <- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction = 'forward')

### Both backward and forward step-wise selection methods give us the same final model.
### The logistic regression model is chonsen with the lowest AIC of 1888.27 as criterion.

#The final model from backward and forward step-wise selection is as below
fit.glm <- glm(state ~ main_category+launched_year+duration+backers+usd_pledged_real+usd_goal_real+textlength, 
               data=ks.train, family="binomial")
summary(fit.glm)

ctrl <- trainControl(method="repeatedcv", number=10, savePredictions=TRUE)

fit.model <- train(state ~ main_category+launched_year+duration+backers+usd_pledged_real+usd_goal_real+textlength, data=ks.train, 
                   method="glm", family="binomial", trControl=ctrl, tuneLength=10)

pred <- predict(fit.model, newdata=ks.test)
confusionMatrix(data=pred, ks.test$state)
### 99.92% accuracy?


#Confusion Matrix and Statistics
#
#Reference
#Prediction     0     1
#0 49247     0
#1    67 33553
#
#Accuracy : 0.9992         
#95% CI : (0.999, 0.9994)
#No Information Rate : 0.5951         
#P-Value [Acc > NIR] : < 2.2e-16      
#
#Kappa : 0.9983         
#Mcnemar's Test P-Value : 7.433e-16      
#
#Sensitivity : 0.9986         
#Specificity : 1.0000         
#Pos Pred Value : 1.0000         
#Neg Pred Value : 0.9980         
#Prevalence : 0.5951         
#Detection Rate : 0.5943         
#Detection Prevalence : 0.5943         
#Balanced Accuracy : 0.9993         
#
#'Positive' Class : 0          


















