library(glmnet)
library(readxl)

ks <- read_excel("ks.xlsx")

#View(ks)
attach(ks)

#Creation of training, test and validation set @(60/20/20)
set.seed(12420360)
prop <- c(train=0.6, test=0.2, validate=0.2)
tag <- sample(cut(seq(nrow(ks)), nrow(ks)*cumsum(c(0, prop)), labels=names(prop))) #tagging the obs randomly based on prop

set <- split(ks, tag)
train.set <- set$train
test.set <- set$test
validation.set <- set$validate

### A full logistic regression model was built with State as the response variable against the rest of the 
### 9 predictor variables - Main Category, Country, Launched Year, Launched Month, Duration, Backers,Amount Pledged, Goal and Textlength. 
### Step-wise variable selection method was used to identify the most important variables in the logistic regression model.

nullmodel <- glm(state~1, data=train.set, family="binomial")
fullmodel <- glm(state~., data=train.set, family="binomial")

#Backward step-wise selection
backward.model <- step(fullmodel, direction="backward")

#Start:  AIC=1049.81
#state ~ main_category + country + launched_year + launched_month + 
#  duration + currency + backers + usd_pledged_real + usd_goal_real + 
#  textlength
#
#
#Step:  AIC=1049.81
#state ~ main_category + country + launched_year + launched_month + 
#  duration + backers + usd_pledged_real + usd_goal_real + textlength
#
#Df Deviance    AIC
#- country          21      976   1020
#- launched_month    1      965   1049
#<none>                     964   1050
#- duration          1      967   1051
#- launched_year     1      968   1052
#- textlength        1      971   1055
#- backers           1      972   1056
#- main_category    14     1003   1061
#- usd_pledged_real  1   123373 123457
#- usd_goal_real     1   173215 173299
#
#Step:  AIC=1020.13
#state ~ main_category + launched_year + launched_month + duration + 
#  backers + usd_pledged_real + usd_goal_real + textlength
#
#Df Deviance    AIC
#- launched_month    1      977   1019
#<none>                     976   1020
#- launched_year     1      980   1022
#- duration          1      980   1022
#- textlength        1      984   1026
#- backers           1      985   1027
#- main_category    14     1017   1033
#- usd_pledged_real  1   123546 123588
#- usd_goal_real     1   173491 173533
#
#Step:  AIC=1018.6
#state ~ main_category + launched_year + duration + backers + 
#  usd_pledged_real + usd_goal_real + textlength
#
#Df Deviance    AIC
#<none>                     977   1019
#- launched_year     1      980   1020
#- duration          1      980   1020
#- textlength        1      985   1025
#- backers           1      986   1026
#- main_category    14     1018   1032
#- usd_pledged_real  1   123553 123593
#- usd_goal_real     1   173569 173609

#Forward step-wise selection
forward.model <- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction = 'forward')

#Start:  AIC=268224.2
#state ~ 1
#
#Df Deviance    AIC
#+ backers           1   190220 190224
#+ usd_pledged_real  1   208657 208661
#+ main_category    14   258852 258882
#+ usd_goal_real     1   260086 260090
#+ duration          1   265449 265453
#+ textlength        1   265727 265731
#+ country          21   266872 266916
#+ currency         13   267021 267049
#+ launched_year     1   267196 267200
#+ launched_month    1   268198 268202
#<none>                  268222 268224
#
#Step:  AIC=190224
#state ~ backers
#
#Df Deviance    AIC
#+ usd_goal_real     1   129495 129501
#+ main_category    14   177586 177618
#+ duration          1   187547 187553
#+ launched_year     1   188804 188810
#+ country          21   189339 189385
#+ currency         13   189409 189439
#+ textlength        1   190076 190082
#+ usd_pledged_real  1   190137 190143
#+ launched_month    1   190142 190148
#<none>                  190220 190224
#
#Step:  AIC=129500.7
#state ~ backers + usd_goal_real
#
#Df Deviance    AIC
#+ usd_pledged_real  1     1036   1044
#+ main_category    14   124239 124273
#+ duration          1   129119 129127
#+ launched_year     1   129184 129192
#+ country          21   129151 129199
#+ currency         13   129193 129225
#+ textlength        1   129342 129350
#+ launched_month    1   129479 129487
#<none>                  129495 129501
#
#Step:  AIC=1043.69
#state ~ backers + usd_goal_real + usd_pledged_real
#
#Df Deviance    AIC
#+ main_category  14   992.15 1028.2
#+ textlength      1  1023.54 1033.5
#+ launched_year   1  1031.40 1041.4
#<none>               1035.69 1043.7
#+ duration        1  1034.22 1044.2
#+ launched_month  1  1034.94 1044.9
#+ currency       13  1024.22 1058.2
#+ country        21  1021.29 1071.3
#
#Step:  AIC=1028.15
#state ~ backers + usd_goal_real + usd_pledged_real + main_category
#
#Df Deviance    AIC
#+ textlength      1   982.96 1021.0
#+ launched_year   1   988.79 1026.8
#+ duration        1   989.17 1027.2
#<none>                992.15 1028.2
#+ launched_month  1   991.75 1029.8
#+ currency       13   983.56 1045.6
#+ country        21   978.75 1056.8
#
#Step:  AIC=1020.96
#state ~ backers + usd_goal_real + usd_pledged_real + main_category + 
#  textlength
#
#Df Deviance    AIC
#+ duration        1   979.97 1020.0
#+ launched_year   1   980.18 1020.2
#<none>                982.96 1021.0
#+ launched_month  1   982.80 1022.8
#+ currency       13   975.11 1039.1
#+ country        21   971.03 1051.0
#
#Step:  AIC=1019.97
#state ~ backers + usd_goal_real + usd_pledged_real + main_category + 
#  textlength + duration
#
#Df Deviance    AIC
#+ launched_year   1   976.60 1018.6
#<none>                979.97 1020.0
#+ launched_month  1   979.76 1021.8
#+ currency       13   972.31 1038.3
#+ country        21   968.42 1050.4
#
#Step:  AIC=1018.6
#state ~ backers + usd_goal_real + usd_pledged_real + main_category + 
#  textlength + duration + launched_year
#
#Df Deviance    AIC
#<none>                976.60 1018.6
#+ launched_month  1   976.13 1020.1
#+ currency       13   968.77 1036.8
#+ country        21   965.03 1049.0

### Both backward and forward step-wise selection methods give us the same final model.
### The logistic regression model is chonsen with the lowest AIC of 1018.6 as criterion.

#The final model from backward and forward step-wise selection is as below
glm.model <- glm(state ~ main_category+launched_year+duration+backers+usd_pledged_real+usd_goal_real+textlength, data=train.set, family="binomial")
summary(glm.model)

#Call:
#  glm(formula = state ~ main_category + launched_year + duration + 
#        backers + usd_pledged_real + usd_goal_real + textlength, 
#      family = "binomial", data = train.set)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-8.490   0.000   0.000   0.000   6.565  
#
#Coefficients:
#                            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)               200.087168  95.375255   2.098  0.03591 *  
#main_categoryComics         0.448784   0.749215   0.599  0.54917    
#main_categoryCrafts        -0.367159   0.408444  -0.899  0.36869    
#main_categoryDance          1.745263   1.057725   1.650  0.09894 .  
#main_categoryDesign        -0.055592   0.445255  -0.125  0.90064    
#main_categoryFashion        0.546677   0.465772   1.174  0.24051    
#main_categoryFilm & Video   1.000687   0.320861   3.119  0.00182 ** 
#main_categoryFood          -1.406151   0.255667  -5.500 3.80e-08 ***
#main_categoryGames         -0.595662   0.370579  -1.607  0.10797    
#main_categoryJournalism     0.593732   0.870777   0.682  0.49534    
#main_categoryMusic          0.514271   0.302421   1.701  0.08903 .  
#main_categoryPhotography   -0.329702   0.399214  -0.826  0.40887    
#main_categoryPublishing    -0.234465   0.292450  -0.802  0.42271    
#main_categoryTechnology    -0.076684   0.485381  -0.158  0.87447    
#main_categoryTheater        1.335558   0.618936   2.158  0.03094 *  
#launched_year              -0.098240   0.047331  -2.076  0.03793 *  
#duration                   -0.011938   0.005760  -2.073  0.03821 *  
#backers                     0.039111   0.005903   6.626 3.46e-11 ***
#usd_pledged_real            0.241412   0.011058  21.832  < 2e-16 ***
#usd_goal_real              -0.241357   0.011070 -21.802  < 2e-16 ***
#textlength                  0.104874   0.032182   3.259  0.00112 ** 
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 268222.2  on 198878  degrees of freedom
#Residual deviance:    976.6  on 198858  degrees of freedom
#AIC: 1018.6
#
#Number of Fisher Scoring iterations: 25

### The results above implies that if a KS project is launched under Comics category,
### the odds of its success increase by 57%

#Parameter tuning
### pcut - optimal cut-off probability for classifying projects into successful or failure class, a 10-fold cross-validation (CV) method is used on training set.
### pcut is calculated by using a symmetric cost function for wrongly classified KS projects and is the probability where the logistic regression model has the least misclassification rate.

#Logistic Regression - Parameter Tuning
#CV to choose pcut
search.grid <- seq(0.4, 0.7, 0.02)
result <- cbind(search.grid, NA)
cost0 <- function(r, pi) {
  weight1 <- 1
  weight0 <- 1
  c1 <- (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0 (False Negative)
  c0 <- (r == 0) & (pi > pcut)  #logical vector - true if actual 0 but predict 1 (False Positive)
  return(mean(weight1*c1 + weight0*c0))
}

for (i in 1:length(search.grid)) {
  pcut <- result[i, 1]
  result[i, 2] <- cv.glm(data=train.data, glmfit=glm.model, cost=cost0, K = 10)$delta[2]
}

plot(result, ylab="Cost of CV", main="Optimal cut-off probability identification")

























