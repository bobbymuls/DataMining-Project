rm(list = ls())

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

dataset$state = as.numeric(as.character(factor(dataset$state,
                                              levels = c('failed', 'successful'),
                                              labels = c(0, 1))))
str(dataset)
dataset[1:10,]

## Scale data for neural network
max = apply(dataset , 2 , max)
min = apply(dataset, 2 , min)
scaled = as.data.frame(scale(dataset, center = min, scale = max - min))

# Random sampling
samplesize = 0.60 * nrow(dataset)
set.seed(4510)
index = sample( seq_len ( nrow ( dataset ) ), size = samplesize )

# Create training and test set
datatrain = dataset[ index, ]
datatest = dataset[ -index, ]

## Fit neural network 

# install library
install.packages("neuralnet")
# load library
library(neuralnet)

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network 
#stepmax increased from 1e5 to 1e6
nn <- neuralnet(state~main_category+country+goal+pledged+backers+usd_pledged_real+duration+textlength,
                data = trainNN,hidden = 5,stepmax = 1e6,linear.output = F)
                
#if the code runs faster, repeat nn trainings
#nn <- neuralnet(state~main_category+country+goal+pledged+backers+usd_pledged_real+duration+textlength,
                #data = trainNN,hidden = 5,stepmax = 1e6,linear.output = F, rep=5)
                
##Currently cannot find the weights as the algorithm did not converge

# plot neural network
plot(nn)
