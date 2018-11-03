rm(list = ls())

library(neuralnet)
library(data.table)
library(mltools)

dataset = read.csv("ks_project_2018_new.csv")

drops = c("X",
          "name",
          "category",
          "launched_year",
          "launched_day",
          "deadline_year",
          "deadline_month",
          "deadline_day",
          "currency",
          "usd.pledged",
          "usd_goal_real",
          "main_category",
          "country"
)

drops2 = c("backers",
           "goal",
           "launched_month",
           "pledged",
           "usd_pledged_real",
           "duration",
           "textlength"
)

dataset$state = as.numeric(as.character(factor(dataset$state,
                                               levels = c('failed', 'successful'),
                                               labels = c(0, 1))))

dataset_h1 = one_hot(as.data.table(dataset$main_category))
dataset_h2 = one_hot(as.data.table(dataset$country))

normalize = function(x)
{
  return ((x - min(x)) / (max(x)-min(x)))
}

dataset = dataset[,!names(dataset) %in% drops]

dataset2 = as.data.frame(lapply(dataset[,c(1,2,3,4,5,7,8)],normalize))

state = dataset[,!names(dataset) %in% drops2]

dataset = cbind(state,dataset_h1,dataset_h2,dataset2)

#Renaming of variables after one hot encoding
names(dataset)[2:16] = c("Arts","Comics","Crafts","Dance","Design","Fashion","Film_and_Video","Food",
                         "Games","Journalism","Music","Photography","Publishing","Technology","Theater")
names(dataset)[17:38] = c("AT","AU","BE","CA","CH","DE","DK","ES","FR","GB","HK","IE","IT","JP",
                          "LU","MX","NL","NO","NZ","SE","SG","US")
#str(dataset)
#Ensure all data are of numeric to run neuralnet package
{
  dataset$Arts = as.numeric(dataset$Arts)
  dataset$Comics = as.numeric(dataset$Comics)
  dataset$Crafts = as.numeric(dataset$Crafts)
  dataset$Dance = as.numeric(dataset$Dance)
  dataset$Design = as.numeric(dataset$Design)
  dataset$Fashion = as.numeric(dataset$Fashion)
  dataset$Film_and_Video = as.numeric(dataset$Film_and_Video)
  dataset$Food = as.numeric(dataset$Food)
  dataset$Games = as.numeric(dataset$Games)
  dataset$Journalism = as.numeric(dataset$Journalism)
  dataset$Music = as.numeric(dataset$Music)
  dataset$Photography = as.numeric(dataset$Photography)
  dataset$Publishing = as.numeric(dataset$Publishing)
  dataset$Technology = as.numeric(dataset$Technology)
  dataset$Theater = as.numeric(dataset$Theater)
  
  dataset$AT = as.numeric(dataset$AT)
  dataset$AU = as.numeric(dataset$AU)
  dataset$BE = as.numeric(dataset$BE)
  dataset$CA = as.numeric(dataset$CA)
  dataset$CH = as.numeric(dataset$CH)
  dataset$DE = as.numeric(dataset$DE)
  dataset$DK = as.numeric(dataset$DK)
  dataset$ES = as.numeric(dataset$ES)
  dataset$FR = as.numeric(dataset$FR)
  dataset$GB = as.numeric(dataset$GB)
  dataset$HK = as.numeric(dataset$HK)
  dataset$IE = as.numeric(dataset$IE)
  dataset$IT = as.numeric(dataset$IT)
  dataset$JP = as.numeric(dataset$JP)
  dataset$LU = as.numeric(dataset$LU)
  dataset$MX = as.numeric(dataset$MX)
  dataset$NL = as.numeric(dataset$NL)
  dataset$NO = as.numeric(dataset$NO)
  dataset$NZ = as.numeric(dataset$NZ)
  dataset$SE = as.numeric(dataset$SE)
  dataset$SG = as.numeric(dataset$SG)
  dataset$US = as.numeric(dataset$US)
}
str(dataset)
set.seed(4510)

#Select 1k random rows to test the code out
df = dataset
rNames = row.names(df)
samprows = sample(rNames,5000)
dataset = subset(df,rNames%in%samprows) 

# Random sampling
samplesize = 0.75 * nrow(dataset)
index = sample( seq_len ( nrow ( dataset ) ), size = samplesize )

## Fit neural network 
# creating training and test set
trainNN = dataset[index , ]
testNN = dataset[-index , ]

# fit neural network 
#2 layers of 7 neurons in the first layer in the lowest error terms for 5 repetitions
n = names(dataset)
f = as.formula(paste("state ~", paste(n[!n %in% "state"], collapse = " + ")))
nn = neuralnet(f,data = trainNN,hidden = 7,stepmax = 1e6,linear.output = F,lifesign = 'full', rep=5)

# plot neural network
#returns the matrix of the rep with the lowest error
nn$result.matrix[1,]
min(nn$result.matrix[1,])
#best outcome: lowest error term: rep = 1
plot(nn, rep = 1)

mypredict = compute(nn,testNN[,-1],rep = 1) #Compute predictions of test set
#Descaling as output is a normalized prediction, so we need to scale it back in order to make a more meaningful comparison
mypredict = (mypredict$net.result * (max(dataset$state) - min(dataset$state))) + min(dataset$state)
#print(head(mypredict)) #To check out net.result(dataset$state) of predictions
mypredictrounded = sapply(mypredict,round,digits=0) #to round off as State is either 0 or 1

#To compute confusion matrix
confmat = table(mypredictrounded, testNN$state)
misclassificationrate = (1-sum(diag(confmat))/sum(confmat))*100
(accuracy = 100-misclassificationrate)

#mypredictrounded    
#   0      1
#0 740    17
#1  10    483
#Accuracy = 97.84%


