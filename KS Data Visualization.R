rm(list = ls())

#install.packages("dplyr") 
#install.packages("tidyr")  
#install.packages("ggplot2")  
#install.packages("lubridate")
#install.packages("gridExtra")
#install.packages("corrplot") 

library("dplyr")     #data wrangling tasks
library("tidyr")     #data wrangling tasks
library("ggplot2")   #plotting and data visualization tasks
library("lubridate") #date and time manipulation
library("gridExtra") #arrangement of plots
library("corrplot")  #Correlation function

ks <- read.csv("ks_project_2018.csv") #data import
attach(ks)
names(ks)
str(ks) #for a compact display of ks data

#2. Data Cleansing Steps
#2a. Conversion of data type and reordering of variables order
ks$name <- as.character(ks$name)
ks$deadline <- as.Date(ks$deadline)
ks$launched <- as.Date(ks$launched)
ks <- ks[, c(1:4, 12, 8, 6, 5, 7, 9, 11, 13:15, 10)]

#2b. Subsetting of data
ggplot(ks, aes(state)) + geom_bar() + ylab("# of Projects") + xlab("Final State") + ggtitle("Final State of Kickstarter projects") #bar chart for all states
ks.proj <- ks %>% filter(state == "failed" | state == "successful") 
ks.proj$state <- as.character(ks.proj$state)
ks.proj$state <- as.factor(ks.proj$state)
summary(ks.proj$state)

#2e. Manipulation of features
#reducing levels in country
ks.proj$country <- as.character(ks.proj$country)
ks.proj$country[ks.proj$country %in% c("JP", "LU", "AT", "HK", "SG", "BE", "CH", "IE", "NO", "DK","MX", "NZ", "SE", "ES", "IT", "NL", "FR", "DE")] <- "Other"
ks.proj$country <- as.factor(ks.proj$country)
levels(ks.proj$country) #5 levels
sort(round(prop.table(table(ks.proj$country)),2)) #percentage of total for each level

#reducing levels in launched year
round(prop.table(table(ks.proj$launched_year)),2) #percentage of total for each level
ks.proj$launched_year <- as.character(ks.proj$launched_year)
ks.proj$launched_year[ks.proj$launched_year %in% c("2009", "2010", "2011")] <- "Before 2012"
ks.proj$launched_year <- as.factor(ks.proj$launched_year)

#reducing levels in currency
levels(ks.proj$currency)
sort(round(prop.table(table(ks.proj$currency)),2))
ks.proj$currency <- as.character(ks.proj$currency)
ks.proj$currency[ks.proj$currency %in% c("JPY", "HKD", "SGD", "CHF", "NOK", "DKK", "MXN", "NZD", "SEK")] <- "Other"
ks.proj$currency <- as.factor(ks.proj$currency)

#check the dimension and summary of finalised dataset
str(ks.proj) 

#3. Preliminary Analysis
#Final State of KS project
ggplot(ks.proj, aes(state, fill = state)) + geom_bar() + ylab("# of Projects") + xlab("Final State") + theme(legend.position = "bottom") + ggtitle("Final State of Kickstarter projects")
sort(round(prop.table(table(ks.proj$state)),2)) #percentage of total for state levels

#Main Categories present in the dataset
p2 <- ggplot(ks.proj, aes(x = main_category, fill = ks.proj$state)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("") +
  ggtitle("Main categories of KS Projects")  

#Main categories of the KS Projects - percent to whole
p1 <- ks.proj %>% 
  count(main_category) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(reorder(main_category, pct), pct)) +
  geom_col() +
  coord_flip() + 
  ylab("% of projects") + xlab("") +
  ggtitle("Main categories of KS Projects") 

gridExtra::grid.arrange(p1, p2, ncol = 2) #join the 2 plots side by side

#Plot for duration
p1 <- ggplot(ks.proj, aes(duration, fill = ks.proj$state)) +
  geom_histogram(binwidth = 5) +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("Duration") +
  ggtitle("Duration of KS projects (in days)")

p2 <- ggplot(ks.proj, aes(x = state, y = duration, fill = ks.proj$state)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  coord_flip() +
  xlab("") + ylab("Duration") +
  ggtitle("Active Duration of KS projects")

gridExtra::grid.arrange(p1, p2, ncol = 2)

#Plot for backers
quantile(ks.proj$backers, probs = seq(from = 0, to = 1, by = .1)) #quantiles for backers data
#Log transforming the backers data
p1 <- ggplot(ks.proj, aes(log(backers),  fill = ks.proj$state)) +
  geom_density() +
  theme(legend.position = "bottom") +
  ylab("Number of Backers") + xlab("") +
  ggtitle("# of Backers of KS projects")

p2 <- ggplot(ks.proj, aes(x = state, y = log(backers), fill = ks.proj$state)) +
  geom_boxplot() +
  coord_flip() + 
  theme(legend.position = "bottom") +
  ylab("# of Backers (log-transformed)") + xlab("") +
  ggtitle("# of Backers of the KS projects (Log)")

gridExtra::grid.arrange(p1, p2, ncol = 2)

#Plot for amount pledged
# Log transforming the usd_pledged_real data
p1 <- ggplot(ks.proj, aes(log(usd_pledged_real),  fill = ks.proj$state)) +
  geom_density() +
  theme(legend.position = "bottom") +
  xlab("USD pledged (log-transformed)") + ylab("") +
  ggtitle("USD pledged for KS projects")

# Log-transformed usd_pledged_real
p2 <- ggplot(ks.proj, aes(x = state, y = log(usd_pledged_real), fill = ks.proj$state)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ylab("USD pledged (log-transformed)") + xlab("") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() + 
  ggtitle("USD pledged for KS projects (Log)")

gridExtra::grid.arrange(p1, p2, ncol = 2)

# Correlation between all numerical variables
corMat <- cor(ks.proj[, c(9, 13, 15, 17:20)])
corrplot.mixed(corMat,tl.pos = "lt")
               
