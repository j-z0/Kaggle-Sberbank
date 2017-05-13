#Sberbank Russian Housing Market
library(Hmisc)
library(tidyverse)
library(caret)
color<-"coral1"

#Load data
dtrain<-read.csv("train.csv", stringsAsFactors = FALSE)

#Over 30000 observations and almost 300 variables
dim(dtrain)
names(dtrain)

head(dtrain, 10)
tail(dtrain, 10)

#Histotram of price_doc (outcome variable)
ggplot(aes(x=price_doc), data=dtrain) + 
  geom_histogram(fill=color, bins=20) + 
  ggtitle('Distribution of sale price')

ggplot(aes(x=log10(price_doc)), data=dtrain) + 
  geom_histogram(fill=color, bins=20) + 
  ggtitle('Distribution of log(sale price)')
