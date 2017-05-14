#Sberbank Russian Housing Market
library(data.table)
library(tidyverse)
library(lubridate)
library(scales)
library(corrplot)
library(DT)
library(Hmisc)
color<-"cornflowerblue"

#Load data
dtrain<-read.csv("train.csv", stringsAsFactors = FALSE)

#Over 30000 observations and almost 300 variables
dim(dtrain)
names(dtrain)

head(dtrain, 10)
tail(dtrain, 10)

##Distribution of outcome variable
#Histotram of price_doc
ggplot(aes(x=price_doc), data=dtrain) + 
  geom_histogram(fill=color, bins=20) + 
  ggtitle('Distribution of sale price')

#Histogram of log(price_doc)
ggplot(aes(x=log10(price_doc)), data=dtrain) + 
  geom_histogram(fill=color, bins=20) + 
  ggtitle('Distribution of log(sale price)')

#
dtrain %>% 
  filter(build_year > 1691 & build_year < 2018) %>% 
  ggplot(aes(x=build_year, y=full_sq)) + 
  geom_point() + 
  ggtitle('Full area vs. year built')


#Correlation matrix 
internal_chars <- c('full_sq', 'life_sq', 'floor', 'max_floor', 'build_year', 'num_room', 
                    'kitch_sq', 'state', 'price_doc')

corrplot(cor(dtrain[, internal_chars], use="complete.obs"))


#Prepare for PCA by subsetting out non-numerical variables
col.class<-sapply(dtrain, class)
col.class.nonchar<-names(which(col.class!="character"))

dtrain.pca<-dtrain[,col.class.nonchar]


# Pricipal Components Analysis entering raw data and extracting PCs from the correlation matrix 
fit <- princomp(~ ., data = dtrain.pca, cor = TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)
