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
train <- read.csv("./input/train.csv", stringsAsFactors = F)
test <- read.csv("./input/test.csv", stringsAsFactors = F)
macro <- read.csv("./input/macro.csv", stringsAsFactors = F)
sample_submission <- read.csv("./input/sample_submission.csv", stringsAsFactors = F)

#Over 30000 observations and almost 300 variables
dim(train)
names(train)

head(train, 10)
tail(train, 10)

##Distribution of outcome variable
#Histotram of price_doc
ggplot(aes(x=price_doc), data=train) + 
  geom_histogram(fill=color, bins=20) + 
  ggtitle('Distribution of sale price')

#Histogram of log(price_doc)
ggplot(aes(x=log10(price_doc)), data=train) + 
  geom_histogram(fill=color, bins=20) + 
  ggtitle('Distribution of log(sale price)')

#Examine build_year variable
#Some of these years (0,1,2,3,20,71,215, and 20052009) do not make sense
table(train$build_year)

#House area vs. build year
train %>% 
  filter(build_year > 1691 & build_year < 2018) %>% 
  ggplot(aes(x=build_year, y=full_sq)) + 
  geom_point() + 
  ggtitle('Full area vs. year built')+
  geom_smooth(method="loess")

train %>% 
  filter(build_year > 1691 & build_year < 2018) %>% 
  ggplot(aes(x=build_year, y=life_sq)) + 
  geom_point() + 
  ggtitle('Life area vs. year built')+
  geom_smooth(method="loess")


#Correlation matrix 
internal_chars <- c('full_sq', 'life_sq', 'floor', 'max_floor', 'build_year', 'num_room', 
                    'kitch_sq', 'state', 'price_doc')

corrplot(cor(train[, internal_chars], use="complete.obs"))


#Prepare for PCA by subsetting out non-numerical variables
col.class<-sapply(train, class)
col.class.nonchar<-names(which(col.class!="character"))

train.pca<-train[,col.class.nonchar]

cor.matrix<-(cor(train.pca, train.pca, use="complete.obs"))

# Pricipal Components Analysis entering raw data and extracting PCs from the correlation matrix 
fit <- princomp(~ ., data = train.pca, cor = TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)
