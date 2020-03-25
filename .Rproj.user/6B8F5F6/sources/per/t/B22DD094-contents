library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)
library(ranger)
train<-read_excel("nettrain.xlsx",header = TRUE,stringAsFactors = FALSE)
view(train)

test<-read_excel("nettest.xlsx")
view(test)

data_combined<-rbind(train,test)
data_combined

view(data_combined)

str(data_combined)

data_combined$rating<-as.factor(data_combined$rating)

str(data_combined)

data_combined$listed_in<-as.factor(data_combined$listed_in)

str(data_combined)

data_combined$type<-as.factor(data_combined$type)

str(data_combined)

glimpse(data_combined)

data_combined$country<-as.character(data_combined$country)

glimpse(data_combined)

str(data_combined)

data_combined$duration<-as.factor(data_combined$duration)

str(data_combined)

library(dplyr)
data_combined%>%
  group_by(country)%>%
  summarise(count= n())%>%
  top_n(10,country)%>%
  ggplot(data_combined,mapping =aes(x = country,y = count))+
  geom_col(binwidth = 0.5)+
  ggtitle("Various countries in producing films")

count(darating)


data_combined%>%
  group_by(country)%>%
  summarise(count= n())%>%
  mutate(vex = mean(count))%>%
 
ggplot(data_combined,mapping = aes(x = country,y = rating,fill = type))+
  geom_col()

data_combined%>%
  group_by(rating)%>%
  summarise(count= n())
#rating contains 1 NA
sum(is.na(data_combined$type))
sum(is.na(data_combined$release_year))

ggplot(data_combined,mapping = aes(x = rating,fill = type))+
  geom_histogram(stat = "count")

ggplot(data_combined,mapping = aes(x = release_year,color = type))+
  geom_histogram(stat = "count",binwidth = 0.5)
library(ranger)
form<-type~rating+release
form
class(form)
  
set.seed(1234)
model<-ranger(type~rating+release_year,nettrain,num.trees = 500,respect.unordered.factors = "order")
model
#pot<-as.data.frame(release_year = 2017,rating = "PG")
ft<-predict(model,data = nettest)
table(nettest$type,predictions(ft))
table(ft)

set.seed(1234)
library(nnet)
rt<-nnet(type~...,data = nettrain,size = 3)
rt

table(nettrain$type,predict(rt,nettrain,type = "class"))

