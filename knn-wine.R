library(tidyverse)
library(class)
data<-read.csv("winequality-red.csv")
view(data)
data2<-data%>%select(fixed.acidity,volatile.acidity,citric.acid,pH,quality)
data2<-data2%>%filter(!is.na(fixed.acidity),!is.na(volatile.acidity),!is.na(citric.acid),!
                        is.na(pH))
split=0.8
training<-sample(nrow(data2),size=split*nrow(data2),replace=FALSE)
testing<-setdiff(seq_len(nrow(data2)),training)
result<-knn(data2[training,-5],data2[testing,-5],data2$quality[training])
comparison<-result==data2$quality[testing]
sum(comparison)
set.volatile.acidity<-c(0.76,0.88,0.2,0.11)
fixed.acidity<-c(10.1,11.6,3.2,2.1)
citric.acid<-c(0.99,0.71,0.01,0.3)
pH<-c(3.01,3.04,3.11,3.21)
data3<-data.frame(fixed.acidity,volatile.acidity,citric.acid,pH)
result2<-knn(data2[training,-5],data3,data2$quality[training])
data2[testing,] %>%group_by(quality)%>%summarise(count=n())
summary(comparison)
library(caret)
ref<-as.factor(data2$quality[testing])
result22<-as.factor(result)
confusionMatrix(result22,ref)
conf<-confusionMatrix(result22,ref)
conf$byClass