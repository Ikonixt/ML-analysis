library(tidyverse)
library(factoextra)
library(clValid)
library(rgl)
games<-read.csv("vgsales-12-4-2019.csv")
extracted<-games%>%select(Critic_Score,Global_Sales)
extracted2<-extracted%>%filter(!is.na(Critic_Score),!is.na(Global_Sales))
times<-1:15
testcluster<-sapply(times,function(k){
  cl<-kmeans(extracted2,k,100)
  cl$tot.withinss
})
plot(times, testcluster, type="b")
cl<-kmeans(extracted2,3,25)
cl
plot(extracted2,col=cl$cluster)
fviz_nbclust(extracted2, FUNcluster = kmeans, method = "silhouette")
cluster.stats
dunn1
cl$size