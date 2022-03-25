setwd('C:/Users/rapta/OneDrive/Documents/ACTL4001/Assignment/Useable files/')
library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(rpart)
library(caret)
library(rpart.plot)
library(vip)
library(pdp)
library(ROSE)
library(randomForest)
library(gbm)
library(ISLR)
library(Rcpp)
dat<-read.csv(file = "Monte Carlo Actuals.csv", header = T, sep = ",", na.strings = c("","NA"))
Rar.dat<-read.csv(file = "Rar dat.csv", header = T, sep = ",", na.strings = c("","NA"))
#Split into Test and Training

probs<-c()
for(j in 1: 50){
  ref<-round(runif(n =100, min = 0, max = 1)*1000, digits = 0)
  prob<-c()
  for(i in 1: length(ref)){
    set.seed(ref[i])
    idx<-sample(2,nrow(dat),replace = T, prob = c(0.8,0.2))
    trn<-dat[idx==1,]
    tst<-dat[idx==2,]
    table(trn$Top5)
    bal.dat<-ovun.sample(Top5~., data = trn, method = "both", N = nrow(trn)*4, seed = 143)$data
    table(bal.dat$Top5)
    dt<-rpart(formula = Top5 ~., data = bal.dat, method = "class")
    rpart.plot(dt)
    tst_pred<-predict(dt, tst, type = "class")
    table(predicted = tst_pred, actual = tst$Top5)
    vip(dt, num_features = 90)
    boost<-gbm(ifelse(Top5== "Yes",1,0)~., data = bal.dat, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.001)
    prob[i]<-ifelse(predict(boost, newdata = Rar.dat)>0.5,1,0)
  }
  probs[j]<-sum(prob)/length(ref)
}
sd(probs)
mean(probs)
