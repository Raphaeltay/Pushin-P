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
library(randomForest)
library(gbm)
library(ISLR)
library(Rcpp)
library(ROSE)
Tourn<-read.csv(file = "Tournament stats 2021.csv", header = T, sep = ",", na.strings = c("","NA"))
#FW metrics
#Top 5
FW.T<-Tourn %>% filter(Pos == list("FW","FWMF","FWDF")) %>% select(7:71, Top5)
FW.T.both<-ovun.sample(Top5~., data = FW.T, method = "both", N = nrow(FW.T), seed = 156)$data
table(FW.T.both$Top5)
FWTdt.b<-rpart(formula = Top5 ~ ., data = FW.T.both, method = "class")
vip(FWTdt.b, num_features = 90)
#Bottom 5
FW.B<-Tourn %>% filter(Pos == list("FW","FWMF","FWDF")) %>% select(7:71, Bot5)
FW.B.both<-ovun.sample(Bot5~., data = FW.B, method = "both", N = nrow(FW.B), seed = 156)$data
table(FW.B.both$Bot5)
FWBdt.b<-rpart(formula = Bot5 ~ ., data = FW.B.both, method = "class")
vip(FWBdt.b, num_features = 90)

#MF metrics
#Top 5
MF.T<-Tourn %>% filter(Pos == list("MF","MFFW","MFDF")) %>% select(7:71, Top5)
MF.T.both<-ovun.sample(Top5~., data = MF.T, method = "both", N = nrow(MF.T), seed = 156)$data
table(MF.T.both$Top5)
MFTdt.b<-rpart(formula = Top5 ~ ., data = MF.T.both, method = "class")
vip(MFTdt.b, num_features = 90)
#Bottom 5
MF.B<-Tourn %>% filter(Pos == list("MF","MFFW","MFDF")) %>% select(7:71, Bot5)
MF.B.both<-ovun.sample(Bot5~., data = MF.B, method = "both", N = nrow(MF.B), seed = 156)$data
table(MF.B.both$Bot5)
MFBdt.b<-rpart(formula = Bot5 ~ ., data = MF.B.both, method = "class")
vip(MFBdt.b, num_features = 90)

#DF metrics
#Top 5
DF.T<-Tourn %>% filter(Pos == list("DF","DFFW","DFMF")) %>% select(7:71, Top5)
DF.T.both<-ovun.sample(Top5~., data = DF.T, method = "both", N = nrow(DF.T), seed = 156)$data
table(DF.T.both$Top5)
DFTdt.b<-rpart(formula = Top5 ~ ., data = DF.T.both, method = "class")
vip(DFTdt.b, num_features = 90)
#Bottom 5
DF.B<-Tourn %>% filter(Pos == list("DF","DFFW","DFDF")) %>% select(7:71, Bot5)
DF.B.both<-ovun.sample(Bot5~., data = DF.B, method = "both", N = nrow(DF.B), seed = 156)$data
table(DF.B.both$Bot5)
DFBdt.b<-rpart(formula = Bot5 ~ ., data = DF.B.both, method = "class")
vip(DFBdt.b, num_features = 90)

#GK metrics
#Top 5
GK.T<-Tourn %>% filter(Pos == list("GK")) %>% select(7:71, Top5)
GK.T.both<-ovun.sample(Top5~., data = GK.T, method = "both", N = nrow(GK.T), seed = 156)$data
table(GK.T.both$Top5)
GKTdt.b<-rpart(formula = Top5 ~ ., data = GK.T.both, method = "class")
vip(GKTdt.b, num_features = 90)
#Bottom 5
GK.B<-Tourn %>% filter(Pos == list("GK")) %>% select(7:71, Bot5)
GK.B.both<-ovun.sample(Bot5~., data = GK.B, method = "both", N = nrow(GK.B), seed = 156)$data
table(GK.B.both$Bot5)
GKBdt.b<-rpart(formula = Bot5 ~ ., data = GK.B.both, method = "class")
vip(GKBdt.b, num_features = 90)
