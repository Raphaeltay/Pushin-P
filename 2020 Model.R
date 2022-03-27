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
dat.20<-read.csv(file = "2020 data.csv", header = T, sep = ",", na.strings = c("","NA"))
dat.21<-read.csv(file = "2021 data.csv", header = T, sep = ",", na.strings = c("","NA"))
#FWs 2020
FW.20<-dat.20 %>% filter(Pos == list("FW","FWMF","FWDF")) %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
FW.p.20<-FW.20 %>% select(1:65)
#PCA 
FWpca.20<-prcomp(na.omit(FW.p.20), center = T, scale. = T)
summary(FWpca.20)
plot(FWpca.20, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
fviz_nbclust(as.data.frame(-FWpca.20$x[,1:2]),kmeans, method = "wss")
fviz_nbclust(as.data.frame(-FWpca.20$x[,1:2]),kmeans, method = "silhouette")
fviz_nbclust(as.data.frame(-FWpca.20$x[,1:2]),kmeans, method = "gap_stat")
k = 2
kmeans.pca.20<-kmeans(as.data.frame(-FWpca.20$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.20,as.data.frame(-FWpca.20$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(FW.20),replace = T, prob = c(0.8,0.2))
FWtrn<-FW.20[idx==1,]
FWtst<-FW.20[idx==2,]
table(FWtrn$Tournament)
#Balancing Data
library(ROSE)
FWover<-ovun.sample(Tournament~., data = FWtrn, method = "over", N = nrow(FWtrn), seed = 122)$data
FWunder<-ovun.sample(Tournament~., data = FWtrn, method = "under", N = nrow(FWtrn)*0.7, seed = 125)$data
FWboth<-ovun.sample(Tournament~., data = FWtrn, method = "both", N = nrow(FWtrn), seed = 156)$data
table(FWover$Tournament) 
table(FWunder$Tournament) 
table(FWboth$Tournament)
FWover<-FWover %>% select(1:66)
FWunder<-FWunder %>% select(1:66)
FWboth<-FWboth %>% select(1:66)
#Tree Model
#Oversampling
FWdt.o.20<-rpart(formula = Tournament ~ ., data = FWover, method = "class")
rpart.plot(FWdt.o.20)
FW_tst_pred.o<-predict(FWdt.o.20, FWtst, type = "class")
table(predicted = FW_tst_pred.o, actual = FWtst$Tournament)
vip(FWdt.o.20, num_features = 90)
FWtree_acc.o<- calc_acc(predicted = FW_tst_pred.o, actual = FWtst$Tournament)
FWtree_acc.o
#Undersampling
FWdt.u.20<-rpart(formula = Tournament ~ ., data = FWunder, method = "class")
rpart.plot(FWdt.u.20)
FW_tst_pred.u<-predict(FWdt.u.20, FWtst, type = "class")
table(predicted = FW_tst_pred.u, actual = FWtst$Tournament)
vip(FWdt.u.20, num_features = 90)
FWtree_acc.u<- calc_acc(predicted = FW_tst_pred.u, actual = FWtst$Tournament)
FWtree_acc.u
#Both
FWdt.b.20<-rpart(formula = Tournament ~ ., data = FWboth, method = "class")
rpart.plot(FWdt.b.20)
FW_tst_pred.b<-predict(FWdt.b.20, FWtst, type = "class")
table(predicted = FW_tst_pred.b, actual = FWtst$Tournament)
vip(FWdt.b.20, num_features = 10)
FWtree_acc.b<- calc_acc(predicted = FW_tst_pred.b, actual = FWtst$Tournament)
FWtree_acc.b
#Bagging
library(randomForest)
library(gbm)
library(ISLR)
library(Rcpp)
#Tuning
set.seed(825)
oob<-trainControl(method = "oob")
cv_5<-trainControl(method = "cv", number = 5)
rf_grid =  expand.grid(mtry = 1:100)
FWrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(FWboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
FW_bag<-randomForest(as.factor(Tournament)~., data = na.omit(FWboth), mtry = ncol(FWboth)-1, importance = T, ntrees = 500)
FW_rf<-randomForest(as.factor(Tournament)~., data = na.omit(FWboth), mtry = as.numeric(FWrf_tune$bestTune), importance = T, ntrees = 500)

#Gradient Boosting Model
FW_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~., data = FWboth, distribution = "bernoulli", n.trees = 500, interaction.depth = 4, shrinkage = 0.01)
FW_boost_pred<-ifelse(predict(FW_boost, FWtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = FW_boost_pred, actual = FWtst$Tournament)
FWboost_acc<- calc_acc(predicted = FW_boost_pred, actual = FWtst$Tournament)
FWboost_acc

#DFs 2020
DF.20<-dat.20 %>% filter(Pos == list("DF","DFFW","DFMF")) %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
DF.p.20<-DF.20 %>% select(1:65)
#PCA 
DFpca.20<-prcomp(na.omit(DF.p.20), center = T, scale. = T)
summary(DFpca.20)
plot(DFpca.20, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
k = 2
kmeans.pca.20<-kmeans(as.data.frame(-DFpca.20$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.20,as.data.frame(-DFpca.20$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(DF.20),replace = T, prob = c(0.8,0.2))
DFtrn<-DF.20[idx==1,]
DFtst<-DF.20[idx==2,]
table(DFtrn$Tournament)
#Balancing Data
library(ROSE)
DFover<-ovun.sample(Tournament~., data = DFtrn, method = "over", N = nrow(DFtrn), seed = 122)$data
DFunder<-ovun.sample(Tournament~., data = DFtrn, method = "under", N = nrow(DFtrn)*0.5, seed = 125)$data
DFboth<-ovun.sample(Tournament~., data = DFtrn, method = "both", N = nrow(DFtrn), seed = 156)$data
table(DFover$Tournament) 
table(DFunder$Tournament) 
table(DFboth$Tournament)
DFover<-DFover %>% select(1:66)
DFunder<-DFunder %>% select(1:66)
DFboth<-DFboth %>% select(1:66)
#Tree Model
#Addressing data imbalance
#Both
DFdt.b.20<-rpart(formula = Tournament ~ ., data = DFboth, method = "class")
rpart.plot(DFdt.b.20)
DF_tst_pred.b<-predict(DFdt.b.20, DFtst, type = "class")
table(predicted = DF_tst_pred.b, actual = DFtst$Tournament)
vip(DFdt.b.20, num_features = 10)
DFtree_acc.b<- calc_acc(predicted = DF_tst_pred.b, actual = DFtst$Tournament)
DFtree_acc.b
#Bagging
#Tuning
set.seed(825)
oob<-trainControl(method = "oob")
cv_5<-trainControl(method = "cv", number = 5)
rf_grid =  expand.grid(mtry = 1:100)
DFrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(DFboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
DF_bag<-randomForest(as.factor(Tournament)~., data = na.omit(DFboth), mtry = as.numeric(DFrf_tune$bestTune), importance = T, ntrees = 500)
DF_rf<-randomForest(as.factor(Tournament)~., data = na.omit(DFboth), mtry = as.numeric(DFrf_tune$bestTune), importance = T, ntrees = 500)
#Gradient Boosting Model
DF_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~., data = DFboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
DF_boost
DF_boost_pred<-ifelse(predict(DF_boost, DFtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = DF_boost_pred, actual = DFtst$Tournament)
DFboost_acc<- calc_acc(predicted = DF_boost_pred, actual = DFtst$Tournament)
DFboost_acc

#MF 2020
MF.20<-dat.20 %>% filter(Pos == list("MF","MFDF","MFFW")) %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
MF.p.20<-MF.20 %>% select(1:65)
#PCA 
MFpca.20<-prcomp(na.omit(MF.p.20), center = T, scale. = T)
summary(MFpca.20)
plot(MFpca.20, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
fviz_nbclust(as.data.frame(-MFpca.20$x[,1:2]),kmeans, method = "wss")
fviz_nbclust(as.data.frame(-MFpca.20$x[,1:2]),kmeans, method = "silhouette")
fviz_nbclust(as.data.frame(-MFpca.20$x[,1:2]),kmeans, method = "gap_stat")
k = 2
kmeans.pca.20<-kmeans(as.data.frame(-MFpca.20$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.20,as.data.frame(-MFpca.20$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(MF.20),replace = T, prob = c(0.8,0.2))
MFtrn<-MF.20[idx==1,]
MFtst<-MF.20[idx==2,]
table(MFtrn$Tournament)
#Balancing Data
library(ROSE)
MFboth<-ovun.sample(Tournament~., data = MFtrn, method = "both", N = nrow(MFtrn), seed = 156)$data
table(MFboth$Tournament)
MFboth<-MFboth %>% select(1:66)
#Tree Model
#Both
MFdt.b.20<-rpart(formula = Tournament ~ ., data = MFboth, method = "class")
rpart.plot(MFdt.b.20)
MF_tst_pred.b<-predict(MFdt.b.20, MFtst, type = "class")
table(predicted = MF_tst_pred.b, actual = MFtst$Tournament)
vip(MFdt.b.20, num_features = 10)
MFtree_acc.b<- calc_acc(predicted = MF_tst_pred.b, actual = MFtst$Tournament)
MFtree_acc.b
#Bagging
#Tuning
set.seed(825)
oob<-trainControl(method = "oob")
cv_5<-trainControl(method = "cv", number = 5)
rf_grid =  expand.grid(mtry = 1:100)
MFrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(MFboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
MF_bag<-randomForest(as.factor(Tournament)~., data = na.omit(MFboth), mtry = as.numeric(MFrf_tune$bestTune), importance = T, ntrees = 500)
MF_bag
MF_rf<-randomForest(as.factor(Tournament)~., data = na.omit(MFboth), mtry = as.numeric(MFrf_tune$bestTune), importance = T, ntrees = 500)
MF_rf
#Gradient Boosting Model
MF_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~., data = MFboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
MF_boost
MF_boost_pred<-ifelse(predict(MF_boost, MFtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = MF_boost_pred, actual = MFtst$Tournament)
MFboost_acc<- calc_acc(predicted = MF_boost_pred, actual = MFtst$Tournament)
MFboost_acc

#GK 2020
GK.20<-dat.20 %>% filter(Pos == "GK") %>% select(8:91, Tournament, In.Out) %>%  filter(In.Out == "Yes")
GK.p.20<-GK.20 %>% select(1:6,8:9,13:17,18:84)
#PCA 
GKpca.20<-prcomp(na.omit(GK.p.20), center = T, scale. = T)
summary(GKpca.20)
plot(GKpca.20, type = "l", main = "Principal Component Analysis - Optimal number of components")
#K means
k = 2
kmeans.pca.20<-kmeans(as.data.frame(-GKpca.20$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.20,as.data.frame(-GKpca.20$x[,1:2]))#No reasonable predictive model possible

#Splitting into test and training dataset
set.seed(123)
Gk.20<-GK.20 %>% select(1:6,8:9,13:17,18:84, Tournament)
idx<-sample(2,nrow(Gk.20),replace = T, prob = c(0.8,0.2))
GKtrn<-Gk.20[idx==1,]
GKtst<-Gk.20[idx==2,]
table(GKtrn$Tournament)
#Balancing Data
library(ROSE)
GKover<-ovun.sample(Tournament~., data = GKtrn, method = "over", N = 235, seed = 122)$data
GKunder<-ovun.sample(Tournament~., data = GKtrn, method = "under", N = 6, seed = 189)$data
GKboth<-ovun.sample(Tournament~., data = GKtrn, method = "both", N = nrow(Gk.20), seed = 156)$data
table(GKover$Tournament) 
table(GKunder$Tournament) 
table(GKboth$Tournament)
#Tree Model
#Oversampling
GKdt.o.20<-rpart(formula = Tournament ~ ., data = GKover, method = "class")
rpart.plot(GKdt.o.20)
GK_tst_pred.o<-predict(GKdt.o.20, GKtst, type = "class")
table(predicted = GK_tst_pred.o, actual = GKtst$Tournament)
vip(GKdt.o.20, num_features = 10)
GKtree_acc.o<- calc_acc(predicted = GK_tst_pred.o, actual = GKtst$Tournament)
GKtree_acc.o
#Undersampling
GKdt.u.20<-rpart(formula = Tournament ~ ., data = GKunder, method = "class")
rpart.plot(GKdt.u.20)
GK_tst_pred.u<-predict(GKdt.u.20, GKtst, type = "class")
table(predicted = GK_tst_pred.u, actual = GKtst$Tournament)
vip(GKdt.u.20, num_features = 90)
GKtree_acc.u<- calc_acc(predicted = GK_tst_pred.u, actual = GKtst$Tournament)
GKtree_acc.u
#Both
GKdt.b.20<-rpart(formula = Tournament ~ ., data = GKboth, method = "class")
rpart.plot(GKdt.b.20)
GK_tst_pred.b<-predict(GKdt.b.20, GKtst, type = "class")
table(predicted = GK_tst_pred.b, actual = GKtst$Tournament)
vip(GKdt.b.20, num_features = 90)
GKtree_acc.b<- calc_acc(predicted = GK_tst_pred.b, actual = GKtst$Tournament)
GKtree_acc.b
#Bagging
#Tuning
set.seed(825)
oob<-trainControl(method = "oob")
cv_5<-trainControl(method = "cv", number = 5)
rf_grid =  expand.grid(mtry = 1:100)
GKrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(GKboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
GK_bag<-randomForest(as.factor(Tournament)~., data = na.omit(GKboth), mtry = ncol(GKboth)-1, importance = T, ntrees = 5000)
GK_rf<-randomForest(as.factor(Tournament)~., data = na.omit(GKboth), mtry = as.numeric(GKrf_tune$bestTune), importance = T, ntrees = 500)
GK_rf
GK_bag
#Gradient Boosting Model
GK_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~., data = GKboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
GK_boost
GK_boost_pred<-ifelse(predict(GK_boost, GKtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = GK_boost_pred, actual = GKtst$Tournament)
GKboost_acc<- calc_acc(predicted = GK_boost_pred, actual = GKtst$Tournament)
GKboost_acc


Rar.dat<-dat.21 %>% filter(Nation == "Rarita") %>% select(Pos, 8:91)
#FW
Rar.FW<-Rar.dat %>% filter(Pos == list("FW", "FWDF", "FWMF")) %>% select(1:66)
FW.pred<-ifelse(predict(FW_boost, newdata = Rar.FW)>0,"Yes","No")
FW.pred.int<-predict(FW_boost, newdata = Rar.FW, interval = "confidence")
FWs<-dat.21 %>% filter(Nation == "Rarita") %>% filter(Pos == list("FW", "FWDF", "FWMF"))%>%  mutate("selected" = FW.pred)
FWs.names.20<-FWs %>% select(Player, selected) %>% filter(selected == "Yes") %>% select(Player)
FWs.names.20

#MF
Rar.MF<-Rar.dat %>% filter(Pos == list("MF","MFDF","MFFW") )%>% select(1:66)
MF.pred<-ifelse(predict(MF_boost, newdata = Rar.MF)>0,"Yes","No")
MF.pred.int<-predict(MF_boost, newdata = Rar.MF, interval = "confidence")
MFs<-dat.21 %>% filter(Nation == "Rarita") %>% filter(Pos == list("MF","MFDF","MFFW"))%>%  mutate("selected" = MF.pred)
MFs.names.20<-MFs %>% select(Player, selected) %>% filter(selected == "Yes") %>% select(Player)
MFs.names.20

#DF
Rar.DF<-Rar.dat %>% filter(Pos == list("DF","DFMF","DFFW") )%>% select(1:66)
DF.pred<-ifelse(predict(DF_boost, newdata = Rar.DF)>0,"Yes","No")
DF.pred.int<-predict(DF_boost, newdata = Rar.DF, interval = "confidence")
DFs<-dat.21 %>% filter(Nation == "Rarita") %>% filter(Pos == list("DF","DFMF","DFFW"))%>%  mutate("selected" = DF.pred)
DFs.names.20<-DFs %>% select(Player, selected) %>% filter(selected == "Yes")%>% select(Player)
DFs.names.20

#GK
Rar.GK<-Rar.dat %>% filter(Pos == "GK")
GK.pred<-ifelse(predict(GK_boost, newdata = Rar.GK)>0,"Yes","No")
GK.pred.int<-predict(GK_boost, newdata = Rar.GK, interval = "confidence")
GKs<-dat.21 %>% filter(Nation == "Rarita") %>% filter(Pos == "GK")%>%  mutate("selected" = GK.pred)
GKs.names.20<-GKs %>% select(Player, selected) %>% filter(selected == "Yes") %>% select(Player)
GKs.names.20

#FW 2021
FW.21<-dat.21 %>% filter(Pos == list("FW","FWMF","FWDF")) %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
FW.p.21<-FW.21 %>% select(1:65)
#PCA 
FWpca.21<-prcomp(na.omit(FW.p.21), center = T, scale. = T)
summary(FWpca.21)
plot(FWpca.21, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
fviz_nbclust(as.data.frame(-FWpca.21$x[,1:2]),kmeans, method = "wss")
fviz_nbclust(as.data.frame(-FWpca.21$x[,1:2]),kmeans, method = "silhouette")
fviz_nbclust(as.data.frame(-FWpca.21$x[,1:2]),kmeans, method = "gap_stat")
k = 2
kmeans.pca.21<-kmeans(as.data.frame(-FWpca.21$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.21,as.data.frame(-FWpca.21$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(FW.21),replace = T, prob = c(0.8,0.2))
FWtrn<-FW.21[idx==1,]
FWtst<-FW.21[idx==2,]
table(FWtrn$Tournament)
#Balancing Data
library(ROSE)
FWover<-ovun.sample(Tournament~., data = FWtrn, method = "over", N = nrow(FWtrn), seed = 122)$data
FWunder<-ovun.sample(Tournament~., data = FWtrn, method = "under", N = nrow(FWtrn)*0.6, seed = 125)$data
FWboth<-ovun.sample(Tournament~., data = FWtrn, method = "both", N = nrow(FWtrn), seed = 156)$data
table(FWover$Tournament) 
table(FWunder$Tournament) 
table(FWboth$Tournament)
FWover<-FWover %>% select(1:66)
FWunder<-FWunder %>% select(1:66)
FWboth<-FWboth %>% select(1:66)
#Tree Model
#Oversampling
FWdt.o.21<-rpart(formula = Tournament ~ ., data = FWover, method = "class")
rpart.plot(FWdt.o.21)
FW_tst_pred.o<-predict(FWdt.o.21, FWtst, type = "class")
table(predicted = FW_tst_pred.o, actual = FWtst$Tournament)
vip(FWdt.o.21, num_features = 90)
FWtree_acc.o<- calc_acc(predicted = FW_tst_pred.o, actual = FWtst$Tournament)
FWtree_acc.o
#Undersampling
FWdt.u.21<-rpart(formula = Tournament ~ ., data = FWunder, method = "class")
rpart.plot(FWdt.u.21)
FW_tst_pred.u<-predict(FWdt.u.21, FWtst, type = "class")
table(predicted = FW_tst_pred.u, actual = FWtst$Tournament)
vip(FWdt.u.21, num_features = 90)
FWtree_acc.u<- calc_acc(predicted = FW_tst_pred.u, actual = FWtst$Tournament)
FWtree_acc.u
#Both
FWdt.b.21<-rpart(formula = Tournament ~ ., data = FWboth, method = "class")
rpart.plot(FWdt.b.21)
FW_tst_pred.b<-predict(FWdt.b.21, FWtst, type = "class")
table(predicted = FW_tst_pred.b, actual = FWtst$Tournament)
vip(FWdt.b.21, num_features = 90)
FWtree_acc.b<- calc_acc(predicted = FW_tst_pred.b, actual = FWtst$Tournament)
FWtree_acc.b
#Bagging
library(randomForest)
library(gbm)
library(ISLR)
library(Rcpp)
#Tuning
set.seed(825)
oob<-trainControl(method = "oob")
cv_5<-trainControl(method = "cv", number = 5)
rf_grid =  expand.grid(mtry = 1:100)
FWrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(FWboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
FW_bag<-randomForest(as.factor(Tournament)~., data = na.omit(FWboth), mtry = ncol(FWboth)-1, importance = T, ntrees = 500)
FW_rf<-randomForest(as.factor(Tournament)~., data = na.omit(FWboth), mtry = as.numeric(FWrf_tune$bestTune), importance = T, ntrees = 500)
FW_bag
FW_rf
#Gradient Boosting Model
FW_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~., data = FWboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
FW_boost
FW_boost_pred<-ifelse(predict(FW_boost, FWtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = FW_boost_pred, actual = FWtst$Tournament)
FWboost_acc<- calc_acc(predicted = FW_boost_pred, actual = FWtst$Tournament)
FWboost_acc

#DF 2021
DF.21<-dat.21 %>% filter(Pos == list("DF","DFMF","DFFW")) %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
DF.p.21<-DF.21 %>% select(1:65)
#PCA 
DFpca.21<-prcomp(na.omit(DF.p.21), center = T, scale. = T)
summary(DFpca.21)
plot(DFpca.21, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
fviz_nbclust(as.data.frame(-DFpca.21$x[,1:2]),kmeans, method = "wss")
fviz_nbclust(as.data.frame(-DFpca.21$x[,1:2]),kmeans, method = "silhouette")
fviz_nbclust(as.data.frame(-DFpca.21$x[,1:2]),kmeans, method = "gap_stat")
k = 2
kmeans.pca.21<-kmeans(as.data.frame(-DFpca.21$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.21,as.data.frame(-DFpca.21$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(DF.21),replace = T, prob = c(0.8,0.2))
DFtrn<-DF.21[idx==1,]
DFtst<-DF.21[idx==2,]
table(DFtrn$Tournament)
#Balancing Data
library(ROSE)
DFover<-ovun.sample(Tournament~., data = DFtrn, method = "over", N = nrow(DFtrn), seed = 122)$data
DFunder<-ovun.sample(Tournament~., data = DFtrn, method = "under", N = nrow(DFtrn)*0.6, seed = 125)$data
DFboth<-ovun.sample(Tournament~., data = DFtrn, method = "both", N = nrow(DFtrn), seed = 156)$data
table(DFover$Tournament) 
table(DFunder$Tournament) 
table(DFboth$Tournament)
DFover<-DFover %>% select(1:66)
DFunder<-DFunder %>% select(1:66)
DFboth<-DFboth %>% select(1:66)
#Tree Model
#Oversampling
DFdt.o.21<-rpart(formula = Tournament ~ ., data = DFover, method = "class")
rpart.plot(DFdt.o.21)
DF_tst_pred.o<-predict(DFdt.o.21, DFtst, type = "class")
table(predicted = DF_tst_pred.o, actual = DFtst$Tournament)
vip(DFdt.o.21, num_features = 90)
DFtree_acc.o<- calc_acc(predicted = DF_tst_pred.o, actual = DFtst$Tournament)
DFtree_acc.o
#Undersampling
DFdt.u.21<-rpart(formula = Tournament ~ ., data = DFunder, method = "class")
rpart.plot(DFdt.u.21)
DF_tst_pred.u<-predict(DFdt.u.21, DFtst, type = "class")
table(predicted = DF_tst_pred.u, actual = DFtst$Tournament)
vip(DFdt.u.21, num_features = 90)
DFtree_acc.u<- calc_acc(predicted = DF_tst_pred.u, actual = DFtst$Tournament)
DFtree_acc.u
#Both
DFdt.b.21<-rpart(formula = Tournament ~ ., data = DFboth, method = "class")
rpart.plot(DFdt.b.21)
DF_tst_pred.b<-predict(DFdt.b.21, DFtst, type = "class")
table(predicted = DF_tst_pred.b, actual = DFtst$Tournament)
vip(DFdt.b.21, num_features = 90)
DFtree_acc.b<- calc_acc(predicted = DF_tst_pred.b, actual = DFtst$Tournament)
DFtree_acc.b
#Bagging
#Tuning
set.seed(825)
oob<-trainControl(method = "oob")
cv_5<-trainControl(method = "cv", number = 5)
rf_grid =  expand.grid(mtry = 1:100)
DFrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(DFboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
DF_bag<-randomForest(as.factor(Tournament)~., data = na.omit(DFboth), mtry = as.numeric(DFrf_tune$bestTune), importance = T, ntrees = 500)
DF_bag
DF_rf<-randomForest(as.factor(Tournament)~., data = na.omit(DFboth), mtry = as.numeric(DFrf_tune$bestTune), importance = T, ntrees = 500)
DF_rf
#Gradient Boosting Model
DF_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~.-In.Out, data = DFboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
DF_boost
DF_boost_pred<-ifelse(predict(DF_boost, DFtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = DF_boost_pred, actual = DFtst$Tournament)
DFboost_acc<- calc_acc(predicted = DF_boost_pred, actual = DFtst$Tournament)
DFboost_acc

#MF 2021
MF.21<-dat.21 %>% filter(Pos == list("MF","MFDF","MFFW")) %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
MF.p.21<-MF.21 %>% select(1:65)
#PCA 
MFpca.21<-prcomp(na.omit(MF.p.21), center = T, scale. = T)
summary(MFpca.21)
plot(MFpca.21, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
fviz_nbclust(as.data.frame(-MFpca.21$x[,1:2]),kmeans, method = "wss")
fviz_nbclust(as.data.frame(-MFpca.21$x[,1:2]),kmeans, method = "silhouette")
fviz_nbclust(as.data.frame(-MFpca.21$x[,1:2]),kmeans, method = "gap_stat")
k = 2
kmeans.pca.21<-kmeans(as.data.frame(-MFpca.21$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.21,as.data.frame(-MFpca.21$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(MF.21),replace = T, prob = c(0.8,0.2))
MFtrn<-MF.21[idx==1,]
MFtst<-MF.21[idx==2,]
table(MFtrn$Tournament)
#Balancing Data
library(ROSE)
MFover<-ovun.sample(Tournament~., data = MFtrn, method = "over", N = nrow(MFtrn), seed = 122)$data
MFunder<-ovun.sample(Tournament~., data = MFtrn, method = "under", N = nrow(MFtrn)*0.7, seed = 125)$data
MFboth<-ovun.sample(Tournament~., data = MFtrn, method = "both", N = nrow(MFtrn), seed = 156)$data
table(MFover$Tournament) 
table(MFunder$Tournament) 
table(MFboth$Tournament)
MFover<-MFover %>% select(1:66)
MFunder<-MFunder %>% select(1:66)
MFboth<-MFboth %>% select(1:66)
#Tree Model
#Oversampling
MFdt.o.21<-rpart(formula = Tournament ~ ., data = MFover, method = "class")
rpart.plot(MFdt.o.21)
MF_tst_pred.o<-predict(MFdt.o.21, MFtst, type = "class")
table(predicted = MF_tst_pred.o, actual = MFtst$Tournament)
vip(MFdt.o.21, num_features = 90)
MFtree_acc.o<- calc_acc(predicted = MF_tst_pred.o, actual = MFtst$Tournament)
MFtree_acc.o
#Undersampling
MFdt.u.21<-rpart(formula = Tournament ~ ., data = MFunder, method = "class")
rpart.plot(MFdt.u.21)
MF_tst_pred.u<-predict(MFdt.u.21, MFtst, type = "class")
table(predicted = MF_tst_pred.u, actual = MFtst$Tournament)
vip(MFdt.u.21, num_features = 90)
MFtree_acc.u<- calc_acc(predicted = MF_tst_pred.u, actual = MFtst$Tournament)
MFtree_acc.u
#Both
MFdt.b.21<-rpart(formula = Tournament ~ ., data = MFboth, method = "class")
rpart.plot(MFdt.b.21)
MF_tst_pred.b<-predict(MFdt.b.21, MFtst, type = "class")
table(predicted = MF_tst_pred.b, actual = MFtst$Tournament)
vip(MFdt.b.21, num_features = 90)
MFtree_acc.b<- calc_acc(predicted = MF_tst_pred.b, actual = MFtst$Tournament)
MFtree_acc.b
#Bagging
#Tuning
set.seed(825)
oob<-trainControl(method = "oob")
cv_5<-trainControl(method = "cv", number = 5)
rf_grid =  expand.grid(mtry = 1:100)
MFrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(MFboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
MF_bag<-randomForest(as.factor(Tournament)~., data = na.omit(MFboth), mtry = as.numeric(MFrf_tune$bestTune), importance = T, ntrees = 500)
MF_bag
MF_rf<-randomForest(as.factor(Tournament)~., data = na.omit(MFboth), mtry = as.numeric(MFrf_tune$bestTune), importance = T, ntrees = 500)
MF_rf
#Gradient Boosting Model
MF_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~.-In.Out, data = MFboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
MF_boost
MF_boost_pred<-ifelse(predict(MF_boost, MFtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = MF_boost_pred, actual = MFtst$Tournament)
MFboost_acc<- calc_acc(predicted = MF_boost_pred, actual = MFtst$Tournament)
MFboost_acc

#GK 2021
GK.21<-dat.21 %>% filter(Pos == "GK") %>% select(8:91, Tournament, In.Out) %>%  filter(In.Out == "Yes")
GK.p.21<-GK.21 %>% select(1:6,8:9,13:17,18:84)
#PCA 
GKpca.21<-prcomp(na.omit(GK.p.21), center = T, scale. = T)
summary(GKpca.21)
plot(GKpca.21, type = "l", main = "Principal Component Analysis - Optimal number of components")
#K means
k = 2
kmeans.pca.21<-kmeans(as.data.frame(-GKpca.21$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.21,as.data.frame(-GKpca.21$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
#Splitting into test and training dataset
set.seed(123)
Gk.21<-GK.21 %>% select(1:6,8:9,13:17,19:84, Tournament)
idx<-sample(2,nrow(Gk.21),replace = T, prob = c(0.8,0.2))
GKtrn<-Gk.21[idx==1,]
GKtst<-Gk.21[idx==2,]
table(GKtrn$Tournament)
#Balancing Data
library(ROSE)
GKover<-ovun.sample(Tournament~., data = GKtrn, method = "over", N = 193, seed = 122)$data
GKunder<-ovun.sample(Tournament~., data = GKtrn, method = "under", N = nrow(GK.21)*0.3, seed = 189)$data
GKboth<-ovun.sample(Tournament~., data = GKtrn, method = "both", N = nrow(Gk.21), seed = 156)$data
table(GKover$Tournament) 
table(GKunder$Tournament) 
table(GKboth$Tournament)
#Tree Model
#Oversampling
GKdt.o.21<-rpart(formula = Tournament ~ ., data = GKover, method = "class")
rpart.plot(GKdt.o.21)
GK_tst_pred.o<-predict(GKdt.o.21, GKtst, type = "class")
table(predicted = GK_tst_pred.o, actual = GKtst$Tournament)
vip(GKdt.o.21, num_features = 90)
GKtree_acc.o<- calc_acc(predicted = GK_tst_pred.o, actual = GKtst$Tournament)
GKtree_acc.o
#Undersampling
GKdt.u.21<-rpart(formula = Tournament ~ ., data = GKunder, method = "class")
rpart.plot(GKdt.u.21)
GK_tst_pred.u<-predict(GKdt.u.21, GKtst, type = "class")
table(predicted = GK_tst_pred.u, actual = GKtst$Tournament)
vip(GKdt.u.21, num_features = 90)
GKtree_acc.u<- calc_acc(predicted = GK_tst_pred.u, actual = GKtst$Tournament)
GKtree_acc.u
#Both
GKdt.b.21<-rpart(formula = Tournament ~ ., data = GKboth, method = "class")
rpart.plot(GKdt.b.21)
GK_tst_pred.b<-predict(GKdt.b.21, GKtst, type = "class")
table(predicted = GK_tst_pred.b, actual = GKtst$Tournament)
vip(GKdt.b.21, num_features = 90)
GKtree_acc.b<- calc_acc(predicted = GK_tst_pred.b, actual = GKtst$Tournament)
GKtree_acc.b
#Bagging
#Tuning
set.seed(825)
oob<-trainControl(method = "oob")
cv_5<-trainControl(method = "cv", number = 5)
rf_grid =  expand.grid(mtry = 1:100)
GKrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(GKboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
GK_bag<-randomForest(as.factor(Tournament)~., data = na.omit(GKboth), mtry = ncol(GKboth)-1, importance = T, ntrees = 5000)
GK_rf<-randomForest(as.factor(Tournament)~., data = na.omit(GKboth), mtry = as.numeric(GKrf_tune$bestTune), importance = T, ntrees = 500)
GK_rf
GK_bag
#Gradient Boosting Model
GK_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~., data = GKboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
GK_boost
GK_boost_pred<-ifelse(predict(GK_boost, GKtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = GK_boost_pred, actual = GKtst$Tournament)
GKboost_acc<- calc_acc(predicted = GK_boost_pred, actual = GKtst$Tournament)
GKboost_acc

#Team selection
Rar.dat<-dat.21 %>% filter(Nation == "Rarita") %>% select(Pos, 8:91)
#FW
Rar.FW<-Rar.dat %>% filter(Pos == list("FW", "FWDF", "FWMF")) %>% select(1:66)
FW.pred<-ifelse(predict(FW_boost, newdata = Rar.FW)>0,"Yes","No")
FW.pred.int<-predict(FW_boost, newdata = Rar.FW, interval = "confidence")
FWs<-dat.21 %>% filter(Nation == "Rarita") %>% filter(Pos == list("FW", "FWDF", "FWMF"))%>%  mutate("selected" = FW.pred)
FWs.names.21<-FWs %>% select(Player, selected) %>% filter(selected == "Yes") %>% select(Player)
FWs.names.21

#MF
Rar.MF<-Rar.dat %>% filter(Pos == list("MF","MFDF","MFFW") )%>% select(1:66)
MF.pred<-ifelse(predict(MF_boost, newdata = Rar.MF)>0,"Yes","No")
MF.pred.int<-predict(MF_boost, newdata = Rar.MF, interval = "confidence")
MFs<-dat.21 %>% filter(Nation == "Rarita") %>% filter(Pos == list("MF","MFDF","MFFW"))%>%  mutate("selected" = MF.pred)
MFs.names.21<-MFs %>% select(Player, selected) %>% filter(selected == "Yes") %>% select(Player)
MFs.names.21

#DF
Rar.DF<-Rar.dat %>% filter(Pos == list("DF","DFMF","DFFW") )%>% select(1:66)
DF.pred<-ifelse(predict(DF_boost, newdata = Rar.DF)>0,"Yes","No")
DF.pred.int<-predict(DF_boost, newdata = Rar.DF, interval = "confidence")
DFs<-dat.21 %>% filter(Nation == "Rarita") %>% filter(Pos == list("DF","DFMF","DFFW"))%>%  mutate("selected" = DF.pred)
DFs.names.21<-DFs %>% select(Player, selected) %>% filter(selected == "Yes") %>% select(Player)
DFs.names.21

#GK
Rar.GK<-Rar.dat %>% filter(Pos == "GK")
GK.pred<-ifelse(predict(GK_boost, newdata = Rar.GK)>0,"Yes","No")
GK.pred.int<-predict(GK_boost, newdata = Rar.GK, interval = "confidence")
GKs<-dat.21 %>% filter(Nation == "Rarita") %>% filter(Pos == "GK")%>%  mutate("selected" = GK.pred)
GKs.names.21<-GKs %>% select(Player, selected) %>% filter(selected == "Yes") %>% select(Player)
GKs.names.21

DF.sel<-head(DFs.names.20, n = 7)
MF.sel<-head(MFs.names.20, n = 6)
FW.sel<-head(FWs.names.21)
GK.sel<-GKs.names.21
