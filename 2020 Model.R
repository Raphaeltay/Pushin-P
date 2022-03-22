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
#FW 2020
FW.20<-dat.20 %>% filter(Pos == "FW") %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
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
vip(FWdt.b.20, num_features = 90)
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
#Neural Networks


#FWMF 2020
FWMF.20<-dat.20 %>% filter(Pos == "FWMF") %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
FWMF.p.20<-FWMF.20 %>% select(1:65)
#PCA 
FWMFpca.20<-prcomp(na.omit(FWMF.p.20), center = T, scale. = T)
summary(FWMFpca.20)
plot(FWMFpca.20, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
fviz_nbclust(as.data.frame(-FWMFpca.20$x[,1:2]),kmeans, method = "wss")
fviz_nbclust(as.data.frame(-FWMFpca.20$x[,1:2]),kmeans, method = "silhouette")
fviz_nbclust(as.data.frame(-FWMFpca.20$x[,1:2]),kmeans, method = "gap_stat")
k = 2
kmeans.pca.20<-kmeans(as.data.frame(-FWMFpca.20$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.20,as.data.frame(-FWMFpca.20$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(FWMF.20),replace = T, prob = c(0.8,0.2))
FWMFtrn<-FWMF.20[idx==1,]
FWMFtst<-FWMF.20[idx==2,]
table(FWMFtrn$Tournament)
#Balancing Data
library(ROSE)
FWMFover<-ovun.sample(Tournament~., data = FWMFtrn, method = "over", N = nrow(FWMFtrn), seed = 122)$data
FWMFunder<-ovun.sample(Tournament~., data = FWMFtrn, method = "under", N = nrow(FWMFtrn)*0.7, seed = 125)$data
FWMFboth<-ovun.sample(Tournament~., data = FWMFtrn, method = "both", N = nrow(FWMFtrn), seed = 156)$data
table(FWMFover$Tournament) 
table(FWMFunder$Tournament) 
table(FWMFboth$Tournament)
FWMFover<-FWMFover %>% select(1:66)
FWMFunder<-FWMFunder %>% select(1:66)
FWMFboth<-FWMFboth %>% select(1:66)
#Tree Model
#Oversampling
FWMFdt.o.20<-rpart(formula = Tournament ~ ., data = FWMFover, method = "class")
rpart.plot(FWMFdt.o.20)
FWMF_tst_pred.o<-predict(FWMFdt.o.20, FWMFtst, type = "class")
table(predicted = FWMF_tst_pred.o, actual = FWMFtst$Tournament)
vip(FWMFdt.o.20, num_features = 90)
FWMFtree_acc.o<- calc_acc(predicted = FWMF_tst_pred.o, actual = FWMFtst$Tournament)
FWMFtree_acc.o
#Undersampling
FWMFdt.u.20<-rpart(formula = Tournament ~ ., data = FWMFunder, method = "class")
rpart.plot(FWMFdt.u.20)
FWMF_tst_pred.u<-predict(FWMFdt.u.20, FWMFtst, type = "class")
table(predicted = FWMF_tst_pred.u, actual = FWMFtst$Tournament)
vip(FWMFdt.u.20, num_features = 90)
FWMFtree_acc.u<- calc_acc(predicted = FWMF_tst_pred.u, actual = FWMFtst$Tournament)
FWMFtree_acc.u
#Both
FWMFdt.b.20<-rpart(formula = Tournament ~ ., data = FWMFboth, method = "class")
rpart.plot(FWMFdt.b.20)
FWMF_tst_pred.b<-predict(FWMFdt.b.20, FWMFtst, type = "class")
table(predicted = FWMF_tst_pred.b, actual = FWMFtst$Tournament)
vip(FWMFdt.b.20, num_features = 90)
FWMFtree_acc.b<- calc_acc(predicted = FWMF_tst_pred.b, actual = FWMFtst$Tournament)
FWMFtree_acc.b
#Bagging
#Tuning
set.seed(825)
oob<-trainControl(method = "oob")
cv_5<-trainControl(method = "cv", number = 5)
rf_grid =  expand.grid(mtry = 1:100)
FWMFrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(FWMFboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
FWMF_bag<-randomForest(as.factor(Tournament)~., data = na.omit(FWMFboth), mtry = as.numeric(FWMFrf_tune$bestTune), importance = T, ntrees = 500)
FWMF_bag
FWMF_rf<-randomForest(as.factor(Tournament)~., data = na.omit(FWMFboth), mtry = as.numeric(FWMFrf_tune$bestTune), importance = T, ntrees = 500)
FWMF_rf
#Gradient Boosting Model
FWMF_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~., data = FWMFboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
FWMF_boost
FWMF_boost_pred<-ifelse(predict(FWMF_boost, FWMFtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = FWMF_boost_pred, actual = FWMFtst$Tournament)
FWMFboost_acc<- calc_acc(predicted = FWMF_boost_pred, actual = FWMFtst$Tournament)
FWMFboost_acc

#FWDF 2020
FWDF.20<-dat.20 %>% filter(Pos == "FWDF") %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
FWDF.p.20<-FWDF.20 %>% select(1:65)
#PCA 
FWDFpca.20<-prcomp(na.omit(FWDF.p.20), center = T, scale. = T)
summary(FWDFpca.20)
plot(FWDFpca.20, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
fviz_nbclust(as.data.frame(-FWDFpca.20$x[,1:2]),kmeans, method = "wss")
fviz_nbclust(as.data.frame(-FWDFpca.20$x[,1:2]),kmeans, method = "silhouette")
fviz_nbclust(as.data.frame(-FWDFpca.20$x[,1:2]),kmeans, method = "gap_stat")
k = 2
kmeans.pca.20<-kmeans(as.data.frame(-FWDFpca.20$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.20,as.data.frame(-FWDFpca.20$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(FWDF.20),replace = T, prob = c(0.8,0.2))
FWDFtrn<-FWDF.20[idx==1,]
FWDFtst<-FWDF.20[idx==2,]
table(FWDFtrn$Tournament)
#Balancing Data
library(ROSE)
FWDFover<-ovun.sample(Tournament~., data = FWDFtrn, method = "over", N = nrow(FWDFtrn), seed = 122)$data
FWDFunder<-ovun.sample(Tournament~., data = FWDFtrn, method = "under", N = nrow(FWDFtrn)*0.7, seed = 125)$data
FWDFboth<-ovun.sample(Tournament~., data = FWDFtrn, method = "both", N = nrow(FWDFtrn), seed = 156)$data
table(FWDFover$Tournament) 
table(FWDFunder$Tournament) 
table(FWDFboth$Tournament)
FWDFover<-FWDFover %>% select(1:66)
FWDFunder<-FWDFunder %>% select(1:66)
FWDFboth<-FWDFboth %>% select(1:66)
#Tree Model
#Oversampling
FWDFdt.o.20<-rpart(formula = Tournament ~ ., data = FWDFover, method = "class")
rpart.plot(FWDFdt.o.20)
FWDF_tst_pred.o<-predict(FWDFdt.o.20, FWDFtst, type = "class")
table(predicted = FWDF_tst_pred.o, actual = FWDFtst$Tournament)
vip(FWDFdt.o.20, num_features = 90)
FWDFtree_acc.o<- calc_acc(predicted = FWDF_tst_pred.o, actual = FWDFtst$Tournament)
FWDFtree_acc.o
#Undersampling
FWDFdt.u.20<-rpart(formula = Tournament ~ ., data = FWDFunder, method = "class")
rpart.plot(FWDFdt.u.20)
FWDF_tst_pred.u<-predict(FWDFdt.u.20, FWDFtst, type = "class")
table(predicted = FWDF_tst_pred.u, actual = FWDFtst$Tournament)
vip(FWDFdt.u.20, num_features = 90)
FWDFtree_acc.u<- calc_acc(predicted = FWDF_tst_pred.u, actual = FWDFtst$Tournament)
FWDFtree_acc.u
#Both
FWDFdt.b.20<-rpart(formula = Tournament ~ ., data = FWDFboth, method = "class")
rpart.plot(FWDFdt.b.20)
FWDF_tst_pred.b<-predict(FWDFdt.b.20, FWDFtst, type = "class")
table(predicted = FWDF_tst_pred.b, actual = FWDFtst$Tournament)
vip(FWDFdt.b.20, num_features = 90)
FWDFtree_acc.b<- calc_acc(predicted = FWDF_tst_pred.b, actual = FWDFtst$Tournament)
FWDFtree_acc.b
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
FWDFrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(FWDFboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
FWDF_bag<-randomForest(as.factor(Tournament)~., data = na.omit(FWDFboth), mtry = as.numeric(FWDFrf_tune$bestTune), importance = T, ntrees = 500)
FWDF_bag
FWDF_rf<-randomForest(as.factor(Tournament)~., data = na.omit(FWDFboth), mtry = as.numeric(FWDFrf_tune$bestTune), importance = T, ntrees = 500)
FWDF_rf
#Gradient Boosting Model
FWDF_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~.-In.Out, data = FWDFboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
FWDF_boost
FWDF_boost_pred<-ifelse(predict(FWDF_boost, FWDFtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = FWDF_boost_pred, actual = FWDFtst$Tournament)
FWDFboost_acc<- calc_acc(predicted = FWDF_boost_pred, actual = FWDFtst$Tournament)
FWDFboost_acc
#Neural Networks



#DFFW 2020
DFFW.20<-dat.20 %>% filter(Pos == "DFFW") %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
DFFW.p.20<-DFFW.20 %>% select(1:65)
#PCA 
DFFWpca.20<-prcomp(na.omit(DFFW.p.20), center = T, scale. = T)
summary(DFFWpca.20)
plot(DFFWpca.20, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
fviz_nbclust(as.data.frame(-DFFWpca.20$x[,1:2]),kmeans, method = "wss")
fviz_nbclust(as.data.frame(-DFFWpca.20$x[,1:2]),kmeans, method = "silhouette")
fviz_nbclust(as.data.frame(-DFFWpca.20$x[,1:2]),kmeans, method = "gap_stat")
k = 2
kmeans.pca.20<-kmeans(as.data.frame(-DFFWpca.20$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.20,as.data.frame(-DFFWpca.20$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(DFFW.20),replace = T, prob = c(0.8,0.2))
DFFWtrn<-DFFW.20[idx==1,]
DFFWtst<-DFFW.20[idx==2,]
table(DFFWtrn$Tournament)
#Balancing Data
library(ROSE)
DFFWover<-ovun.sample(Tournament~.-In.Out, data = DFFWtrn, method = "over", N = nrow(DFFWtrn), seed = 122)$data
DFFWunder<-ovun.sample(Tournament~.-In.Out, data = DFFWtrn, method = "under", N = nrow(DFFWtrn)*0.7, seed = 125)$data
DFFWboth<-ovun.sample(Tournament~.-In.Out, data = DFFWtrn, method = "both", N = nrow(DFFWtrn), seed = 156)$data
table(DFFWover$Tournament) 
table(DFFWunder$Tournament) 
table(DFFWboth$Tournament)
DFFWover<-DFFWover %>% select(1:66)
DFFWunder<-DFFWunder %>% select(1:66)
DFFWboth<-DFFWboth %>% select(1:66)
#Tree Model
#Oversampling
DFFWdt.o.20<-rpart(formula = Tournament ~ ., data = DFFWover, method = "class")
rpart.plot(DFFWdt.o.20)
DFFW_tst_pred.o<-predict(DFFWdt.o.20, DFFWtst, type = "class")
table(predicted = DFFW_tst_pred.o, actual = DFFWtst$Tournament)
vip(DFFWdt.o.20, num_features = 90)
DFFWtree_acc.o<- calc_acc(predicted = DFFW_tst_pred.o, actual = DFFWtst$Tournament)
DFFWtree_acc.o
#Undersampling
DFFWdt.u.20<-rpart(formula = Tournament ~ ., data = DFFWunder, method = "class")
rpart.plot(DFFWdt.u.20)
DFFW_tst_pred.u<-predict(DFFWdt.u.20, DFFWtst, type = "class")
table(predicted = DFFW_tst_pred.u, actual = DFFWtst$Tournament)
vip(DFFWdt.u.20, num_features = 90)
DFFWtree_acc.u<- calc_acc(predicted = DFFW_tst_pred.u, actual = DFFWtst$Tournament)
DFFWtree_acc.u
#Both
DFFWdt.b.20<-rpart(formula = Tournament ~ ., data = DFFWboth, method = "class")
rpart.plot(DFFWdt.b.20)
DFFW_tst_pred.b<-predict(DFFWdt.b.20, DFFWtst, type = "class")
table(predicted = DFFW_tst_pred.b, actual = DFFWtst$Tournament)
vip(DFFWdt.b.20, num_features = 90)
DFFWtree_acc.b<- calc_acc(predicted = DFFW_tst_pred.b, actual = DFFWtst$Tournament)
DFFWtree_acc.b
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
DFFWrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(DFFWboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
DFFW_bag<-randomForest(as.factor(Tournament)~., data = na.omit(DFFWboth), mtry = as.numeric(DFFWrf_tune$bestTune), importance = T, ntrees = 500)
DFFW_bag
DFFW_rf<-randomForest(as.factor(Tournament)~., data = na.omit(DFFWboth), mtry = as.numeric(DFFWrf_tune$bestTune), importance = T, ntrees = 500)
DFFW_rf
#Gradient Boosting Model
DFFW_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~.-In.Out, data = DFFWboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
DFFW_boost
DFFW_boost_pred<-ifelse(predict(DFFW_boost, DFFWtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = DFFW_boost_pred, actual = DFFWtst$Tournament)
DFFWboost_acc<- calc_acc(predicted = DFFW_boost_pred, actual = DFFWtst$Tournament)
DFFWboost_acc
#Neural Networks



#DFMF 2020
DFMF.20<-dat.20 %>% filter(Pos == "DFMF") %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
DFMF.p.20<-DFMF.20 %>% select(1:65)
#PCA 
DFMFpca.20<-prcomp(na.omit(DFMF.p.20), center = T, scale. = T)
summary(DFMFpca.20)
plot(DFMFpca.20, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
fviz_nbclust(as.data.frame(-DFMFpca.20$x[,1:2]),kmeans, method = "wss")
fviz_nbclust(as.data.frame(-DFMFpca.20$x[,1:2]),kmeans, method = "silhouette")
fviz_nbclust(as.data.frame(-DFMFpca.20$x[,1:2]),kmeans, method = "gap_stat")
k = 2
kmeans.pca.20<-kmeans(as.data.frame(-DFMFpca.20$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.20,as.data.frame(-DFMFpca.20$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(DFMF.20),replace = T, prob = c(0.8,0.2))
DFMFtrn<-DFMF.20[idx==1,]
DFMFtst<-DFMF.20[idx==2,]
table(DFMFtrn$Tournament)
#Balancing Data
library(ROSE)
DFMFover<-ovun.sample(Tournament~.-In.Out, data = DFMFtrn, method = "over", N = nrow(DFMFtrn), seed = 122)$data
DFMFunder<-ovun.sample(Tournament~.-In.Out, data = DFMFtrn, method = "under", N = nrow(DFMFtrn)*0.7, seed = 125)$data
DFMFboth<-ovun.sample(Tournament~.-In.Out, data = DFMFtrn, method = "both", N = nrow(DFMFtrn), seed = 156)$data
table(DFMFover$Tournament) 
table(DFMFunder$Tournament) 
table(DFMFboth$Tournament)
DFMFover<-DFMFover %>% select(1:66)
DFMFunder<-DFMFunder %>% select(1:66)
DFMFboth<-DFMFboth %>% select(1:66)
#Tree Model
#Oversampling
DFMFdt.o.20<-rpart(formula = Tournament ~ ., data = DFMFover, method = "class")
rpart.plot(DFMFdt.o.20)
DFMF_tst_pred.o<-predict(DFMFdt.o.20, DFMFtst, type = "class")
table(predicted = DFMF_tst_pred.o, actual = DFMFtst$Tournament)
vip(DFMFdt.o.20, num_features = 90)
DFMFtree_acc.o<- calc_acc(predicted = DFMF_tst_pred.o, actual = DFMFtst$Tournament)
DFMFtree_acc.o
#Undersampling
DFMFdt.u.20<-rpart(formula = Tournament ~ ., data = DFMFunder, method = "class")
rpart.plot(DFMFdt.u.20)
DFMF_tst_pred.u<-predict(DFMFdt.u.20, DFMFtst, type = "class")
table(predicted = DFMF_tst_pred.u, actual = DFMFtst$Tournament)
vip(DFMFdt.u.20, num_features = 90)
DFMFtree_acc.u<- calc_acc(predicted = DFMF_tst_pred.u, actual = DFMFtst$Tournament)
DFMFtree_acc.u
#Both
DFMFdt.b.20<-rpart(formula = Tournament ~ ., data = DFMFboth, method = "class")
rpart.plot(DFMFdt.b.20)
DFMF_tst_pred.b<-predict(DFMFdt.b.20, DFMFtst, type = "class")
table(predicted = DFMF_tst_pred.b, actual = DFMFtst$Tournament)
vip(DFMFdt.b.20, num_features = 90)
DFMFtree_acc.b<- calc_acc(predicted = DFMF_tst_pred.b, actual = DFMFtst$Tournament)
DFMFtree_acc.b
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
DFMFrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(DFMFboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
DFMFgbm_tune <- train(Tournament ~ ., data = na.omit(DFMFboth),method = "gbm",trControl = cv_5, verbose = T, tuneGrid = gbm_grid)
DFMF_bag<-randomForest(as.factor(Tournament)~., data = na.omit(DFMFboth), mtry = as.numeric(DFMFrf_tune$bestTune), importance = T, ntrees = 500)
DFMF_bag
DFMF_rf<-randomForest(as.factor(Tournament)~., data = na.omit(DFMFboth), mtry = as.numeric(DFMFrf_tune$bestTune), importance = T, ntrees = 500)
DFMF_rf
#Gradient Boosting Model
DFMF_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~.-In.Out, data = DFMFboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
DFMF_boost
DFMF_boost_pred<-ifelse(predict(DFMF_boost, DFMFtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = DFMF_boost_pred, actual = DFMFtst$Tournament)
DFMFboost_acc<- calc_acc(predicted = DFMF_boost_pred, actual = DFMFtst$Tournament)
DFMFboost_acc
#Neural Networks



#DF 2020
DF.20<-dat.20 %>% filter(Pos == "DF") %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
DF.p.20<-DF.20 %>% select(1:65)
#PCA 
DFpca.20<-prcomp(na.omit(DF.p.20), center = T, scale. = T)
summary(DFpca.20)
plot(DFpca.20, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
fviz_nbclust(as.data.frame(-DFpca.20$x[,1:2]),kmeans, method = "wss")
fviz_nbclust(as.data.frame(-DFpca.20$x[,1:2]),kmeans, method = "silhouette")
fviz_nbclust(as.data.frame(-DFpca.20$x[,1:2]),kmeans, method = "gap_stat")
k = 2
kmeans.pca.20<-kmeans(as.data.frame(-DFpca.20$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.20,as.data.frame(-DFpca.20$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(DF.20),replace = T, prob = c(0.8,0.2))
DFtrn<-DF.20[idx==1,]
DFtst<-DF.20[idx==2,]
table(DFtrn$Tournament)
#Balancing Data
library(ROSE)
DFover<-ovun.sample(Tournament~.-In.Out, data = DFtrn, method = "over", N = nrow(DFtrn), seed = 122)$data
DFunder<-ovun.sample(Tournament~.-In.Out, data = DFtrn, method = "under", N = nrow(DFtrn)*0.7, seed = 125)$data
DFboth<-ovun.sample(Tournament~.-In.Out, data = DFtrn, method = "both", N = nrow(DFtrn), seed = 156)$data
table(DFover$Tournament) 
table(DFunder$Tournament) 
table(DFboth$Tournament)
DFover<-DFover %>% select(1:66)
DFunder<-DFunder %>% select(1:66)
DFboth<-DFboth %>% select(1:66)
#Tree Model
#Oversampling
DFdt.o.20<-rpart(formula = Tournament ~ ., data = DFover, method = "class")
rpart.plot(DFdt.o.20)
DF_tst_pred.o<-predict(DFdt.o.20, DFtst, type = "class")
table(predicted = DF_tst_pred.o, actual = DFtst$Tournament)
vip(DFdt.o.20, num_features = 90)
DFtree_acc.o<- calc_acc(predicted = DF_tst_pred.o, actual = DFtst$Tournament)
DFtree_acc.o
#Undersampling
DFdt.u.20<-rpart(formula = Tournament ~ ., data = DFunder, method = "class")
rpart.plot(DFdt.u.20)
DF_tst_pred.u<-predict(DFdt.u.20, DFtst, type = "class")
table(predicted = DF_tst_pred.u, actual = DFtst$Tournament)
vip(DFdt.u.20, num_features = 90)
DFtree_acc.u<- calc_acc(predicted = DF_tst_pred.u, actual = DFtst$Tournament)
DFtree_acc.u
#Both
DFdt.b.20<-rpart(formula = Tournament ~ ., data = DFboth, method = "class")
rpart.plot(DFdt.b.20)
DF_tst_pred.b<-predict(DFdt.b.20, DFtst, type = "class")
table(predicted = DF_tst_pred.b, actual = DFtst$Tournament)
vip(DFdt.b.20, num_features = 90)
DFtree_acc.b<- calc_acc(predicted = DF_tst_pred.b, actual = DFtst$Tournament)
DFtree_acc.b
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
#Neural Networks



#MF 2020
MF.20<-dat.20 %>% filter(Pos == "MF") %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
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
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(MF.20),replace = T, prob = c(0.8,0.2))
MFtrn<-MF.20[idx==1,]
MFtst<-MF.20[idx==2,]
table(MFtrn$Tournament)
#Balancing Data
library(ROSE)
MFover<-ovun.sample(Tournament~.-In.Out, data = MFtrn, method = "over", N = nrow(MFtrn), seed = 122)$data
MFunder<-ovun.sample(Tournament~.-In.Out, data = MFtrn, method = "under", N = nrow(MFtrn)*0.7, seed = 125)$data
MFboth<-ovun.sample(Tournament~.-In.Out, data = MFtrn, method = "both", N = nrow(MFtrn), seed = 156)$data
table(MFover$Tournament) 
table(MFunder$Tournament) 
table(MFboth$Tournament)
MFover<-MFover %>% select(1:66)
MFunder<-MFunder %>% select(1:66)
MFboth<-MFboth %>% select(1:66)
#Tree Model
#Oversampling
MFdt.o.20<-rpart(formula = Tournament ~ ., data = MFover, method = "class")
rpart.plot(MFdt.o.20)
MF_tst_pred.o<-predict(MFdt.o.20, MFtst, type = "class")
table(predicted = MF_tst_pred.o, actual = MFtst$Tournament)
vip(MFdt.o.20, num_features = 90)
MFtree_acc.o<- calc_acc(predicted = MF_tst_pred.o, actual = MFtst$Tournament)
MFtree_acc.o
#Undersampling
MFdt.u.20<-rpart(formula = Tournament ~ ., data = MFunder, method = "class")
rpart.plot(MFdt.u.20)
MF_tst_pred.u<-predict(MFdt.u.20, MFtst, type = "class")
table(predicted = MF_tst_pred.u, actual = MFtst$Tournament)
vip(MFdt.u.20, num_features = 90)
MFtree_acc.u<- calc_acc(predicted = MF_tst_pred.u, actual = MFtst$Tournament)
MFtree_acc.u
#Both
MFdt.b.20<-rpart(formula = Tournament ~ ., data = MFboth, method = "class")
rpart.plot(MFdt.b.20)
MF_tst_pred.b<-predict(MFdt.b.20, MFtst, type = "class")
table(predicted = MF_tst_pred.b, actual = MFtst$Tournament)
vip(MFdt.b.20, num_features = 90)
MFtree_acc.b<- calc_acc(predicted = MF_tst_pred.b, actual = MFtst$Tournament)
MFtree_acc.b
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
#Neural Networks


#MFFW 2020
MFFW.20<-dat.20 %>% filter(Pos == "MFFW") %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
MFFW.p.20<-MFFW.20 %>% select(1:65)
#PCA 
MFFWpca.20<-prcomp(na.omit(MFFW.p.20), center = T, scale. = T)
summary(MFFWpca.20)
plot(MFFWpca.20, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
fviz_nbclust(as.data.frame(-MFFWpca.20$x[,1:2]),kmeans, method = "wss")
fviz_nbclust(as.data.frame(-MFFWpca.20$x[,1:2]),kmeans, method = "silhouette")
fviz_nbclust(as.data.frame(-MFFWpca.20$x[,1:2]),kmeans, method = "gap_stat")
k = 2
kmeans.pca.20<-kmeans(as.data.frame(-MFFWpca.20$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.20,as.data.frame(-MFFWpca.20$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(MFFW.20),replace = T, prob = c(0.8,0.2))
MFFWtrn<-MFFW.20[idx==1,]
MFFWtst<-MFFW.20[idx==2,]
table(MFFWtrn$Tournament)
#Balancing Data
library(ROSE)
MFFWover<-ovun.sample(Tournament~., data = MFFWtrn, method = "over", N = nrow(MFFWtrn), seed = 122)$data
MFFWunder<-ovun.sample(Tournament~., data = MFFWtrn, method = "under", N = nrow(MFFWtrn)*0.7, seed = 125)$data
MFFWboth<-ovun.sample(Tournament~., data = MFFWtrn, method = "both", N = nrow(MFFWtrn), seed = 156)$data
table(MFFWover$Tournament) 
table(MFFWunder$Tournament) 
table(MFFWboth$Tournament)
MFFWover<-MFFWover %>% select(1:66)
MFFWunder<-MFFWunder %>% select(1:66)
MFFWboth<-MFFWboth %>% select(1:66)
#Tree Model
#Oversampling
MFFWdt.o.20<-rpart(formula = Tournament ~ ., data = MFFWover, method = "class")
rpart.plot(MFFWdt.o.20)
MFFW_tst_pred.o<-predict(MFFWdt.o.20, MFFWtst, type = "class")
table(predicted = MFFW_tst_pred.o, actual = MFFWtst$Tournament)
vip(MFFWdt.o.20, num_features = 90)
MFFWtree_acc.o<- calc_acc(predicted = MFFW_tst_pred.o, actual = MFFWtst$Tournament)
MFFWtree_acc.o
#Undersampling
MFFWdt.u.20<-rpart(formula = Tournament ~ ., data = MFFWunder, method = "class")
rpart.plot(MFFWdt.u.20)
MFFW_tst_pred.u<-predict(MFFWdt.u.20, MFFWtst, type = "class")
table(predicted = MFFW_tst_pred.u, actual = MFFWtst$Tournament)
vip(MFFWdt.u.20, num_features = 90)
MFFWtree_acc.u<- calc_acc(predicted = MFFW_tst_pred.u, actual = MFFWtst$Tournament)
MFFWtree_acc.u
#Both
MFFWdt.b.20<-rpart(formula = Tournament ~ ., data = MFFWboth, method = "class")
rpart.plot(MFFWdt.b.20)
MFFW_tst_pred.b<-predict(MFFWdt.b.20, MFFWtst, type = "class")
table(predicted = MFFW_tst_pred.b, actual = MFFWtst$Tournament)
vip(MFFWdt.b.20, num_features = 90)
MFFWtree_acc.b<- calc_acc(predicted = MFFW_tst_pred.b, actual = MFFWtst$Tournament)
MFFWtree_acc.b
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
MFFWrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(MFFWboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
MFFW_bag<-randomForest(as.factor(Tournament)~., data = na.omit(MFFWboth), mtry = as.numeric(MFFWrf_tune$bestTune), importance = T, ntrees = 500)
MFFW_bag
MFFW_rf<-randomForest(as.factor(Tournament)~., data = na.omit(MFFWboth), mtry = as.numeric(MFFWrf_tune$bestTune), importance = T, ntrees = 500)
MFFW_rf
#Gradient Boosting Model
MFFW_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~., data = MFFWboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
MFFW_boost
MFFW_boost_pred<-ifelse(predict(MFFW_boost, MFFWtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = MFFW_boost_pred, actual = MFFWtst$Tournament)
MFFWboost_acc<- calc_acc(predicted = MFFW_boost_pred, actual = MFFWtst$Tournament)
MFFWboost_acc
#Neural Networks

#MFDF 2020
MFDF.20<-dat.20 %>% filter(Pos == "MFDF") %>% select(8:72, Tournament, In.Out) %>% filter(In.Out == "Yes")
MFDF.p.20<-MFDF.20 %>% select(1:65)
#PCA 
MFDFpca.20<-prcomp(na.omit(MFDF.p.20), center = T, scale. = T)
summary(MFDFpca.20)
plot(MFDFpca.20, type = "l", main = "Principal Component Analysis - Optimal number of components")

#K-means
fviz_nbclust(as.data.frame(-MFDFpca.20$x[,1:2]),kmeans, method = "wss")
fviz_nbclust(as.data.frame(-MFDFpca.20$x[,1:2]),kmeans, method = "silhouette")
fviz_nbclust(as.data.frame(-MFDFpca.20$x[,1:2]),kmeans, method = "gap_stat")
k = 2
kmeans.pca.20<-kmeans(as.data.frame(-MFDFpca.20$x[,1:2]), centers = k, nstart = 50)
fviz_cluster(kmeans.pca.20,as.data.frame(-MFDFpca.20$x[,1:2]))#No reasonable predictive model possible

#Decision Trees
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

#Splitting into test and training dataset
set.seed(123)
idx<-sample(2,nrow(MFDF.20),replace = T, prob = c(0.8,0.2))
MFDFtrn<-MFDF.20[idx==1,]
MFDFtst<-MFDF.20[idx==2,]
table(MFDFtrn$Tournament)
#Balancing Data
library(ROSE)
MFDFover<-ovun.sample(Tournament~., data = MFDFtrn, method = "over", N = nrow(MFDFtrn), seed = 122)$data
MFDFunder<-ovun.sample(Tournament~., data = MFDFtrn, method = "under", N = nrow(MFDFtrn)*0.7, seed = 125)$data
MFDFboth<-ovun.sample(Tournament~., data = MFDFtrn, method = "both", N = nrow(MFDFtrn), seed = 156)$data
table(MFDFover$Tournament) 
table(MFDFunder$Tournament) 
table(MFDFboth$Tournament)
MFDFover<-MFDFover %>% select(1:66)
MFDFunder<-MFDFunder %>% select(1:66)
MFDFboth<-MFDFboth %>% select(1:66)
#Tree Model
#Oversampling
MFDFdt.o.20<-rpart(formula = Tournament ~ ., data = MFDFover, method = "class")
rpart.plot(MFDFdt.o.20)
MFDF_tst_pred.o<-predict(MFDFdt.o.20, MFDFtst, type = "class")
table(predicted = MFDF_tst_pred.o, actual = MFDFtst$Tournament)
vip(MFDFdt.o.20, num_features = 90)
MFDFtree_acc.o<- calc_acc(predicted = MFDF_tst_pred.o, actual = MFDFtst$Tournament)
MFDFtree_acc.o
#Undersampling
MFDFdt.u.20<-rpart(formula = Tournament ~ ., data = MFDFunder, method = "class")
rpart.plot(MFDFdt.u.20)
MFDF_tst_pred.u<-predict(MFDFdt.u.20, MFDFtst, type = "class")
table(predicted = MFDF_tst_pred.u, actual = MFDFtst$Tournament)
vip(MFDFdt.u.20, num_features = 90)
MFDFtree_acc.u<- calc_acc(predicted = MFDF_tst_pred.u, actual = MFDFtst$Tournament)
MFDFtree_acc.u
#Both
MFDFdt.b.20<-rpart(formula = Tournament ~ ., data = MFDFboth, method = "class")
rpart.plot(MFDFdt.b.20)
MFDF_tst_pred.b<-predict(MFDFdt.b.20, MFDFtst, type = "class")
table(predicted = MFDF_tst_pred.b, actual = MFDFtst$Tournament)
vip(MFDFdt.b.20, num_features = 90)
MFDFtree_acc.b<- calc_acc(predicted = MFDF_tst_pred.b, actual = MFDFtst$Tournament)
MFDFtree_acc.b
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
MFDFrf_tune <- train(as.factor(Tournament) ~ ., data = na.omit(MFDFboth), method = "rf", trControl = oob, verbose = FALSE, tuneGrid = rf_grid)
MFDF_bag<-randomForest(as.factor(Tournament)~., data = na.omit(MFDFboth), mtry = as.numeric(MFDFrf_tune$bestTune), importance = T, ntrees = 500)
MFDF_bag
MFDF_rf<-randomForest(as.factor(Tournament)~., data = na.omit(MFDFboth), mtry = as.numeric(MFDFrf_tune$bestTune), importance = T, ntrees = 500)
MFDF_rf
#Gradient Boosting Model
MFDF_boost<-gbm(ifelse(Tournament == "Yes", 1, 0)~.-In.Out, data = MFDFboth, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
MFDF_boost
MFDF_boost_pred<-ifelse(predict(MFDF_boost, MFDFtst, n.trees = 5000, "response")>0.5, "Yes", "No")
table(predicted = MFDF_boost_pred, actual = MFDFtst$Tournament)
MFDFboost_acc<- calc_acc(predicted = MFDF_boost_pred, actual = MFDFtst$Tournament)
MFDFboost_acc
#Neural Networks

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

#Decision Trees
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

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
GKunder<-ovun.sample(Tournament~., data = GKtrn, method = "under", N = 7, seed = 189)$data
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
vip(GKdt.o.20, num_features = 90)
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
library(randomForest)
library(gbm)
library(ISLR)
library(Rcpp)
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
<<<<<<< HEAD:2020 Model.R
GKboost_acc
=======
GKboost_acc
>>>>>>> 29033a05683867ebfcc8e5915c922a4cae3cca6d:Assignment 1.R
