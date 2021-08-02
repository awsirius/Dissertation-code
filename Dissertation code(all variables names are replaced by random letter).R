############################
#     Data cleansing       #
############################

# load the library, this is for loading sas7bat file.
# if your laptop has not installed this package before, use this line of code first.
# install.packages(haven)
library(haven)
# read the dataset
wave<-read_sas("file.sas7bdat")
# remove the specified cases(XX) which are not in the scope of this study depending on the response rate definition
wavenew<-wave[-which(wave$V1=="XX"),]
# create a new variables for the response variable
wavenew$Respondent<-NA # first fill NA
wavenew$Respondent[wavenew$V2==1.1]<-"respondent" #if V2=1.1, then fill respondent
wavenew$Respondent[wavenew$V2!=1.1]<-"nonrespondent" #if not, then nonrespondent
# select the variables
final<-wavenew[,c(3,6,8,10,15,17,18,19,37,40,42,44,45,46,47,48,
                  50,52,53,54,55,57,58,59,60,61,77,78,
                  79,80,81,82,83,84,88,89,91,
                  92,93,94,95,96,97,98)]
# convert into dataframe
final<-as.data.frame(final)

# install.packages("magrittr")
# install.packages("Amelia")
# load the library
require(magrittr)
require(Amelia)
# visualize the missing value
missmap(final)

# firstly check the missing value of respondent variable
x<-final[which(is.na(final$Respondent)),]
# remove it
final<-final[-which(is.na(final$Respondent)),]

# install.packages("dplyr")
# load the library
library(dplyr)
# convert 1 to the corresponding value for variables: AA, BB, CC
final[which(final$V3==1),"V3"]<-"AA"
final[which(final$V4==1),"V4"]<-"BB"
final[which(final$V5==1),"V5"]<-"CC"
# merge these columns, V3;V4;V5
final<-mutate(final,V6=coalesce(V3,V4,V5))
#remove the column:V3;V4;V5
final<-final[,-which(colnames(final) %in% c("V3","V4","V5"))]

# check the missing map again
missmap(final)

# remove the variable V7 as this column is empty
final<-final[,-grep("V7",colnames(final))]

# convert the na to no for V8 variable, 1 to yes
final$V8[is.na(final$V8)]<-"no"
final[which(final$V8==1),"V8"]<-"yes"

# convert the na to no for V9 variable, 1 to yes
final$V9[is.na(final$V9)]<-"no"
final[which(final$V9==1),"V9]<-"yes"

# convert the na to 0 for V10 variable
final$V10[is.na(final$V10)]<-0
# load the library
library(lubridate)
# convert time format to numerical format
final$V10<-as.numeric(final$V10)/60

# convert 1 to 0 for variable "V11"
final[which(final$V11==1),"V11"]<-0
# merge columns
final<-mutate(final,V13=coalesce(V11,V12))
# remove V11 and V12 variables
final<-final[,-which(colnames(final) %in% c("V11","V12"))]

# remove the rows which have the na in V14 column.
final<-final[-which(is.na(final$V14)),]

# convert 1 to 2 for variable "V15"
final[which(final$V15==1),"V15"]<-2
# merge columns, 1:V16;2:V15
final<-mutate(final,V17=coalesce(V15,V16))
# remove
final<-final[,-which(colnames(final) %in% c("V15","V16"))]

# complete the V6 according to the V13 and V17 variables
n<-which(is.na(final$V6))
for(i in n) {
  if(is.na(final[i,"V13"])&is.na(final[i,"V17"])){
    final[i,"V6"]<-"AA"
  }else if (final[i,"V13"]=="0") {
    final[i,"V6"]<-"CC"
  }
}

# remove V13 and V17 variables
final<-final[,-which(colnames(final) %in% c("V13","V17"))]

# remove the rows with blank value for variable "V18"
final<-final[-which(final$V18==""),]

# change blank into na 
for(i in 1:length(final)){
  final[which(final[,i]==""),i]<-NA
}

# check the missing value
par(oma=c(9,2,2,2),mar=c(9,2,2,2))
missmap(final)



############################
#     Bespoke KNN          #
############################


#firstly, use the completed variables to develop classfication model.
# generate a dataset
first<-as.data.frame(t(na.omit(t(final))))
# move the variable respondent into last column
first<-first%>% select(-one_of('Respondent'),one_of('Respondent'))
# remove three variables which should be excluded in the following classification.
first<-first[,-c(4,5,7)]
# classification model
knnDIY3 <- function(train,test,labels,k){
  # determine whether the dim of the train and test is the consistent
  if(ncol(train)!=ncol(test)) stop("dims of 'test' and 'train' differ")
  # determine whether the length of train and label is consistent
  if(nrow(train)!=length(labels)) stop("'train' and 'class' have different lengths")
  # change the format
  labels <- as.character(labels)
  # extract continous variables from training set
  train1<-train[,c(3,8,9,10,16,18)]
  # extract categorical variables from traing set
  train2<-train[,c(1,2,4,5,6,7,11,12,13,14,15,17,19,20)]
  classify0 <- function(vec){
    vec1<-vec[c(3,8,9,10,16,18)]
    vec2<-vec[c(1,2,4,5,6,7,11,12,13,14,15,17,19,20)]
    # Euclidean distance calculation
    diffMat <- matrix(as.numeric(vec1),nrow(train1),ncol(train1),byrow = T) - apply(train1,2,as.numeric)
    distances1 <- diffMat^2  %>% apply(.,1,sum) %>% sqrt
    # hamming distance calculation function
    dis<-function(x){
      ham<-hamming.distance(x,vec2)
      return(ham)
    }
    # hamming distance
    distances2<-apply(t(train2),2,dis)
    # bespoke distance using simple addition
    distances<-distances1+distances2
    # sort the distance
    sortedDistIndexes <- order(distances)
    # set blank vector
    kMaxDistLabels <- vector(length = k)
    # get the label of k-nearest neighbours
    for(i in 1:k){
      kMaxDistLabels[i] <- labels[sortedDistIndexes[i]]
    }
    # use table to get frequency statistics
    predictLabel <- table(kMaxDistLabels) %>% which.max %>% names
    return(predictLabel)
  }
  # get the whole label of test test
  allPredict <- apply(test,1,classify0)
  return(allPredict)
}

# copy the dataset
modeldata<-first
# change the format
modeldata[,c(3,8,9,10,16,18)]<-lapply(modeldata[,c(3,8,9,10,16,18)], as.numeric)
modeldata[,c(1,2,4,5,6,7,11,12,13,14,15,17,19,20,21)]<-lapply(modeldata[,c(1,2,4,5,6,7,11,12,13,14,15,17,19,20,21)], as.factor)
# scale the numerical data
modeldata[,c(3,8,9,10,16,18)]<-scale(modeldata[,c(3,8,9,10,16,18)])
# set the seed to ensure that the results are the same every running
set.seed(1234)
# split the data
train.rows3<-sample(1:nrow(modeldata),0.75*dim(modeldata)[1])
# training set without response variable
train1<-modeldata[train.rows3,-21]
# train set with response variable
data0<-modeldata[train.rows3,]
# test set without response variable
test1<-modeldata[-train.rows3,-21]
# the number for folds
K<-5
# the row number of the whole dataset
N<-nrow(modeldata)
# the row number of the train set
N_train<-nrow(train1)
folds<-rep(1:K,ceiling(N/K))
folds<-sample(folds) # random permute
folds<-folds[1:N_train]# ensure we got N_train data points
acc <- matrix(NA, K, 20) # accuracy of the classifiers in the k-folds
# function to compute classification accuracy
class_acc <- function(y, yhat) {
  tab <- table(y, yhat)
  return(sum(diag(tab))/sum(tab) ) 
}
# load the library
library(e1071)
# k-fold cross-validation
for(k in 1:K){
  train_fold<-which(folds!=k)
  # get validation set
  validation<-setdiff(1:N_train,train_fold)
  labels3<-data0[-validation,21]
  for(x in 1:20){
    results<-knnDIY3(train1[-validation,],train1[validation,],labels3,x)
    acc[k,x]<-class_acc(results,data0$Respondent[validation])
  }
}
# compute the average accuracy for the 5-folds
meanacc<-colMeans(acc)
# set the graph parameter
par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(4,4,3,3))
# plot the average accuracy
plot(1:20,meanacc,type = "b",xlab = "k",ylab = "Mean accuracy rate value",main = "Mean accuracy against k under 5 folds",mgp=c(3,1,0))
# load the library
library(gmodels)
# label of training set
labels4<-modeldata[train.rows3,21]
# get the prediction
results1<-knnDIY3(train1,test1,labels4,13)
# combine it together
combineresults<-modeldata[-train.rows3,]
combineresults$predict<-results1
# confusion matrix
CrossTable(modeldata[-train.rows3,21],results1,dnn = c("Actual","Predicted"),prop.chisq = FALSE)



############################
#     Lasso Regression     #
############################

# load the library
library(caret)
# extract the continuous variable
contin<-modeldata[,c(3,8,9,10,16,18)]
# extract te categorical variable
categ<-modeldata[,c(1,2,4,5,6,7,11,12,13,14,15,17,19,20)]
# extract the response variable
respon<-modeldata[,21]
# load the library
library(dummies)
# convert to dummy variable
catdummy<-dummy.data.frame(categ)
# merge the continuous variables and dummy variables
new<-cbind(contin,catdummy)
# add the response variable
new<-cbind(new,modeldata$Respondent)
# set the seed
set.seed(1234)
#explanatory variables
x<-as.matrix(new[,-251])
#  response variable
y<-new$`modeldata$Respondent`
# load the library
library(glmnet)
# split the data into train set and testing set
set.seed(1234)
train_index<-sample(1:nrow(new),0.75*dim(new)[1])
x_train<-as.matrix(new[train_index,-251])
y_train<-new$`modeldata$Respondent`[train_index]
x_test<-as.matrix(new[-train_index,-251])
y_test<-new$`modeldata$Respondent`[-train_index]
# fit the model, class represents that the results will show misclassification error against log(lambda)
fitcv1<-cv.glmnet(x_train,y_train,alpha=1,family='binomial',nfolds = 5,type.measure = 'class')
# plot the model
plot(fitcv1)
# optimal lambda value
fitcv1$lambda.min
# it gives a model with excellent performance but the least number of independent variables
fitcv1$lambda.1se
# predict based on the test set 
preds<-predict(fitcv1,s=fitcv1$lambda.1se,newx = x_test,type="response")
# merge actual and prediction
re<-cbind(y_test,preds)
# the first 5 rows
head(re)
# install and load the library
#install.packages("ROCR")
library(ROCR)
# provide in input the estimated probabilities and the actual value of the response variable
pred_obj<-prediction(re[,2],re[,1])
# compute the sensitivity
sens<-performance(pred_obj,"sens")
# compute the specificity
spec<-performance(pred_obj,"spec")
tau<-sens@x.values[[1]]
# the optimal threshold tau can be found by maximizing the sum of sensitivity and specificity for different values of tau
#the sum of sensitivity and specificity
sens_spec<-sens@y.values[[1]]+spec@y.values[[1]]
# find the index corresponding to the maximium value
best<-which.max(sens_spec)
# plot the curve
plot(tau,sens_spec,type = "l",main="The sum of sensitivity and specificity against tau ")
points(tau[best],sens_spec[best],pch=19,col=adjustcolor("darkorange2",0.5))
# determine the optimal threshold
threshold<-tau[best]
# classification for optimal tau
predsnew<-as.integer(preds>threshold)
# confusion matrix
table(predsnew,y_test)
# combine the actual value, the probablity and the prediction
combinefirst<-modeldata[-train_index,]
combinefirst$probablity<-preds
combinefirst$predict<-predsnew
combinefirst$predict[combinefirst$predict==0]<-"nonrespondent"
combinefirst$predict[combinefirst$predict==1]<-"respondent"
# get the coefficients of each variable
coefficients1<-as.data.frame(as.matrix(coef(fitcv1,s=fitcv1$lambda.1se)))
# create the new column from the index.
coefficients1$variable<-rownames(coefficients1)
# re-index
rownames(coefficients1)<-1:nrow(coefficients1)
# variable index which coefficient is not 0
Active.Index1<-which(coefficients1$s1!=0)
# the value of variablewhich coefficient is not 0
Active.coefficients1<-coefficients1[Active.Index1,]
Active.coefficients1



##############################################
#     KNN using the selected variables      #
##############################################

# Repeat
modeldata1<-modeldata[,c(3,6,10,16,21)]
set.seed(1234)
train.rows4<-sample(1:nrow(modeldata1),0.75*dim(modeldata1)[1])
train2<-modeldata1[train.rows4,-5]
data1<-modeldata1[train.rows4,]
test2<-modeldata1[-train.rows4,-5]
K<-5
N1<-nrow(modeldata1)
N_train1<-nrow(train2)
folds1<-rep(1:K,ceiling(N1/K))
folds1<-sample(folds1)
folds1<-folds1[1:N_train1]
acc1 <- matrix(NA, K, 20)
class_acc <- function(y, yhat) {
  tab <- table(y, yhat)
  return(sum(diag(tab))/sum(tab) ) 
}
library(e1071)
 knnDIY4 <- function(train,test,labels,k){
   if(ncol(train)!=ncol(test)) stop("dims of 'test' and 'train' differ")
   if(nrow(train)!=length(labels)) stop("'train' and 'class' have different lengths")
   labels <- as.character(labels) 
   train1<-train[,c(1,3,4)]
   train2<-train[,c(2)]
   classify0 <- function(vec){
     vec1<-vec[c(1,3,4)]
     vec2<-vec[c(2)]
     diffMat <- matrix(as.numeric(vec1),nrow(train1),ncol(train1),byrow = T) - apply(train1,2,as.numeric)
     distances1 <- diffMat^2  %>% apply(.,1,sum) %>% sqrt
     dis<-function(x){
       ham<-hamming.distance(x,vec2)
       return(ham)
     }
     distances2<-apply(t(train2),2,dis)
     distances<-distances1+distances2
     sortedDistIndexes <- order(distances)
     kMaxDistLabels <- vector(length = k)
     for(i in 1:k){
       kMaxDistLabels[i] <- labels[sortedDistIndexes[i]]
     }
     predictLabel <- table(kMaxDistLabels) %>% which.max %>% names
     return(predictLabel)
   }
   allPredict <- apply(test,1,classify0)
   return(allPredict)
 }
 for(k in 1:K){
   train_fold1<-which(folds1!=k)
   validation1<-setdiff(1:N_train1,train_fold1)
   labels4<-data1[-validation1,5]
   for(x in 1:20){
     results1<-knnDIY4(train2[-validation1,],train2[validation1,],labels4,x)
     acc1[k,x]<-class_acc(results1,data1$Respondent[validation1])
   }
 }

meanacc1<-colMeans(acc1)
par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(4,4,3,3))
plot(1:20,meanacc1,type = "b",xlab = "k",ylab = "Mean accuracy rate value",main = "Mean accuracy against K under 5 folds cross-validation",mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=2.5,cex.main=2)
labels5<-modeldata1[train.rows4,5]
results2<-knnDIY4(train2,test2,labels5,13)
CrossTable(modeldata1[-train.rows4,5],results2,dnn = c("Actual","Predicted"),prop.chisq = FALSE)





