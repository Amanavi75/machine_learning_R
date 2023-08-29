# knn algorithm on iris dataset

dataset= iris
View(iris)
table(dataset$Species)
#cheking the missing values 
sum(is.na(dataset)) 
round(prop.table(table(dataset$Species)) * 100, digits = 1)
normalize <- function(x) {
  num <- x-min(x)
  denom<- max(x)-min(x)
  return (num/denom)
}  
#using normalize function 
iris_norm <- as.data.frame(lapply(dataset[1:4], normalize))
summary(iris_norm)
set.seed(1234)
# using sample function for getting sample data for the training the model
ind= sample (2,nrow(dataset),replace=TRUE,prob=c(0.70,0.30))
ind
prop.table(table(ind))
round(prop.table(table(ind))*100 , digit=1)

dataset.training<- dataset[ind==1,1:4] # training the dataset 
dataset.training



dataset.test= dataset[ind==2,1:4] # test the required part of the dataset
dataset.test

dataset.trainLabels<-dataset[ind==1,5] # tarinLabels
dataset.trainLabels

dataset.testLabels<-dataset[ind==2,5]
dataset.testLabels

install.packages("class")
library("class")
#creating the model on the basis of trained and tested data 
dataset_pred<- knn(train = dataset.training, test = dataset.test,
                      cl = dataset.trainLabels, k = 3)

install.packages("gmodels")
library(gmodels)
CrossTable(x=dataset.testLabels , y=dataset_pred , prop.chisq = FALSE)

tab<-table(dataset_pred,dataset.testLabels)
# check for the accuracy without using the direct functions 
accuracy <- function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy(tab)
