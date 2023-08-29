#data preprocessing
getwd()

#importing the dataset
dataset = read.csv(file="C:\\Users\\hp\\Downloads\\Data.csv")

dataset=read.csv(file.choose())
View(dataset)
str(dataset)

#labelling the variables

names(dataset)


names(dataset)<-c("Country","Age","Salary","Purchased")




str(dataset)
View(dataset)
#Checking missing values




is.na(dataset)


sum(is.na(dataset))
View(dataset)

colSums(is.na(dataset))

dataset[1,]

missingdata<-dataset[!complete.cases(dataset), ]


missingdata
#sum(is.na(missingdata))
View(dataset)

#taking care of missing values
dataset$Age=ifelse(is.na(dataset$Age),
                   ave(dataset$Age, FUN= function(x) mean(x,na.rm=TRUE)),
                   dataset$Age)
dataset$Salary=ifelse(is.na(dataset$Salary),
                      ave(dataset$Salary, FUN= function(x) mean(x,na.rm=TRUE)),
                      dataset$Salary)
sum(is.na(dataset))
View(dataset)

str(dataset)

#handling the categorical values
dataset$Country= factor(dataset$Country, 
                        levels=c('France','Spain','Germany'),
                        labels= c(1,2,3))
dataset$Purchased= factor(dataset$Purchased, 
                          levels=c('No','Yes'),
                          labels= c(0,1))
str(dataset)
View(dataset)

