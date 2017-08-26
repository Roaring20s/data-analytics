#KNN Using the Vacation-trip-classification.csv

vaca <- read.csv("vacation-trip-classification.csv")

#Create the range and store the new column into a new variable
#Big note, this is the standardize technique
#Note on z, if you see a value like 1.11, it means 1.11 std dev. above avg. This would be higher than most people make 
vaca$Family_size_z <- scale(vaca$Family_size)
vaca$Income_z <- scale(vaca$Income)

#load caret package to create data partitions
#remove.packages(c("ggplot2", "data.table"))
#install.packages('Rcpp', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('data.table', dependencies = TRUE)

#Power to partition
library(caret)

set.seed(2015)

#Samp will grab random ID numbers (25 total) 
samp <-createDataPartition(vaca$Result, p=0.6, list=FALSE)
#train.a will use the ID numbers inside vaca to grab rest of info
train.a <- vaca[samp,]
rest <- vaca[-samp,]

#partition the b and test
samp <- createDataPartition(rest$Result, p=0.5, list=FALSE)
train.b <- rest[samp,]
test <- rest[-samp,]

#This will knn and tell us the value of an inserted k number
library(class)

#Find the lowest k that will give you good results, higher k, better score till a certain point
#train.b$pred.1 will hold the predicted results - pred is predicted
train.b$pred.1 <- knn(train.a[,4:5], train.b[,4:5], train.a[,3], 1)

#Make the error matrix table 
tab.1 <- table(train.b$Result, train.b$pred.1, dnn = c("Actual", "Predicted"))

#Same Knn process with k=3
train.b$pred.3 <- knn(train.a[,4:5], train.b[,4:5], train.a[,3], 3)
tab.3 <- table(train.b$Result, train.b$pred.3, dnn = c("Actual", "Predicted"))

#Same Knn process with k=5
train.b$pred.5 <- knn(train.a[,4:5], train.b[,4:5], train.a[,3], 5)
tab.5 <- table(train.b$Result, train.b$pred.5, dnn=c("Actual", "Predicted"))

#Same Knn process with k=7
train.b$pred.7 <- knn(train.a[,4:5], train.b[,4:5], train.a[,3], 7)
tab.7 <- table(train.b$Result, train.b$pred.7, dnn=c("Actual", "Predicted"))

#Same Knn process with k=9
train.b$pred.9 <- knn(train.a[,4:5], train.b[,4:5], train.a[,3],9)
tab.9 <- table(train.b$Result, train.b$pred.9, dnn=c("Actual", "Predicted"))

prop.table(tab.1)
prop.table(tab.3)

#Now put k to the test
test$pred.1 <- knn(train.a[,4:5], test[,4:5], train.a[,3], 1)
tab.test.1 <- table(test$Result, test$pred.1, dnn=c("Actual", "Predicted"))
tab.test.1

#Note, things run worse on the test data then on the partitions
#Now calculate lift to determine how successful your model was
#Get Naive classification is the scenario where you dont have a model
#summary will give 21 buyers out of 40 or being right 52.5% of the time
summary(vaca$Result)
#prop table shows a 57% correct rate meaning you're 9% more correct
prop.table(tab.test.1)

total <-  91 + 24 + 28 + 110

error <- 28+24

ans <- error/total
ans








