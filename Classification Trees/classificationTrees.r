# Classification Trees
#Set your working directory in hash
setwd("#")
getwd()

# Step 1 Read the Data
ol <- read.csv("Olympics.csv") 

# Step 2 Summary to Determine the Naive Classificatoin
summary(ol)
summary(ol$Medal_Winner)
table(ol$Medal_Winner)

# Step 3 Load the Library
library(caret)
library(rpart)
library(rpart.plot)

# Step 4 Create Partitions
set.seed(2015)
sam <- createDataPartition(ol$Medal_Winner, 
                           p = 0.7, list = FALSE)
train <- ol[sam, ]
test <- ol[-sam, ]

# Step 5 Build the tree
ol.tree <- rpart(Medal_Winner ~ Country + Age + Height + Weight + Sex + Sport, 
                 data = train, control = rpart.control(minbucket = 10, cp = 0))

# Step 6 Print the tree
prp(ol.tree, type = 2, extra = 104, 
    nn = TRUE, fallen.leaves = TRUE,
    faclen = 4, varlen = 8, 
    shadow.col = "gray")

# Step 7 Look at the test output
ol.tree

# Step 8 Generate model preditions on the training data
pred.train <- predict(ol.tree, train, type="class")

# Step 9 Generate the error matrix on the training data
table(train$Medal_Winner, pred.train, 
      dnn = c("Actual", "Predicted"))

# Step 10 Generate the preditions and the eroor matrix on the test data
pred.test <- predict(ol.tree, test, type="class")

table(test$Medal_Winner, pred.test, 
      dnn = c("Actual", "Predicted"))

# Step 10a View the variables for test and train
head(test)

head(train)

# Step 11 Plot the cp table
plotcp(ol.tree)

# Step 12 Prune and print new tree
# cp changed to 0.0034 based on Step 11 plot 
ol.pruned<-prune(ol.tree,0.0034)
prp(ol.pruned,type=2,extra=104,nn=TRUE,fallen.leaves=TRUE,
    faclen=4,varlen=8,shadow.col="gray")

# Step 13 Generate error matrix on pruned tree
pred.test<-predict(ol.pruned,test,type="class")
table(test$Medal_Winner, pred.test,dnn=c("Actual","Predicted"))
