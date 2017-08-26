#Simple Linear Regression

# Step 1 Read in our weight data
water <- read.csv("water.csv")
head(water)
summary(water)

# Step 2 Remove Year column; Year is a label
water <- water[,2:8]
head(water)

# Step 3 Load the Library
library(caret)

# Step 4 Create Partitions

set.seed(2015)
sam <- createDataPartition(water$BSAAM, 
                           p = 0.7, list = FALSE)
train <- water[sam, ]
test <- water[-sam, ]

# Step 5 - kitchen sink
# Run a linear regression model with everything in it
model_1 <- lm(BSAAM ~ ., data=train)

# Step 6 - look at the results
print("Model 1: Kitchen sink with everything\n")
print(summary(model_1))

# Step 7 - remove the weakest variable and run it again
# Here, the weakest variable is APSAB (it has largest p-value)
model_2 <- lm(BSAAM ~ APMAM + APSLAKE + OPBPC + OPRC + OPSLAKE, data =
                train)
print("Model 2: Only APMAM, APSLAKE, OPBPC, OPRC, And OPSLAKE\n")
print(summary(model_2))

# Step 8 - remove the weakest remaining variable and run it again
# Here, the weakest variable is OPBPC (it has largest p-value)
model_3 <- lm(BSAAM ~ APMAM + APSLAKE + OPRC + OPSLAKE, data = train)

print("Model 3: Only APMAM, APSLAKE, OPRC, And OPSLAKE\n")
print(summary(model_3))

# Step 9 - remove the weakest remaining variable and run it again
# Here, the weakest variable is APSLAKE (it has largest p-value)
model_4 <- lm(BSAAM ~ APMAM + OPRC + OPSLAKE, data = train) 
print("Model 4: Only APMAM, OPRC, And OPSLAKE\n")
print(summary(model_4))

# Step 10 - All remaining variables are significant at some level 

# Step 11 - Compute RMSE on test partition for Model 4
pred.test4 <- predict(model_4, test)
rmse4 <- sqrt(mean((test$BSAAM - pred.test4)^2))
rmse4
