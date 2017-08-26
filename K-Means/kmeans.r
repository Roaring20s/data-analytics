#We will determine the appropriate number of clusters using the auto-mpg.csv
#Step 1: Read the file
auto <- read.csv("auto-mpg.csv")
dar2ed.kmeans.plot(auto, 2:7)
#Book wants to pick 4 clusters
#-----------------------------
#Step 2: Standardize the data
auto <-dar2ed.scale.many(auto, 2:7)
head(auto)
fit <- kmeans(auto[, 10:15], 5)
fit
#the fourth cluster maintains 80% of original variability
auto$cluster = fit$cluster

#Step 5: Principal Component Analysis
#Considered a dimension reduction technique, it will find 10 components that will explain the data