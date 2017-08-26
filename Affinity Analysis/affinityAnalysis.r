#Affinity Analysis
#-----------------------
library(arules)
grocery <- read.csv("grocery.csv")

binary <- as(split(grocery[,2], grocery[,1]), "transactions")
rules <- apriori(binary, parameter=list(support=0.1, confidence = 0.1))
rules
#Inspect rules to examine the lift, confidence and support
inspect(rules)
inspect(sort(rules, by="confidence"))

rules <- apriori(binary, parameter=list(support=0.2, confidence = 0.8))
inspect(rules)