#Simple Exploratory Analysis
#
x <- read.csv("../bank-teller.csv")
max(x$svc_time)
min(x$svc_time)
mean(x$svc_time)

hist(x$svc_time)

hist(x$svc_time, main="Bank teller service times", xlab="Service time", ylab="Number of occurences")
hist(x$svc_time, breaks=14, main="Bank Teller Service Times")
hist(x$svc_time, freq=FALSE)
plot(density(x$svc_time), main="Density Plot")

boxplot(x$svc_time)

