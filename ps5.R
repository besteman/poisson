# Title: Problem Set 5
# Author: Justin Besteman


# Clean up
rm(list = ls())


library(readr)

theData <- read.table("Poisson.dat")

names(theData) <- c("numVisits" , "x1" , "x2")

sumVisits <- sum(theData$numVisits)

n <- nrow(theData)

lambdaHat <- sumVisits / n 

print(lambdaHat)

standardError <- lambdaHat / n

print(standardError)

rowHat <- 2 / (sum(theData$numVisits) / n)

print(rowHat)


standardErrorRowHat <- ((2^3 * (rowHat + 1)) / (lambdaHat^4 * rowHat^2))

print(standardErrorRowHat)



theModel <- glm(numVisits ~ . , data = theData , poisson(link="log"))

# print(theModel)

summaryModel <- summary(theModel)

print(summaryModel)




