# Title: Problem Set 5
# Author: Justin Besteman


# Clean up
rm(list = ls())


library(readr)

# Reading in the data
theData <- read.table("Poisson.dat")

# Renaming the column names
names(theData) <- c("numVisits" , "x1" , "x2")

# -------------------- Part a ---------------------

# Summing up the numVisits
sumVisits <- sum(theData$numVisits)

# sumVisits = 210

# Finding the number of Observation
n <- nrow(theData)

# n = 100

# Finding the expect truth
lambdaHat <- sumVisits / n 

# 2.1 
print(lambdaHat)

# Finding Error
standardError <- sqrt(lambdaHat / n)

# .1449138
print(standardError)

# -------------------- End ---------------------


# -------------------- Part b ------------------

rowHat <- 2 / (sum(theData$numVisits) / n)

#  0.952381
print(rowHat)

standardErrorRowHat <- ((2^3 * (rowHat + 1)) / (lambdaHat^4 * rowHat^2))

standardErrorRowHat <- sqrt(standardErrorRowHat)

print(standardErrorRowHat)

# -------------------- End ---------------------

# -------------------- Part d ------------------

theModel <- glm(numVisits ~ . , data = theData , poisson(link="log"))

summaryModel <- summary(theModel)

print(summaryModel)

# -------------------- End ---------------------


