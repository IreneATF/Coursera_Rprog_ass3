setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass3/rprog_data_ProgAssignment3-data")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)

## 1. PLOTTING 30-DAY MORTALITY RATES FOR HEART ATTACK

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])