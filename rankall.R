## 3. RANKING HOSPITALS IN ALL STATE

rankall <- function(outcome, num = "best") {
      ## Read outcome data
      
      setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass3/rprog_data_ProgAssignment3-data")
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

      by.state <- split(data,data$State)   ## Splitting data by state
      
      ## Check that the outcome is valid
      
      if(outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
            stop("invalid outcome")
      }
      
## For each state, find the hospital of the given rank
      result <- data.frame(hospital = character(), state = character())
      n.states <- nrow(by.state)
      
      for (x in 1:54) {
      
            state <- names(by.state[x])
      
            state.data <- data.frame(by.state[state])
      
            if(outcome == "heart attack") {                   ## Creating data frame only with wanted 
                  outcome.state <- state.data[,c(2,11)]       ## outcome and hospital names in state
            }else if(outcome == "heart failure") {
                  outcome.state <- state.data[,c(2,17)]
            }else if(outcome == "pneumonia") {
                  outcome.state <- state.data[,c(2,23)]
            }
      
            ## Removing hospitals without available data from calculations
            bad <- c(outcome.state[,2]) != "Not Available"    
            outcome.clean <- outcome.state[bad,]
            colnames(outcome.clean) <- c("Hospital.names", "Mortality.rate")
            outcome.clean <- transform(outcome.clean, Mortality.rate = as.numeric(Mortality.rate))
      
            ## ordering by rate and alphabetical order 
            outcome.ordered <- outcome.clean[order(outcome.clean$Mortality.rate, outcome.clean$Hospital.names, decreasing = FALSE),]  
            n.obs <- dim(outcome.ordered)[1]
            
            ## ranking
            if (num == "best") {
                  num <- 1
                  outcome.rank <- outcome.ordered[num,1]
            }else if (num == "worst") {
                  num <- n.obs
                  outcome.rank <- outcome.ordered[num,1]
            }else if (num > n.obs) {
                  outcome.rank <- NA
            }else{outcome.rank <- outcome.ordered[num,1]}
            
            ## Return a data frame with the hospital names and the
            ## (abbreviated) state name
            result[x,] <- c(outcome.rank, state) 
      }
      
      result
}

setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass3/Coursera_Rprog_ass3")
source("rankall.R")

## Test 
head(rankall("heart attack", 20), 10)

tail(rankall("pneumonia", "worst"), 3)

tail(rankall("heart failure"), 10)
