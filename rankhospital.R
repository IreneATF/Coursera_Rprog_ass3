## 3. RANKING HOSPITALS BY OUTCOME IN A STATE

rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      
      setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass3/rprog_data_ProgAssignment3-data")
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

      by.state <- split(data,data$State)   ## Splitting data by state
      
      ## Check that state and outcome are valid
      
      valid.state <- state == names(by.state)
      
      if(!any(valid.state == TRUE)) {
            stop("invalid state")
      }else if(outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
            stop("invalid outcome")
      }
      
      ## Return hospital name in that state with the given rank
      
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
      if (num == "best") {num <- 1
      }else if (num == "worst") {num <- n.obs
      }else if (num > n.obs) {return(NA)}
      
      outcome.rank <- outcome.ordered[num,1]
      outcome.rank 
      
      ## 30-day death rate
}

setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass3/Coursera_Rprog_ass3")
source("rankhospital.R")

## Test 
rankhospital("TX", "heart failure", 4)

rankhospital("MD", "heart attack", "worst")

rankhospital("MN", "heart attack", 5000)

