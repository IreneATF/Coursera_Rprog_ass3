## 2. FINDING BEST HOSPITAL IN A STATE

best <- function(state, outcome) {
      ## Read outcome data
      
      setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass3/rprog_data_ProgAssignment3-data")
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

      by.state <- split(data,data$State)   ## Splitting data by state
      
      ## Check that state and outcome are valid
      
      valid.state <- state == names(by.state)
      
      if(!any(valid.state == TRUE)) {
            stop("in best('",state,"', '",outcome,"') : invalid state")
      }else if(outcome != "heart attack" || "heart failure" || "pneumonia") {
            stop("in best('",state,"', '",outcome,"') : invalid outcome")
      }
      
      ## Return hospital name in that state with lowest 30-day death
      
      state.data <- data.frame(by.state[state])
      
      if(outcome == "heart attack") {
            outcome.state <- state.data[,c(2,11)]
      }else if(outcome == "heart failure") {
            outcome.state <- state.data[,c(2,17)]
      }else if(outcome == "pneumonia") {
            outcome.state <- state.data[,c(2,23)]
      }
      
      bad <- outcome.state[,2] != "Not Available"
      outcome.state.clean <- outcome.state[c(outcome.state[!bad]),]
      outcome.ordered <- outcome.state.clean[order(outcome.state.clean$TX.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,decreasing = FALSE),]
      outcome.best <- outcome.ordered[,1]
      outcome.best 
      
      ## rate
}

setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass3/Coursera_Rprog_ass3")
source("best.R")

