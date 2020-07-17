##This function outputs the best hospital in a state

library(dplyr)

best <- function(state, outcome) {
  ## Read outcome data
  db <- read.csv("data/outcome-of-care-measures.csv", 
                      colClasses = "character")
  
  
  ## Check that state and outcome are valid
  check <- state == db$State
  if(sum(check > 0)){
    print("state exists")
  }
  else {stop(print("invalid state"))}
  
  conditions <- c("heart attack", "heart failure", "pneumonia")
  check2 <- outcome == conditions
  
  if(sum(check2) > 0){ 
    print("condition exists")
  }
  else {stop(print("invalid condition"))}
  
  ## Return hospital name in that state with lowest 30-day death
  
  stateHospitals <- filter(db, db$State == state)
  return(summary(as.factor(stateHospitals$State)))
  ## rate
}