##This function outputs the best hospital in a state

library(dplyr)

best <- function(state, outcome) {
  ## Read outcome data
  db_big <- read.csv("data/outcome-of-care-measures.csv", 
                      colClasses = "character")
  db <- select(db_big, 1, 2, 7, 11, 17, 23)
  
  ## Check that state and outcome are valid
  check <- state == db$State
  if(sum(check > 0)){
    }
  else {stop(print("invalid state"))}
  
  conditions <- c("MI", "HF", "pneumonia")
  check2 <- outcome == conditions
  
  if(sum(check2) > 0){ 
    }
  else {stop(print("invalid condition"))}
  
  ## Return hospital name in that state with lowest 30-day death
  
  stateHospitals <- filter(db, db$State == state)
  
  if(outcome == "MI") {
    stateHospitalsArranged <- arrange(stateHospitals, 
                                      stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  }
  
  else if(outcome == "HF") {
    stateHospitalsArranged <- stateHospitals %>%
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  }
  
  else if(outcome == "pneumonia") {
    stateHospitalsArranged <- stateHospitals %>%
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  }
  
  else {stop(print("invalid condition"))}
  
  ## rate
  return(head(stateHospitalsArranged[2], 1))
}