library(dplyr)

rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  db_big <- read.csv("data/outcome-of-care-measures.csv", 
                     colClasses = "character")
  db_small <- select(db_big, 2, 7, 11, 17, 23)
  
  db <- db_small %>% rename("MI" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
               "HF" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
               "PN" = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  
  db$MI <- as.numeric(db$MI)
  db$HF <- as.numeric(db$HF)
  db$PN <- as.numeric(db$PN)
  
  #test code
  #x <- filter(db, State == "TX")
  #x <- x %>% arrange(HF, Hospital.Name)
  
  ## Check that state and outcome are valid
  check <- state == db$State
  if(sum(check > 0)) {print("valid state")}
    else {stop(print("invalid state"))}
  
  conditions <- c("MI", "HF", "PN")
  check2 <- outcome == conditions
  if(sum(check2) > 0) {print("valid condition")}
    else {stop(print("invalid condition"))}
  
  ## Return hospital name in that state with the given rank
  
  x <- filter(db, State == state)
  
  if(outcome == "MI"){
    x <- arrange(x, x$MI, x$Hospital.Name)
    y <- x[complete.cases(x[,3]),]
    }
  else if(outcome == "HF"){
    x <- arrange(x, x$HF, x$Hospital.Name)
    y <- x[complete.cases(x[,4]),]
    }
  else if(outcome == "PN"){
    x <- arrange(x, x$PN, x$Hospital.Name)
    y <- x[complete.cases(x[,5]),]
  }
  
  else{stop(print("invalid condition"))}
  
  paste(y[num,1], "Mortality rate for", outcome, ":", x[num,outcome], "Rank:", num, sep = " ")
  
}