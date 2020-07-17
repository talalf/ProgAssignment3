library(dplyr)
rankall <- function(outcome, num) {
  ## Read outcome data
  db_big <- read.csv("data/outcome-of-care-measures.csv", 
                     colClasses = "character")
  db_small <- select(db_big, 2, 7, 11, 17, 23)
  
  db <- db_small %>% 
    rename("MI" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
            "HF" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
            "PN" = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  
  db$MI <- as.numeric(db$MI)
  db$HF <- as.numeric(db$HF)
  db$PN <- as.numeric(db$PN)
  
  ## Check that state and outcome are valid
  
  conditions <- c("MI", "HF", "PN")
  check2 <- outcome == conditions
  if(sum(check2) > 0) {print("valid condition")}
  else {stop(print("invalid condition"))}
  
  ## For each state, find the hospital of the given rank
  
  statelist <- unique(db$State) #makes a list of unique states
  
  hospList <- NULL
  
  for(i in statelist){
    x <- filter(db, db$State == i) #filters by state
    
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
    
    hospital <- y[num, 1:2]
    hospList <- rbind(hospList, hospital)
  }
  
  ## Return a data frame with the hospital names and the state
  hospList
  }