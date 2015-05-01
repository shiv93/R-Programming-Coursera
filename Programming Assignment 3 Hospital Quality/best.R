library(dplyr)
best <- function(state, outcome) {
  file<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  if(is.na(match(outcome,c("heart attack","heart failure","pneumonia"))))
    stop("invalid outcome")
  if(is.na(match(state,file$State)))
    stop("invalid state")
  if(outcome=="heart attack"){
    outcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from.","Heart.Attack",sep="")
    file<-file[file$State==state,c("Hospital.Name",outcome)]
    file<-file[!is.na(file[[outcome]]),]
    file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-suppressWarnings(as.numeric(file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))#supresswarning function used to prevent warning from getting displayed
    
    file<-arrange(file,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name)
  }
  else if(outcome=="heart failure"){
    outcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from.","Heart.Failure",sep="")
    file<-file[file$State==state,c("Hospital.Name",outcome)]
    file<-file[!is.na(file[[outcome]]),]
    file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-suppressWarnings(as.numeric(file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    file<-arrange(file,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name)
  }
  else{
    outcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from.","Pneumonia",sep="")
    file<-file[file$State==state,c("Hospital.Name",outcome)]
    file<-file[!is.na(file[[outcome]]),]
    file$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-suppressWarnings(as.numeric(file$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    file<-arrange(file,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)
  }
  
  
  
  file[1,1]
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
