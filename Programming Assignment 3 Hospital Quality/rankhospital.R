library(dplyr)
rankhospital <- function(state, outcome, num = "best") {
  
  file<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  if(is.na(match(state,file$State)))
    stop("invalid state")
  if(is.na(match(outcome,c("heart attack","heart failure","pneumonia"))))
    stop("invalid outcome")
  if(outcome=="heart attack"){
    outcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from.","Heart.Attack",sep="")
    file<-file[file$State==state,c("Hospital.Name",outcome)]
    file<-file[!is.na(file[[outcome]]),]
    file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-suppressWarnings(as.numeric(file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    file<-arrange(file,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name)
    file<-file[!is.na(file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
  }
  else if(outcome=="heart failure"){
    outcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from.","Heart.Failure",sep="")
    file<-file[file$State==state,c("Hospital.Name",outcome)]
    file<-file[!is.na(file[[outcome]]),]
    file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-suppressWarnings(as.numeric(file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    file<-arrange(file,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name)
    file<-file[!is.na(file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
  }
    
  else{
      outcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from.","Pneumonia",sep="")
      file<-file[file$State==state,c("Hospital.Name",outcome)]
      file<-file[!is.na(file[[outcome]]),]
      file$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-suppressWarnings(as.numeric(file$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      file<-arrange(file,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)
      file<-file[!is.na(file$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
  }
  if(num=="best")
    file[1,1]
  else if(num=="worst")
    file[nrow(file),1]
  else{
    file[num,1]
  }
  
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}