rankall <- function(outcome, num = "best") {
  
  file<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  if(is.na(match(outcome,c("heart attack","heart failure","pneumonia"))))
    stop("invalid outcome")
  if(outcome=="heart attack"){
    outcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from.","Heart.Attack",sep="")
    file<-file[,c("Hospital.Name","State",outcome)]
    file<-file[!is.na(file[[outcome]]),]
    file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-suppressWarnings(as.numeric(file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    file<-group_by(file,State)
    file<-file[!is.na(file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    file<-arrange(file,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name)
    y<-tapply(file$Hospital.Name,as.factor(file$State),fortapply,num)
    newdf(y)
    
  }
  else if(outcome=="heart failure"){
    outcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from.","Heart.Failure",sep="")
    file<-file[,c("Hospital.Name","State",outcome)]
    file<-file[!is.na(file[[outcome]]),]
    file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-suppressWarnings(as.numeric(file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    file<-group_by(file,State)
    file<-file[!is.na(file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    file<-arrange(file,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name)
    y<-tapply(file$Hospital.Name,as.factor(file$State),fortapply,num)
    newdf(y)
  }
  else{
    outcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from.","Pneumonia",sep="")
    file<-file[,c("Hospital.Name","State",outcome)]
    file<-file[!is.na(file[[outcome]]),]
    file<-group_by(file,State)
    file$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-suppressWarnings(as.numeric(file$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    file<-file[!is.na(file$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    file<-arrange(file,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)
    y<-tapply(file$Hospital.Name,as.factor(file$State),fortapply,num)
    newdf(y)
  }
}
  
  fortapply<-function(x,num="best"){
    if(num=="best")
      num=1
    else if(num=="worst")
      num=length(x)
    x[num]
  }
  newdf<-function(tapplyval){
    name<-names(tapplyval)
    values<-as.character(tapplyval)
    
    data.frame("hospital"=values,"state"=name)
    
    
    
  }
  
  
  
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

