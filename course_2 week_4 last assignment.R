setwd("C:/Users/hp/Documents/rprogramming_coursera/rprog_data_ProgAssignment3-data")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])



best <- function(state, outcome) {
  temp <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(nrow(temp[temp$State==state,])==0){
    stop("invalid state")
  }
  if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("invalid outcome")
  }
  temp=temp[temp$State==state,]
  if(outcome=="heart attack"){
    temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    temp=temp[complete.cases(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]),]
    temp=temp[order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, temp$Hospital.Name),]
    return(temp$Hospital.Name[1])
  }
  else if(outcome=="heart failure"){
    temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    temp=temp[complete.cases(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]),]
    temp=temp[order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, temp$Hospital.Name),]
    return(temp$Hospital.Name[1])
  }
  else if(outcome=="pneumonia"){
    temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    temp=temp[complete.cases(temp[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]),]
    temp=temp[order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, temp$Hospital.Name),]
    return(temp$Hospital.Name[1])
  }

}







rankhospital <- function(state, outcome, num="best") {
  temp <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(nrow(temp[temp$State==state,])==0){
    stop("invalid state")
  }
  if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("invalid outcome")
  }
  temp=temp[temp$State==state,]
  if(outcome=="heart attack"){
    temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    temp=temp[complete.cases(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]),]
    temp=temp[order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, temp$Hospital.Name),]
  }
  else if(outcome=="heart failure"){
    temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    temp=temp[complete.cases(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]),]
    temp=temp[order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, temp$Hospital.Name),]
  }
  else if(outcome=="pneumonia"){
    temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    temp=temp[complete.cases(temp[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]),]
    temp=temp[order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, temp$Hospital.Name),]
  }
  if(num=="best")
    return(temp$Hospital.Name[1])
  if(num=="worst")
    return(temp$Hospital.Name[nrow(temp)])
  else
    return(temp$Hospital.Name[num])
  
}








rankall <- function(outcome, num="best") {
  temp <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  temp=temp[,c(2,7,11,17,23)]
  names(temp)=c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("invalid outcome")
  }
  state_list=split(temp, temp$state)
  myfun=function(x, arg1=num){
    x$`heart attack`=as.numeric(x$`heart attack`)
    x$`heart failure`=as.numeric(x$`heart failure`)
    x$pneumonia=as.numeric(x$pneumonia)
    x=x[order(x[ ,grepl(outcome, names(x)) ],x$hospital),c("hospital", "state", outcome)]
    x=x[complete.cases(x),]
    if(num=="best")
      x[1,c("hospital","state")]
    else if(num=="worst")
      x[nrow(x),c("hospital","state")]
    else
      x[num,c("hospital","state")]
  }
  result=lapply(state_list, myfun)
  do.call("rbind", result)
  
}

