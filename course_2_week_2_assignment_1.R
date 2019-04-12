getwd()
add=function(x,y){
  x+y
}
setwd("C:/Users/hp/Documents/rprogramming_coursera")



setwd("specdata")
pollutantmean=function(directory, pollutant, id=1:332){
  #setwd(directory)
  combined=data.frame("Date"=as.Date(character()), "sulphate"=as.numeric(), 
                      "nitrate"=as.numeric(), "ID"=as.numeric())
  id=stringr::str_pad(id, 3, pad = 0)
  for(i in id){
    combined=rbind(combined, read.csv(paste(as.character(i), ".csv", sep = ""), header = TRUE))
  }
  temp=eval(parse(text = paste("combined$", pollutant, sep = "")))
  mean(temp, na.rm = TRUE)
}
pollutantmean("specdata", "nitrate")




complete=function(directory, id=1:332){
  result=data.frame("id"=numeric(), "nobs"=numeric())
  for(i in id){
    result=rbind(result, data.frame("id"=i, "nobs"=sum(complete.cases(read.csv(paste(getwd(),"/",directory,"/",stringr::str_pad(i, 3, pad = 0),".csv", sep = ""))))))
  }
  result
}
complete("specdata", 54)
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])



corr=function(directory, threshold=0){
  result=c()
  for(i in 1:332){
    temp=read.csv(paste(getwd(),"/",directory,"/",stringr::str_pad(i, 3, pad = 0),".csv", sep = ""))
    temp=temp[complete.cases(temp)==TRUE,]
    if(sum(complete.cases(temp))>threshold){
      result=c(result, cor(temp$sulfate, temp$nitrate))
    }
  }
  result
}
corr("specdata", threshold = 400)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

