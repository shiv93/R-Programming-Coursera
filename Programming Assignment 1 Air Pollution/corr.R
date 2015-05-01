#function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. 
#The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0



source("complete.R")  #needed when we are calling another function from current function
corr<- function(directory,threshold=0){
  
  corr<-numeric()
  count<-1
  temp<-complete(directory)
  temp<-temp$nobs
  #dir<-paste("specdata",sep="")
  all_files<-list.files()
  len<-length(all_files)
  #no.of_files<-length(all_files)
  for(i in 1:332){
    if(temp[i]>threshold){
      if(i<=9)
        loc<-paste("00",i,sep="")
      else if(i<=99)
        loc<-paste("0",i,sep="")
      else loc<-i
      dir<-paste("specdata/",loc,".csv",sep="")
      file<-read.csv(dir)
      cr<-cor(file$sulfate,file$nitrate,use="complete.obs")
      cr <- round(cr, digits=4)
      corr<-c(corr,cr)               # repeatedly add values in a vector
    }
    
    #temp<-complete.cases(file)
    #sum<-length(temp)
    #if(sum>threshold){
     # count<-count+1
      #file<-file[temp,]
      
      
    }
  corr
}
  
  
  
  
  
  
  
