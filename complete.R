complete <- function(directory, id = 1:332){
    mainFrame <-data.frame("id"=numeric(0),"nobs"=numeric(0))
    
    for(i in id){
      ithFrame <- frameFromCsv(directory,i)
      sulfateNa <- pollutantNotNaVector(ithFrame, "sulfate")
      nitrateNa <- pollutantNotNaVector(ithFrame, "nitrate")
      bothNotNa <- sulfateNa & nitrateNa
      numComplete <- length(bothNotNa[bothNotNa])
      newrow <- data.frame("id"=i,"nobs"=numComplete)
      mainFrame <- rbind(mainFrame, newrow)
    }
    
    mainFrame
}

frameFromCsv <- function(directory, idnum){
  
  filename <- formatC(idnum, width=3, flag="0")
  fileString <- paste0(directory,"/",filename,".csv")
  openfile<-file(fileString)
  read.csv(openfile)
  
}

pollutantNotNaVector <- function(frame, pollutant){
  
  withNaVector <- frame[pollutant]
  !is.na(withNaVector)
  
}