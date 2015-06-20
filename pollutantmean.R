pollutantmean <- function(directory,pollutant, id= 1:332){

  mainvector <- numeric(0)
  
  for(i in id){
    ithFrame <- frameFromCsv(directory,i)
    ithvector <- pollutantVectorFromFrame(ithFrame, pollutant)
    mainvector <- c(mainvector,ithvector)
  }
  
    mean(mainvector)
}

frameFromCsv <- function(directory, idnum){
  
  filename <- formatC(idnum, width=3, flag="0")
  fileString <- paste0(directory,"/",filename,".csv")
  openfile<-file(fileString)
  read.csv(openfile)

}

pollutantVectorFromFrame <- function(frame, pollutant){
  
  withNaVector <- frame[pollutant]
  isNaVector <-is.na(withNaVector)
  withNaVector[!isNaVector]
  
}
