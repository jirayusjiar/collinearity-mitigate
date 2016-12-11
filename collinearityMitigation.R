### Mitigate collinearity in defect dataset

## Set working directory
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

## Import packages 
# These two lines are used to download DefectData library
#library(devtools)
#install_github("klainfo/DefectData")
library(DefectData)
library(compiler)
library(Hmisc)

enableJIT(3)

## Define functions
.writeLine <- function(arg1,arg2){
  write(paste0(arg2,collapse=","),file=arg1,append=TRUE)  
} writeLine = cmpfun(.writeLine)

## Main
for(targetProjectId in 89:89) {
#for(targetProjectId in 1:nrow(listData)) {
  
  print(paste('ProjectID:', targetProjectId,"START"))
  datasetInfo <<- c(targetProjectId)  
  
  targetData <- as.character(listData[targetProjectId, 1])
  datasetInfo <<- c(datasetInfo,targetData)
  
  # Load data
  Data <- loadData(targetData)
  
  # retrieve data, name of dependent and independent variables
  dataset <- Data$data
  dep <- Data$dep
  indep <- Data$indep
  # Finish import dataset
  
  # Mitigate collinearity 
  rd<-redun(
    as.formula(
      paste(
        "~", 
        paste(indep, collapse = '+')
        )
      ), 
    data = dataset,
    r2 = 0.9,
    type = 'adjusted',
    nk = 0,
    pr = TRUE
  )
  
  # rd$In Leftover variables
  # rd$Out Deleted variables
  # Repeat redun until n(rd(n-1)$In) == n(rd(n)$In)
  nRdIn <- length(rd$In)
  while(TRUE){
    rd <- redun(
      as.formula(
        paste0(
          '~',
          paste0(rd$In,collapse='+')
        )
      )
      , 
      data = dataset,
      r2 = 0.9,
      type = 'adjusted',
      nk = 0,
      pr = TRUE
    )
    if(nRdIn==length(rd$In)){
      break
    } else {
      nRdIn=length(rd$In)
    }
  }
  cat(
    targetData,         #Dataset name
    length(indep),      #n(Indep)
    nRdIn,              #n(Non-redundant metrics)
    nRdIn/length(indep),#n(Indep)/n(Non-redundant metrics)
    rd$In)
}
