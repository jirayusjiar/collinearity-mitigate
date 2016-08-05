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

enableJIT(3)

## Define global variables and functions



## Main

for(targetProjectId in 1:nrow(listData)) {
  
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
  
  trainDataIndex <- sample(nrow(dataset),replace=TRUE)
  # Mitigate collinearity 
  glmModel <- glm(
    as.formula(paste(dep,"~",paste(indep, collapse = '+'))), 
    data=dataset[trainDataIndex,], 
    family=binomial()
  )
  drop1(glmModel, test = "Chisq")
  
}
