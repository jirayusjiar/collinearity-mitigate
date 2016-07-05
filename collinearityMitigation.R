### Mitigate collinearity in defect dataset
### PCA approach
### Ref: http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/

## Set working directory
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

## Import packages 
# These two lines are used to download DefectData library
#library(devtools)
#install_github("klainfo/DefectData")
library(DefectData)
library(compiler)
library(stats)

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
  datasetLog <- dataset
  datasetLog[indep] <- log1p(datasetLog[indep])
  # Finish import dataset
  
  ## Mitigate collinearity - PCA
  # Calculate PCA
  dataset.pca <- prcomp(dataset[indep],
                        center=TRUE,
                        scale=TRUE)
  # Intepretation
  # http://webspace.ship.edu/pgmarr/Geo441/Lectures/Lec%2017%20-%20Principal%20Component%20Analysis.pdf
  print(dataset.pca)
  plot(dataset.pca,type="l")
  summary(dataset.pca)
  dataset.pca$rotation # Rotated component matrix
  corrplot(cor(dataset[indep],dataset.pca$x),method="number") #corrGraph between original and PCA
}
