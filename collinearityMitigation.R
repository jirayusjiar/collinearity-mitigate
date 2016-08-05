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

## Define global variables and functions



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
  redun(
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
  # Variables after #1 redun
  # pre ACD FOUT_avg FOUT_max FOUT_sum MLOC_sum NBD_avg NBD_max NOF_avg NOF_sum NOM_avg NOT NSF_sum NSM_avg PAR_avg PAR_max VG_avg VG_max 
  redun(
      ~pre+ACD+FOUT_avg+FOUT_max+FOUT_sum+MLOC_sum+NBD_avg+NBD_max+NOF_avg+NOF_sum+NOM_avg+NOT+NSF_sum+NSM_avg+PAR_avg+PAR_max+VG_avg+VG_max
    , 
    data = dataset,
    r2 = 0.9,
    type = 'adjusted',
    nk = 0,
    pr = TRUE
  )
  # Variables after #2 redun (NOF_sum is removed)
  # pre ACD FOUT_avg FOUT_max FOUT_sum MLOC_sum NBD_avg NBD_max NOF_avg NOM_avg NOT NSF_sum NSM_avg PAR_avg PAR_max VG_avg VG_max 
  redun(
    ~pre+ACD+FOUT_avg+FOUT_max+FOUT_sum+MLOC_sum+NBD_avg+NBD_max+NOF_avg+NOM_avg+NOT+NSF_sum+NSM_avg+PAR_avg+PAR_max+VG_avg+VG_max
    , 
    data = dataset,
    r2 = 0.9,
    type = 'adjusted',
    nk = 0,
    pr = TRUE
  )
  
}
