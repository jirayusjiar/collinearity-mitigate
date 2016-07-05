### Mitigate collinearity in defect dataset
### Ridge Regression
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
  
  # Mitigate collinearity 
  
  
}


# Test Dummy

x = data.frame(x1=rnorm(20))
x$x2<- rnorm(20,mean=x$x1,sd=.1)
x$y<- rnorm(20,mean=3+x$x1+x$x2)
#x$y<- (x$y-min(x$y))/(max(x$y)-min(x$y))
plot(x)
# Varclus
vc <- varclus(~.,
              data=x,
              similarity = "spearman",
              trans="abs")
plot(vc)
# VIF
model <- glm(y~x1+x2,
             data = x,
             family = binomial())
model <- lm(y~x1+x2,
            data = x)
model.vif <- rms::vif(model)
# Likelyhood ratio chisquare
lrAnova <- Anova(model, test.statistic = "LR")
lrChiSquare <- lrAnova[, 1]
lrChiSquareRelative <- lrChiSquare / sum(lrChiSquare)

# F-test
lrAnova2 <- Anova(model, test.statistic = "F")
lrF <-
  lrAnova2$F
lrFRelative <- lrF / sum(lrF)

# Deviance
lrAnova3 <- anova(model)
lrDeviance <- lrAnova3$Deviance
lrDevianceRelative <- lrDeviance / sum(lrDeviance)


model2 <- glm(y~x2+x1,
              data = x,
              family = binomial())
model2 <- lm(y~x2+x1,
             data = x)
model2.vif <- rms::vif(model2)
# Likelyhood ratio chisquare
.lrAnova <- Anova(model2, test.statistic = "LR")
.lrChiSquare <- .lrAnova[, 1]
.lrChiSquareRelative <- .lrChiSquare / sum(.lrChiSquare)

# F-test
.lrAnova2 <- Anova(model2, test.statistic = "F")
.lrF <-
  .lrAnova2$F
.lrFRelative <- .lrF / sum(.lrF)

# Deviance
.lrAnova3 <- anova(model2)
.lrDeviance <- .lrAnova3$Deviance
.lrDevianceRelative <- .lrDeviance / sum(.lrDeviance)

summary(model)
summary(model2)
model.vif
model2.vif
lrAnova
.lrAnova
lrAnova2
.lrAnova2
lrAnova3
.lrAnova3

