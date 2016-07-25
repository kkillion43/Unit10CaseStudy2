setwd("C:/Users/hb13316/Documents/Data Science/Intro to Data Science/Case Study 2")
getwd()

list.files()
rawAsIs <- read.csv("ImportedAsIsDataChulwalar.csv", sep = ";", 
                    header=FALSE, fill=TRUE)
rawPlanned <- read.csv("ImportedPlanDataChulwalar.csv", sep = ";", 
                       header=FALSE, fill=TRUE)
rawIndicator <- read.csv("ImportedIndicatorsChulwalar.csv", sep = ";", 
                         header=FALSE, fill=TRUE)


library(fpp)

cleanAsIs <- rawAsIs
cleanPlan <- rawPlanned
cleanIndicator <- rawIndicator

#Set Data to the Wuge Flower
wuge <- c(cleanAsIs [30:41,2],cleanAsIs [30:41,3],cleanAsIs [30:41,4],cleanAsIs [30:41,5],cleanAsIs [30:41,6],cleanAsIs [30:41,7])
wugeAsIs <- ts(wuge, start=c(2008,1), end=c(2013,12), frequency=12)


wuge2 <- c(cleanPlan [30:41,2],cleanPlan [30:41,3],cleanPlan [30:41,4],cleanPlan [30:41,5],cleanPlan [30:41,6],cleanPlan [30:41,7])
wugePlan <- ts(wuge2, start=c(2008,1), end=c(2013,12), frequency=12)

wugeIndicator <- ts(cleanIndicator[2:length(cleanIndicator),], start=c(2008,1), end=c(2013,12), frequency=12)
colnames(wugeIndicator) <- c('days','2008','2009','2010','2011','2012','2013','2014')
wugeIndicator <- wugeIndicator[, 1:7]