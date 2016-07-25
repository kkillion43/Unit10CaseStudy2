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


#############################
#   Decompose for Wuge      #
#############################


decIndicator <- decompose(wugeIndicator)
decPlan <- decompose(wugePlan)
decAsIs <- decompose(wugeAsIs)

plot(decPlan, main='Decomposition of additive ts - Wuge Planned')
plot(decAsIs, main='Decomposition of additive ts - Wuge As Is ')

#############################
#         STL for Wuge      #
#############################

wuge_stl <- stl(wugeAsIs , s.window=5)

#Plot the STL
plot(wuge_stl$time.series/1000,
     main='Wuge STL Analysis (based units per 1000)')


#Plot the Trend by itself 
plot(wuge_stl$time.series[,'trend']/1000)


#Loop through the STL for a AsIs resample of monthly TimeFrames
comp <- c('seasonal','trend','remainder')
for ( i in comp){
  monthplot(wuge_stl$time.series[,i],
            ylab = 'i',
            main=paste('AsIs',i))
}

wugePlan_stl <- stl(wugePlan , s.window=5)
#Plot the STL
plot(wugePlan_stl$time.series/1000,
     main='Wuge STL Analysis (based units per 1000)')

#Plot the Trend by itself 
plot(wugePlan_stl$time.series[,'trend']/1000)

#Loop through the STL for a Planned resample of monthly TimeFrames
comp <- c('seasonal','trend','remainder')
for ( i in comp){
  monthplot(wugePlan_stl$time.series[,i],
            ylab = 'i',
            main=paste('Planned',i))
}

#Correlation with AsIs and Plan
cor(wugeAsIs , wugePlan)




#############################
# Holts Winters for Wuge    #
#############################
#Shorten the Time Series in order to compare Accuracy between Acutal vs Forecast
WugeAsIs_2012 <- ts(wugeAsIs, start=c(2008,1), end=c(2012,12), frequency=12)
wugeAsIs_shift4 <- ts(wuge, start=c(2008,1), end=c(2013,8), frequency = 12)

#Fit for 4 months out, Holt Winters Addivitive vs Multiplicative
fit1 <- hw(wugeAsIs, h=4)
fit1$model
mean(fit1$residuals)
sd(fit1$residuals)

fit2 <- hw(wugeAsIs, h=4, seasonal = 'additive')
fit2$model

plot(fit2, col='blue')
lines(fit1$fitted, col='red')
lines(fit1$mean, col='red')

mean(fit2$residuals)
sd(fit2$residuals)

fit2 <- hw(wugePlan, h=4, seasonal = 'multiplicative')

plot(fit1, col='red')
lines(fit2$fitted, col='blue')
lines(fit2$mean, col='blue')

#AIC AICc BIC are all lower with the additive and not mulitplicative


#Check for the accuracy of the fit Holt Winters Additive vs Multiplicative
fit1 <- hw(wugeAsIs_shift4, hw=4, seasonal = 'additive')
fit2 <- hw(wugeAsIs_shift4, hw=4, seasonal = 'multiplicative')
accuracy(fit1, wugeAsIs)
accuracy(fit2, wugeAsIs)



#Compare the Holts vs the Exponential Smoothing Model
wugePlan_ses <- ses(wugePlan)
wugeAsIs_ses <- ses(wugeAsIs)


par(mfrow=c(2,1))
plot(wugePlan_ses$fitted - fit1$fitted, col='blue',
     main='Wuge Diff in Planned Exponential Smoothing vs Acutal Holts Fit')
lines(wugePlan_ses$mean - fit1$mean, col='red')
plot(wugePlan_ses$residuals, col='magenta', main='Residuals of Exponential Smoothing Planned vs Acutal')
lines(wugeAsIs_ses$residuals, col='green')

wugeAsIs_ses$model
wugePlan_ses$model


#############################
#    Linear Fit for Wuge    #
#############################
NationalHolidaysVector <- c(cleanIndicator[170:181,2],cleanIndicator[170:181,3],cleanIndicator[170:181,4],cleanIndicator[170:181,5],cleanIndicator[170:181,6],cleanIndicator[170:181,7])
NationalHolidays <- ts(NationalHolidaysVector, start=c(2008,1), end=c(2013,12), frequency=12)

trend <- wuge_stl$time.series[,'trend']
season <- wuge_stl$time.series[,'seasonal']
random <- wuge_stl$time.series[,'remainder']

trend2 <- wugePlan_stl$time.series[,'trend']
season2 <- wugePlan_stl$time.series[,'seasonal']
random2 <- wugePlan_stl$time.series[,'remainder']

#Look at some residuals EDA comparisons
plot(lm(wugeAsIs ~ wugePlan))
plot(lm(wugeAsIs ~ trend + season))
plot(lm(wugeAsIs ~ trend2 + season2))

#Linear Fit of Wuge - Actual vs Planned with National Holidays factored in
linFit_ActPlan1 <- tslm(wugeAsIs ~ wugePlan + season + trend + NationalHolidays)
linFit_ActPlan2 <- tslm(wugeAsIs ~ wugePlan + season2 + trend2 + NationalHolidays)
summary(linFit_ActPlan1)
summary(linFit_ActPlan2)
