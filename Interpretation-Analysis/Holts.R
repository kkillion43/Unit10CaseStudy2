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