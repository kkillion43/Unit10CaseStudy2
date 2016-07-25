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