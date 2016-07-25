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