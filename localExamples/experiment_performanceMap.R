airnow <- airnow_load(20150101, 20151231)
airnow_dailyAvg <- monitor_dailyStatistic(airnow, mean)
airnow_dailyAvg <- monitor_subset(airnow_dailyAvg, stateCodes='WA')
airnow_dailyRollingAvg <- monitor_rollingMean(airnow)
airnow_dailyRollingAvg <- monitor_subset(airnow_dailyRollingAvg, stateCodes='WA')

airnow_m1 <- airnow_load(20141231, 20151230)
airnow_dailyAvg_m1 <- monitor_dailyStatistic(airnow_m1, mean)
airnow_dailyAvg_m1 <- monitor_subset(airnow_dailyAvg_m1, stateCodes='WA')
airnow_dailyRollingAvg_m1 <- monitor_rollingMean(airnow_m1)
airnow_dailyRollingAvg_m1 <- monitor_subset(airnow_dailyRollingAvg_m1, stateCodes='WA')

airnow_m2 <- airnow_load(20141230, 20151229)
airnow_dailyAvg_m2 <- monitor_dailyStatistic(airnow_m2, mean)
airnow_dailyAvg_m2 <- monitor_subset(airnow_dailyAvg_m2, stateCodes='WA')
airnow_dailyRollingAvg_m2 <- monitor_rollingMean(airnow_m2)
airnow_dailyRollingAvg_m2 <- monitor_subset(airnow_dailyRollingAvg_m2, stateCodes='WA')

## NOTE: when i=5 there's error reading as below:
# Error in seq.int(0, 1, length.out = n) : 
#   'length.out' must be a non-negative number 
#   In addition: Warning message:
#   In max(indices, na.rm = TRUE) :
#   no non-missing arguments to max; returning -Inf

# map('state', 'washington')
# for loop
# numMonitors <- ncol(airnow_dailyAvg$data)-1
# monitorIDs <- names(airnow_dailyAvg$data)[-1]
# for(i in 1:numMonitors) {
#   monitor_performanceMap( monitor_subset(airnow_dailyAvg_m1, monitorIDs=monitorIDs[i]), 
#                           monitor_subset(airnow_dailyAvg, monitorIDs=monitorIDs[i]),
#                           threshold=AQI$breaks_24[3])
# }

monitor_performanceMap( airnow_dailyAvg_m2, airnow_dailyAvg)
