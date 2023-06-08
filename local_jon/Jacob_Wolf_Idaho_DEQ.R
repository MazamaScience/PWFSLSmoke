# Jacob_Wolf_Idaho_DEQ.R
#
# Why do the Mv4 daily plot and Data Report disagree about the daily average
# color in Salmon, Idaho on July 19'th?

library(PWFSLSmoke)

Salmon <-
  airnow_loadLatest() %>%
  monitor_subset(monitorIDs = "160590004_01")

monitor_dailyBarplot(Salmon) # July 19 is RED

AirMonitorPlots::monitor_ggDailyHourlyBarplot(Salmon) # July 19 is RED

Salmon %>%
  monitor_dailyStatistic(mean) %>%
  monitor_extractData()

#     datetime 160590004_01
# 1 2022-07-12     4.125000
# 2 2022-07-13     4.695652
# 3 2022-07-14     6.541667
# 4 2022-07-15     7.956522
# 5 2022-07-16     6.250000
# 6 2022-07-17     5.458333
# 7 2022-07-18     7.708333
# 8 2022-07-19    55.833333
# 9 2022-07-20    44.625000

# > PWFSLSmoke::AQI$breaks_24
# [1]  -Inf  12.0  35.5  55.5 150.5 250.5   Inf
# > PWFSLSmoke::AQI$colors
# [1] "#00E400" "#FFFF00" "#FF7E00" "#FF0000" "#8F3F97" "#7E0023"
#
# Should be RED beween 55.5 and 150.5

