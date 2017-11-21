# 

library(PWFSLSmoke)

airsis <- get(load('~/Data/monitoring/RData/airsis_pm25_2017.RData'))
airnow <- get(load('~/Data/monitoring/RData/airnow_pm25_2017.RData'))
wrcc <- get(load('~/Data/monitoring/RData/wrcc_pm25_2017.RData'))

monitors <- monitor_combine(list(airsis, airnow, wrcc))
monitors <- monitor_subset(monitors, tlim=c(20170501,20171101))

nw <- monitor_subset(monitors, stateCodes = c('WA','OR','ID','MT'))
ca <- monitor_subset(monitors, stateCodes = c('CA'))

# Timeseries 'gnats' plot (takes forever to plot!)
layout(matrix(seq(2)))
monitorPlot_timeseries(nw, style='gnats', localtime=FALSE, ylim=c(0,400), xpd=NA)
addAQILines()
title('All Monitors in WA-OR-ID-MT')
monitorPlot_timeseries(ca, style='gnats', localtime=FALSE, ylim=c(0,400), xpd=NA)
addAQILines()
title('All Monitors in CA')
layout(1)

nw_daily_mean <- monitor_dailyStatistic(nw, minHours=18)
ca_daily_mean <- monitor_dailyStatistic(ca, minHours=18)

layout(matrix(seq(2)))
monitorPlot_timeseries(nw_daily_mean, style='gnats', localTime=FALSE, ylim=c(0,500), xpd=NA)
addAQILines()
title('All Monitors in WA-OR-ID-MT')
monitorPlot_timeseries(ca_daily_mean, style='gnats', localtime=FALSE, ylim=c(0,500), xpd=NA)
addAQILines()
title('All Monitors in CA')
layout(1)



###############################################################################
###############################################################################
###############################################################################

# One time only, read in all data and write back out as a single object
if ( FALSE ) {
  
  # ----- AirNow -----
  
  airnow_05 <- get(load('~/Data/monitoring/RData/AIRNOW_PM25_201705-201706.RData'))
  airnow_06 <- get(load('~/Data/monitoring/RData/AIRNOW_PM25_201706.RData'))
  airnow_07 <- get(load('~/Data/monitoring/RData/AIRNOW_PM25_201707.RData'))
  airnow_08 <- get(load('~/Data/monitoring/RData/AIRNOW_PM25_201708.RData'))
  airnow_09 <- get(load('~/Data/monitoring/RData/AIRNOW_PM25_201709.RData'))
  
  # NOTE:  All ws_monitor objects have the same set of monitors. Test with:
  # NOTE:    setdiff(airnow_05$monitorID, airnow_09$monitorID)
  
  meta <- airnow_05$meta
  
  dataList <- list(airnow_05$data,
                   airnow_06$data,
                   airnow_07$data,
                   airnow_08$data,
                   airnow_09$data)
  
  joinedData <- suppressMessages(dplyr::bind_rows(dataList))
  joinedData <- joinedData[!duplicated(joinedData$datetime),]
  joinedData <- as.data.frame(joinedData)
  
  ws_monitor <- list(meta=meta, data=joinedData)
  airnow_2017 <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  save(airnow_2017, file='~/Data/monitoring/RData/airnow_pm25_2017.RData')
  
  # ----- AIRSIS -----
  
  apcd <- get(load('~/Data/monitoring/RData/AIRSIS_APCD_2017.RData'))
  arb2 <- get(load('~/Data/monitoring/RData/AIRSIS_ARB2_2017.RData'))
  mariposa <- get(load('~/Data/monitoring/RData/AIRSIS_MARIPOSA_2017.RData'))
  usfs <- get(load('~/Data/monitoring/RData/AIRSIS_USFS_2017.RData'))

  airsis_2017 <- monitor_combine(list(apcd,arb2,mariposa,usfs))
  
  save(airsis_2017, file='~/Data/monitoring/RData/airsis_pm25_2017.RData')
  
  # ----- WRCC -----
  
  cache <- get(load('~/Data/monitoring/RData/WRCC_Cache_2017.RData'))
  misc <- get(load('~/Data/monitoring/RData/WRCC_MISC_2017.RData'))
  usfs <- get(load('~/Data/monitoring/RData/WRCC_USFSRegions_2017.RData'))

  wrcc_2017 <- monitor_combine(list(cache,misc,usfs))
  
  save(wrcc_2017, file='~/Data/monitoring/RData/wrcc_pm25_2017.RData')
  
}