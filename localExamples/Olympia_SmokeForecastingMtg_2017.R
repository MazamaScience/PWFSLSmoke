# 

library(PWFSLSmoke)

airsis <- get(load('~/Data/monitoring/RData/airsis_pm25_2017.RData'))
airnow <- get(load('~/Data/monitoring/RData/airnow_pm25_2017.RData'))
wrcc <- get(load('~/Data/monitoring/RData/wrcc_pm25_2017.RData'))

monitors <- monitor_combine(list(airsis, airnow, wrcc))
monitors <- monitor_subset(monitors, tlim=c(20170801,20171001))

nw <- monitor_subset(monitors, stateCodes = c('WA','OR','ID'))
ca <- monitor_subset(monitors, stateCodes = c('CA'))

# # Timeseries 'gnats' plot (takes forever to plot!)
# layout(matrix(seq(2)))
# monitorPlot_timeseries(nw, style='gnats', localtime=FALSE, ylim=c(0,400), xpd=NA)
# addAQILines()
# title('All Monitors in WA-OR-ID-MT')
# monitorPlot_timeseries(ca, style='gnats', localtime=FALSE, ylim=c(0,400), xpd=NA)
# addAQILines()
# title('All Monitors in CA')
# layout(1)

nw_daily_mean <- monitor_dailyStatistic(nw, minHours=18)
ca_daily_mean <- monitor_dailyStatistic(ca, minHours=18)

# PLOT -- All measurements in Aug-Sep -----
png('all_daily_means.png', width=1000, height=750)
#layout(matrix(seq(2)))
par(mar=c(5,4,4,6)+.1, cex=1.5)
monitorPlot_timeseries(nw_daily_mean, style='gnats', xlab='', ylab='', localTime=FALSE, ylim=c(0,400), xpd=NA)
addAQILines(lwd=2)
text(par('usr')[2], AQI$breaks_24[2:6], AQI$names[2:6], pos=4, xpd=NA)
title(paste0('Daily Mean PM2.5 for ', nrow(nw$meta), ' Monitors in WA-OR-ID'))
# monitorPlot_timeseries(ca_daily_mean, style='gnats', xlab='', ylab='', localtime=FALSE, ylim=c(0,400), xpd=NA)
# addAQILines(lwd=2)
# text(par('usr')[2], AQI$breaks_24[2:6], AQI$names[2:6], pos=4, xpd=NA)
# title(paste0('Daily Mean PM2.5 for ', nrow(nw$meta), ' Monitors in CA'))
par(mar=c(5,4,4,2)+.1, cex=1)
layout(1)
dev.off()
# -----------------------------------------------------------------------------

# PLOT -- Map of Maxima in Aug-Sep -----
png('nw_maxima_map.png', width=1000, height=750)
par(cex=1.5)
monitorMap(nw_daily_mean, max, cex=1.5)
addAQILegend("topright", pt.cex=1.5)
title('Maximum of Daily Average at Each Site')
par(cex=1)
dev.off()
# -----------------------------------------------------------------------------

Lewiston <- monitor_subset(nw, monitorIDs='160690012')
Clarkston <- monitor_subset(nw, monitorIDs='530030004')

Juliaetta <- monitor_subset(nw, monitorIDs='160571012')
Orofino <- monitor_subset(nw, monitorIDs='NPT1000__lon_.116.235_lat_46.465')
Lapwai <- monitor_subset(nw, monitorIDs='160690013')
Reubens <- monitor_subset(nw, monitorIDs='160690014')
Nezperce <- monitor_subset(nw, monitorIDs='Idaho1002__lon_.116.248_lat_46.234')
Kamiah <- monitor_subset(nw, monitorIDs='160490003')
Cottonwood <- monitor_subset(nw, monitorIDs='160491012')
Grangeville <- monitor_subset(nw, monitorIDs='160490002')

Nezperce_area <- monitor_combine(list(Clarkston,
                                      Lewiston,
                                      Juliaetta,
                                      Orofino,
                                      Lapwai,
                                      Reubens,
                                      Nezperce,
                                      Kamiah,
                                      Cottonwood,
                                      Grangeville))

onRez <- monitor_combine(list(Orofino,
                              Lapwai,
                              Reubens,
                              Nezperce,
                              Kamiah))

offRez <- monitor_combine(list(Clarkston,
                               Lewiston,
                               Juliaetta,
                               Cottonwood,
                               Grangeville))

oldPar <- par()
# PLOT -- Google Map of Nezperce area -----
png('nezperce_zoom7.png', width=1000, height=750)
par(cex=1.5)
monitorGoogleMap(Nezperce_area, zoom=7)
addAQILegend(pt.cex=2)
par(cex=1)
dev.off()
# -----------------------------------------------------------------------------

# PLOT -- Google Map of Nezperce area -----
png('nezperce_zoom9.png', width=1000, height=750)
par(cex=1.5)
monitorGoogleMap(Nezperce_area, zoom=9, cex=3)
addAQILegend(pt.cex=2)
par(cex=1)
dev.off()
# -----------------------------------------------------------------------------
par(oldPar)

Nezperce_single <- monitor_collapse(Nezperce_area, monitorID='Nezperce_single')
unhealthyHours <- monitor_dailyThreshold(Nezperce_single, threshold="unhealthy")

# PLOT -- Hours per day Unhealty -----
png('hours_per_day.png', width=1000, height=750)
par(cex=1.5)
unhealthyHours$data[unhealthyHours$data == 0] <- 0.1 # so we don't get blank bars
monitorPlot_timeseries(unhealthyHours, type='h', lend='butt', lwd=6,
                       col='black',
                       ylab="Hours per day")
usr <- par('usr')
rect(usr[1],18,usr[2],24, col=adjustcolor('red',.2), border=NA)
monitorPlot_timeseries(unhealthyHours, type='h', lend='butt', lwd=6, ylab="Hours per day", add=TRUE)
title("Nezperce Area -- Hours per day Above 'Unhealthy'")
text(usr[1], 21, "18-24 Hours per day >= 'Unhealthy'", pos=4, font=2, col='red')
par(cex=1)
dev.off()
# -----------------------------------------------------------------------------


# PLOT -- dailyBarplot for onRez

png('daily_barplot_onrez.png', width=1000, height=750)
layout(matrix(seq(5)))
par(cex=1.0)
par(mar=c(3,4,1,2)+.1)
monitorPlot_dailyBarplot(Orofino, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
title('Orofino', line=-1)
monitorPlot_dailyBarplot(Lapwai, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
title('Lapwai', line=-1)
monitorPlot_dailyBarplot(Reubens, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
title('Reubens', line=-1)
monitorPlot_dailyBarplot(Nezperce, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
title('Nezperce', line=-1)
monitorPlot_dailyBarplot(Kamiah, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
title('Kamiah', line=-1)
par(mar=c(5,4,4,2)+.1)
par(cex=1)
layout(1)
dev.off()
# -----------------------------------------------------------------------------


# PLOT -- dailyBarplot for offRez
png('daily_barplot_offrez.png', width=1000, height=750)
layout(matrix(seq(5)))
par(cex=1.0)
par(mar=c(3,4,1,2)+.1)
monitorPlot_dailyBarplot(Clarkston, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
title('Clarkston', line=-1)
monitorPlot_dailyBarplot(Lewiston, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
title('Lewiston', line=-1)
monitorPlot_dailyBarplot(Juliaetta, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
title('Juliaetta', line=-1)
monitorPlot_dailyBarplot(Cottonwood, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
title('Cottonwood', line=-1)
monitorPlot_dailyBarplot(Grangeville, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
title('Grangeville', line=-1)
layout(1)
par(mar=c(5,4,4,2)+.1)
par(mar=c(5,4,4,2)+.1)
par(cex=1)
layout(1)
dev.off()
# -----------------------------------------------------------------------------


nowcast_Cottonwood <- monitor_nowcast(Cottonwood)

bad_Cottonwood <- monitor_subset(Cottonwood, tlim=c(20170901, 20170914))
bad_nowcast_Cottonwood <- monitor_subset(nowcast_Cottonwood, tlim=c(20170901, 20170914))

# PLOT -- hourly/nowcast comparison
png('cottonwood_nowcast_comparison.png', width=1000, height=750)
par(cex=1.5)
monitorPlot_timeseries(bad_Cottonwood, type='p', pch=16, col='red', shadedNight=TRUE)
monitorPlot_timeseries(bad_nowcast_Cottonwood, type='l', col='black', lwd=2, add=TRUE)
legend("topright", legend=c('Hourly','Nowcast'), col=c('red','black'), lwd=c(NA,2), pch=c(16,NA))
title('Cottonwood Hourly and Nowcast')
par(cex=1)
dev.off()
# -----------------------------------------------------------------------------

bad_Cottonwood <- monitor_subset(Cottonwood, tlim=c(20170904, 20170910))
bad_nowcast_Cottonwood <- monitor_subset(nowcast_Cottonwood, tlim=c(20170904, 20170910))

# PLOT -- hourlyBarplot for Cottonwood
png('cottonwood_nowcast.png', width=1000, height=750)
par(cex=1.5)
monitorPlot_hourlyBarplot(bad_Cottonwood, dayCol='transparent', hourLwd=0,
                          ylab='', main='Cottonwood Hourly Nowcast',
                          labels_x_nudge=3, border=adjustcolor('white',0.2))
par(cex=1)
dev.off()
# -----------------------------------------------------------------------------





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