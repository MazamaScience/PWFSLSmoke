#

library(PWFSLSmoke)

airnow <- airnow_load(2017)
airsis <- airsis_load(2017)
wrcc <- wrcc_load(2017)

oridwa <- monitor_combine(list(airsis, airnow, wrcc)) %>%
  monitor_subset(stateCodes=c('OR','ID','WA'))

# # Timeseries 'gnats' plot (takes forever to plot!)
# monitor_timeseriesPlot(oridwa, style='gnats')
# monitor_timeseriesPlot(oridwa, style='gnats', ylim=c(0,500), xpd=NA)

jas <- monitor_subset(oridwa, tlim=c(20170701,20171001))

# Save default graphical parameters
oldPar <- par()


Potlatch <- monitor_subset(jas, monitorIDs='160571000_01')
Potlatch_200 <- monitor_subsetByDistance(jas, Potlatch$meta$longitude, Potlatch$meta$latitude, 100)





# # -----------------------------------------------------------------------------
# # All hourly data plot
#
# dataCount <- nrow(jas$data) * (ncol(jas$data) - 1)
# dataCountString <- prettyNum(dataCount, big.mark=",")
#
# png('oridwa_hourly.png', width=800, height=600)
#
# par(mar=c(5.1,4.1,9.1,8.1))
# monitor_timeseriesPlot(jas, style='gnats', xlab='2017', ylim=c(0,400), xpd=NA)
# addAQIStackedBar(pos='left', width=0.01, labels=FALSE, title=FALSE)
# addAQILines(lwd=2)
# text(par('usr')[2], AQI$breaks_24[2:6], AQI$names[2:6], pos=4, xpd=NA)
# mtext('Hourly PM2.5', side=3, line=6, adj=0, cex=2.0, font=2)
# mtext('Washginton-Oregon-Idaho', side=3, line=4, adj=0, cex=1.8, font=1)
# mtext(paste0(dataCountString,' measurements'), side=3, line=2, adj=0, cex=1.8, font=1)
#
# dev.off()
#
# par(oldPar)
#
# # -----------------------------------------------------------------------------
# # All daily data plot
#
# dailyMean <- monitor_dailyStatistic(jas)
#
# dataCount <- nrow(dailyMean$data) * (ncol(dailyMean$data) - 1)
# dataCountString <- prettyNum(dataCount, big.mark=",")
#
# png('oridwa_daily.png', width=800, height=600)
#
# par(mar=c(5.1,4.1,9.1,8.1))
# monitor_timeseriesPlot(dailyMean, style='gnats', xlab='2017', ylim=c(0,200), xpd=NA)
# addAQIStackedBar(pos='left', width=0.01, labels=FALSE, title=FALSE)
# addAQILines(lwd=2)
# text(par('usr')[2], AQI$breaks_24[2:5], AQI$names[2:5], pos=4, xpd=NA)
# mtext('Daily Mean PM2.5', side=3, line=6, adj=0, cex=2.0, font=2)
# mtext('Washginton-Oregon-Idaho', side=3, line=4, adj=0, cex=1.8, font=1)
# mtext(paste0(dataCountString,' daily means'), side=3, line=2, adj=0, cex=1.8, font=1)
#
# dev.off()
#
# par(oldPar)
#
#
# # -----------------------------------------------------------------------------
# # Daily max map
#
# png('daily_max_map.png', width=800, height=600)
#
# monitor_map(dailyMean, max, mar=c(1,1,9.1,1), cex=1.8)
# addAQILegend("topright", cex=1.2, pt.cex=1.5)
# mtext('Highest Daily Mean PM2.5 during Jul-Aug-Sep', side=3, line=2, adj=0, cex=2.0, font=1)
#
# dev.off()
#
# par(oldPar)
#
#
# # -----------------------------------------------------------------------------
# # Nez Perce monitors
#
# as <- monitor_subset(jas, tlim=c(20170801,20170915), tz="America/Los_Angeles")
# Lewiston <- monitor_subset(as, monitorIDs='160690012_01')
# Clarkston <- monitor_subset(as, monitorIDs='530030004_01')
#
# Juliaetta <- monitor_subset(as, monitorIDs='160571012_01')
# Orofino <- monitor_subset(as, monitorIDs='lon_.116.235_lat_46.465_apcd.1000')
# Lapwai <- monitor_subset(as, monitorIDs='160690013_01')
# Reubens <- monitor_subset(as, monitorIDs='160690014_01')
# Nezperce <- monitor_subset(as, monitorIDs='lon_.116.248_lat_46.234_apcd.1006')
# Kamiah <- monitor_subset(as, monitorIDs='160490003_01')
# Cottonwood <- monitor_subset(as, monitorIDs='160491012_01')
# Grangeville <- monitor_subset(as, monitorIDs='160490002_01')
#
# Nezperce_area <- monitor_combine(list(Clarkston,
#                                       Lewiston,
#                                       Juliaetta,
#                                       Orofino,
#                                       Lapwai,
#                                       Reubens,
#                                       Nezperce,
#                                       Kamiah,
#                                       Cottonwood,
#                                       Grangeville))
#
# onRez <- monitor_combine(list(Orofino,
#                               Lapwai,
#                               Reubens,
#                               Nezperce,
#                               Kamiah))
#
# offRez <- monitor_combine(list(Clarkston,
#                                Lewiston,
#                                Juliaetta,
#                                Cottonwood,
#                                Grangeville))
#
# onRezList <- list(Orofino,
#                   Lapwai,
#                   Reubens,
#                   Nezperce,
#                   Kamiah)
#
# onRezLabels <- c('Orofino','Lapwai','Reubens','Nezperce','Kamiah')
#
# offRezList <- list(Clarkston,
#                    Lewiston,
#                    Juliaetta,
#                    Cottonwood,
#                    Grangeville)
#
# offRezLabels <- c('Clarkston','Lewiston','Juliaetta','Cottonwood','Grangeville')
#
# # -----------------------------------------------------------------------------
# # Nez Perce zoom out map
#
# Nezperce_area_daily <- monitor_dailyStatistic(Nezperce_area)
#
# png('nezperce_map_zoom_out.png', width=800, height=600)
#
# monitorGoogleMap(Nezperce_area_daily, zoom=7, width=640, height=640, cex=3)
# addAQILegend(cex=1.5, pt.cex=2, title='Max Daily Mean')
#
# dev.off()
#
# par(oldPar)
#
#
#
# png('nezperce_map_zoom_in.png', width=800, height=600)
#
# monitorGoogleMap(Nezperce_area_daily, zoom=9, width=640, height=640, cex=5)
# addAQILegend(cex=1.5, pt.cex=2, title='Max Daily Mean')
#
# dev.off()
#
# par(oldPar)
#
#
# # -----------------------------------------------------------------------------
# # Nez Perce area unhealthy hours
#
# Nezperce_single <- monitor_collapse(Nezperce_area, monitorID='Nezperce_single')
# unhealthyHours <- monitor_dailyThreshold(Nezperce_single, threshold="unhealthy")
#
# png('nezperce_area_unhealthy.png', width=800, height=600)
#
# par(cex=1.5)
#
# unhealthyHours$data[unhealthyHours$data == 0] <- 0.1 # so we don't get blank bars
# monitor_timeseriesPlot(unhealthyHours, type='h', lend='butt', lwd=12,
#                        col='black',
#                        xlab='2017', ylab="Hours Above Unhealthy")
# usr <- par('usr')
# rect(usr[1],18,usr[2],24, col=adjustcolor('red',.2), border=NA)
# monitor_timeseriesPlot(unhealthyHours, type='h', lend='butt', lwd=12, add=TRUE)
# title("Nez Perce Area 10 Monitor Average")
# text(usr[1], 21, "18-24 Hours per day >= 'Unhealthy'", pos=4, font=2, col='red')
#
# par(cex=1)
#
# dev.off()
#
#
#
# # -----------------------------------------------------------------------------
# # PLOT -- dailyBarplot for onRez
#
# png('daily_barplot_onrez.png', width=800, height=600)
# layout(matrix(seq(5)))
# par(cex=1.0)
# par(mar=c(3,4,1,2)+.1)
# monitor_dailyBarplot(Orofino, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
# title('Orofino', line=-1)
# monitor_dailyBarplot(Lapwai, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
# title('Lapwai', line=-1)
# monitor_dailyBarplot(Reubens, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
# title('Reubens', line=-1)
# monitor_dailyBarplot(Nezperce, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
# title('Nezperce', line=-1)
# monitor_dailyBarplot(Kamiah, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
# title('Kamiah', line=-1)
# par(mar=c(5,4,4,2)+.1)
# par(cex=1)
# layout(1)
# dev.off()
#
# # -----------------------------------------------------------------------------
#
#
# # PLOT -- dailyBarplot for offRez
# png('daily_barplot_offrez.png', width=800, height=600)
# layout(matrix(seq(5)))
# par(cex=1.0)
# par(mar=c(3,4,1,2)+.1)
# monitor_dailyBarplot(Clarkston, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
# title('Clarkston', line=-1)
# monitor_dailyBarplot(Lewiston, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
# title('Lewiston', line=-1)
# monitor_dailyBarplot(Juliaetta, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
# title('Juliaetta', line=-1)
# monitor_dailyBarplot(Cottonwood, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
# title('Cottonwood', line=-1)
# monitor_dailyBarplot(Grangeville, ylim=c(0,300), ylab='', main='', labels_x_nudge=1.0, labels_y_nudge=70)
# title('Grangeville', line=-1)
# layout(1)
# par(mar=c(5,4,4,2)+.1)
# par(mar=c(5,4,4,2)+.1)
# par(cex=1)
# layout(1)
# dev.off()
#
# # -----------------------------------------------------------------------------
# # stacked barplot
#
# png('stacked_bars_onrez.png', width=800, height=600)
# #png('stacked_bars_offrez.png', width=800, height=600)
#
# par(mar=c(3.1,4.1,3.1,2.1))
#
# layout(matrix(seq(5)))
#
# for ( i in 1:5 ) {
#
#   mon <- onRezList[[i]]
#   #mon <- offRezList[[i]]
#
#   hours_1 <- monitor_dailyThreshold(mon, threshold=AQI$breaks_24[1])
#   hours_2 <- monitor_dailyThreshold(mon, threshold=AQI$breaks_24[2])
#   hours_3 <- monitor_dailyThreshold(mon, threshold=AQI$breaks_24[3])
#   hours_4 <- monitor_dailyThreshold(mon, threshold=AQI$breaks_24[4])
#   hours_5 <- monitor_dailyThreshold(mon, threshold=AQI$breaks_24[5])
#   hours_6 <- monitor_dailyThreshold(mon, threshold=AQI$breaks_24[6])
#
#   datetime <- hours_1$data[,1]
#   h6 <- hours_6$data[,2]
#   h5 <- hours_5$data[,2] - h6
#   h4 <- hours_4$data[,2] - h6 - h5
#   h3 <- hours_3$data[,2] - h6 - h5 - h4
#   h2 <- hours_2$data[,2] - h6 - h5 - h4 - h3
#   h1 <- hours_1$data[,2] - h6 - h5 - h4 - h3 - h2
#
#   mat <- cbind(h1,h2,h3,h4,h5,h6)
#
#   barplot(t(mat), col=AQI$colors[1:6], border='white', axes=FALSE)
#   title(onRezLabels[i], cex.main=3)
#   #title(offRezLabels[i], cex.main=3)
#   ###axis(2, at=seq(0,24,6), las=1)
#   ###axis.POSIXct(1, mon$data$datetime)
#   aug09Index <- which(hours_1$data$datetime == lubridate::ymd_h("2017-08-09 00", tz="America/Los_Angeles"))
#   sep05Index <- which(hours_1$data$datetime == lubridate::ymd_h("2017-09-05 00", tz="America/Los_Angeles"))
#   # NOTE:  Need to account for 0.2 spacing between bars so 1.2 axis units per label
#   at <- 1.2 * c(aug09Index,sep05Index) - 0.6
#   axis(1, at=at, labels=c('Aug 9', 'Sep 5'), lwd=0, lwd.ticks=2, cex.axis=2)
#
# }
#
# layout(1)
#
# par(oldPar)
#
# dev.off()
#
#
# # -----------------------------------------------------------------------------
#
#
#
# nowcast_Cottonwood <- monitor_nowcast(Cottonwood)
#
# bad_Cottonwood <- monitor_subset(Cottonwood, tlim=c(20170901, 20170914))
# bad_nowcast_Cottonwood <- monitor_subset(nowcast_Cottonwood, tlim=c(20170901, 20170914))
#
# # PLOT -- hourly/nowcast comparison
# png('cottonwood_nowcast_comparison.png', width=1000, height=750)
# par(cex=1.5)
# monitor_timeseriesPlot(bad_Cottonwood, type='p', pch=16, col='red', shadedNight=TRUE)
# monitor_timeseriesPlot(bad_nowcast_Cottonwood, type='l', col='black', lwd=2, add=TRUE)
# legend("topright", legend=c('Hourly','Nowcast'), col=c('red','black'), lwd=c(NA,2), pch=c(16,NA))
# title('Cottonwood Hourly and Nowcast')
# par(cex=1)
# dev.off()
# # -----------------------------------------------------------------------------
#
# bad_Cottonwood <- monitor_subset(Cottonwood, tlim=c(20170903, 20170911))
# bad_nowcast_Cottonwood <- monitor_subset(nowcast_Cottonwood, tlim=c(20170904, 20170911))
#
# # PLOT -- hourlyBarplot for Cottonwood
# png('cottonwood_hourly.png', width=800, height=600)
# par(cex=1.5)
# monitor_hourlyBarplot(bad_Cottonwood, dayCol='transparent', hourLwd=0,
#                           ylab='', main='Cottonwood Hourly',
#                           labels_x_nudge=5, labels_y_nudge=20,
#                           border=adjustcolor('white',0.2))
# par(cex=1)
# dev.off()
# # -----------------------------------------------------------------------------

