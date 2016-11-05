###############################################################
## By setting a customized time window, this script will show
## prescribed burns, satelite-detected burns, temporary monitors
## andpermanent monitors in WA                                                           
###############################################################

library(magrittr)
library(maps)
library(PWFSLSmoke)
library(classInt)

# REMINDER: run DNR_ingestData.R to get janice_SMA
# SMAsizeBy can take 'proposed', 'accomplished' and 'accomplishedFS'

# Variable definition for debugging
if ( FALSE) {
  startdate = 20160915
  timeWindow = 48
  FUN = max
  show_airsis = TRUE
  show_airnow = TRUE
  show_SMA = TRUE
  show_bluesky = TRUE
  SMAsizeBy='accomplished'
  
}

DNR_burn_map <- function(startdate = 20160915, timeWindow = 48, FUN = max, SMAsizeBy='accomplished',
                         show_airsis = TRUE, show_airnow = TRUE, show_SMA = TRUE, show_bluesky = TRUE,
                         show_monitorLegend = TRUE, show_burnLegend = TRUE, show_title = TRUE) {
  
  #------------------------------------------ styles ----------------------------------------------
  
  # color palette for both airsis and airnow monitors
  col_monitor <- AQI$colors  # can delet this since only use AQI colors
  
  #--------- airsis ---------
  breaks_airsis = AQI$breaks_24
  pch_airsis = 16
  cex_airsis = 3
  lwd_airsis = 1
  
  #-------- airnow -----------
  breaks_airnow = AQI$breaks_24
  pch_airnow = 16
  cex_airnow = 1.5
  lwd_airnow = 1
  
  #---------- SMA ------------
  SMA_breaksBy = 'quantile'
  pch_SMA = 2
  lwd_SMA = 3
  col_SMA = 'red'
  
  #-------- satelite ---------
  pch_bluesky = 17
  cex_bluesky = 1
  lwd_bluesky = 1
  col_bluesky = 'grey50'
  
  
  #------------------------------------------ data processing ----------------------------------------------
  if(SMAsizeBy == 'proposed'){
    SMAsizeBy = "Proposed.Tons"
  } else if (SMAsizeBy == 'accomplished') {
    SMAsizeBy = "Accomplished.Tons"
  } else if (SMAsizeBy == "accomplishedFS") {
    SMAsizeBy = "Accomplished.Tons.from.FS.fireportal"
  } else {
    stop(paste("Incorrect value for parameter 'SMAsizeBy'."))
  }
  
  # load necessary data objects
  load("localData/airsis_monitorList.RData")  #mobile monitors
  load("localData/airnow_monitors.RData")  #permanent monitors     
  load("localData/bluesky_eventsList.RData")  #satelite detectors
  
  startDate <- strptime(as.character(startdate),'%Y%m%d', tz='UTC') + 7*60*60
  endDate <- startDate + timeWindow*60*60
  
  #---------- process airsis ----------
  airsisMonitorDF <- data.frame(matrix(nrow = length(airsis_monitorList), ncol = 3))
  names(airsisMonitorDF) <- c("longitude", "latitude", "MaxPm25")
  row.names(airsisMonitorDF) <- names(airsis_monitorList)
  for (i in 1:nrow(airsisMonitorDF)) {
    airsisMonitorDF$longitude[i] <- airsis_monitorList[[i]]$meta$longitude
    airsisMonitorDF$latitude[i] <- airsis_monitorList[[i]]$meta$latitude
    airsisIndexes <- intersect(which(airsis_monitorList[[i]]$data$datetime >= startDate),
                               which(airsis_monitorList[[i]]$data$datetime <= endDate))
    airsisMonitorDF$MaxPm25[i] <- FUN(airsis_monitorList[[i]]$data[,2][airsisIndexes], na.rm=TRUE)
  }
  
  #---------- process airnow ---------
  airnowMonitorDF <- data.frame(matrix(NA, nrow=nrow(airnow_monitors$meta), ncol=3))
  names(airnowMonitorDF) <- c("longitude", "latitude", "MaxPm25")
  row.names(airnowMonitorDF) <- row.names(airnow_monitors$meta)
  airnowMonitorDF$longitude <- airnow_monitors$meta$longitude
  airnowMonitorDF$latitude <- airnow_monitors$meta$latitude
  airnowIndexes <- intersect(which(airnow_monitors$data$datetime >= startDate),
                             which(airnow_monitors$data$datetime <= endDate))
  for (i in 1:nrow(airnowMonitorDF)){
    airnowMonitorDF$MaxPm25[i] <- FUN(airnow_monitors$data[,i+1][airnowIndexes], na.rm=TRUE)
  }
  
  #---------- process satelite ----------
  blueskyIndex <- which(names(bluesky_eventsList) == as.character(startdate))
  blueskyDF <- rbind(bluesky_eventsList[[blueskyIndex]],
                     bluesky_eventsList[[blueskyIndex+1]],
                     bluesky_eventsList[[blueskyIndex+2]])
  blueskyDF <- as.data.frame(blueskyDF[which(blueskyDF$state == 'WA'),])
  
  #---------- process SMA ----------
  janice_SMA$ignitionUTC <- as.POSIXct(format(janice_SMA$Ignition.time,tz='UTC'),tz='UTC')
  subJaniceSMA <- janice_SMA[intersect(which(janice_SMA$ignitionUTC >= startDate), 
                                       which(janice_SMA$ignitionUTC <= endDate)),]
  subJaniceSMA <- as.data.frame(subJaniceSMA[!is.na(subJaniceSMA$Latitude),])
  
  
  #----------------------------------------------- plotting ---------------------------------------------------
  par(oma=c(1,1,1,1))
  par(mar=c(10, 10, 13, 10))
  map('county', 'wa', col='grey')
  
  # plot airsis points, default: filled dots colored by AQI
  if (show_airsis) {
    airsisColIndexes <- .bincode(airsisMonitorDF$MaxPm25, breaks=breaks_airsis)
    points(airsisMonitorDF$longitude, airsisMonitorDF$latitude, pch=pch_airsis, cex=cex_airsis, 
           col=col_monitor[airsisColIndexes])
    text(airsisMonitorDF$longitude, airsisMonitorDF$latitude, labels=airsisMonitorDF$MaxPm25, pos=c(3,rep(4,7)))
  }
  
  # plot airnow points, default: open dots colored by AQI
  if (show_airnow) {
    airnowColIndexes <- .bincode(airnowMonitorDF$MaxPm25, breaks=breaks_airnow)
    points(airnowMonitorDF$longitude, airnowMonitorDF$latitude, pch=pch_airnow, cex=cex_airnow, 
           lwd=lwd_airnow, col=col_monitor[airnowColIndexes])
    text(airnowMonitorDF$longitude, airnowMonitorDF$latitude, labels=airnowMonitorDF$MaxPm25, pos=4)
  }
  
  # plot SMA points, default: open triangle sized by SMAsizeBy 
  if (show_SMA) {
    breaks_SMA <- classIntervals(na.omit(janice_SMA[[SMAsizeBy]]), n=4, style=SMA_breaksBy)$brks
    SMA_sizeIndex <- .bincode(subJaniceSMA[[SMAsizeBy]], breaks=breaks_SMA)
    pch_SMA <- rep(6, nrow(subJaniceSMA))
    pch_SMA[which(subJaniceSMA$DNR_Pilot.24.Hr.Advance)] <- 2
    points(subJaniceSMA$Longitude, subJaniceSMA$Latitude, pch=pch_SMA, lwd=lwd_SMA, col=col_SMA, cex=SMA_sizeIndex)
  }
  
  # plot satelite poits, default: small filled triangle
  if (show_bluesky) {
    points(blueskyDF$longitude, blueskyDF$latitude, pch=pch_bluesky, cex=cex_bluesky, col=col_bluesky)
  }
  
  # legend and title
  if (show_monitorLegend){
    legend(x='bottomleft', legend=AQI$names, col=AQI$colors, pch=16, title='Monitors', bg='transparent', 
           cex=0.9, bty='n')
  }
  
  if (show_burnLegend) {
    legend(x='topleft', legend=c('24hr-SMA', 'SMA', 'bluesky'), col=c(col_SMA, col_SMA, col_bluesky),
           cex=0.9, pch=c(2, 6, 17), title="Burns", bg='transparent', bty='n')
  }
  
  if (show_title) {
    title(sprintf("Burns and Monitor Detects from %s to %s", format(startDate, '%Y-%m-%d'), 
                  format(endDate, '%Y-%m-%d')), line=1)
  }
}