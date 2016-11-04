###############################################################
##                                                           ##
## TODO: add timewindow(hour), legend, title                 ##
##                                                           ##
###############################################################
library(magrittr)
library(maps)
library(PWFSLSmoke)

# SMAsizeBy can take 'proposed', 'accomplished' and 'accomplishedFS'

DNR_burn_map <- function(startdate = 20160915, SMAsizeBy = 'accomplished') {
  
  if(SMAsizeBy == 'proposed'){
    SMAsizeBy = "Proposed Tons"
  } else if (SMAsizeBy == 'accomplished') {
    SMAsizeBy = "Accomplished Tons"
  } else if (SMAsizeBy == "accomplishedFS") {
    SMAsizeBy = "Accomplished Tons from FS fireportal"
  } else {
    stop(paste("Incorrect value for parameter 'SMAsizeBy'."))
  }
  
  # load necessary data objects
  load("localData/airsis_monitorList.RData")  #mobile monitors
  load("localData/airnow_monitors.RData")  #permanent monitors     
  load("localData/bluesky_eventsList.RData")  #satelite detectors
  # run DNR_ingestData to get SMA dataframe

  startDate <- strptime(as.character(startdate),'%Y%m%d', tz='UTC') + 7*60*60
  endDate <- startDate + 48*60*60

  ### process airsis ###### 
  airsisMonitorDF <- data.frame(matrix(nrow = length(airsis_monitorList), ncol = 3))
  names(airsisMonitorDF) <- c("longitude", "latitude", "MaxPm25")
  row.names(airsisMonitorDF) <- names(airsis_monitorList)
  for (i in 1:nrow(airsisMonitorDF)) {
    airsisMonitorDF$longitude[i] <- airsis_monitorList[[i]]$meta$longitude
    airsisMonitorDF$latitude[i] <- airsis_monitorList[[i]]$meta$latitude
    airsisIndexes <- intersect(which(airsis_monitorList[[i]]$data$datetime >= startDate),
                         which(airsis_monitorList[[i]]$data$datetime <= endDate))
    airsisMonitorDF$MaxPm25[i] <- max(airsis_monitorList[[i]]$data[,2][airsisIndexes], na.rm=T)
  }
  
  ##### process airnow #####
  airnowMonitorDF <- data.frame(matrix(NA, nrow=nrow(airnow_monitors$meta), ncol=3))
  names(airnowMonitorDF) <- c("longitude", "latitude", "MaxPm25")
  row.names(airnowMonitorDF) <- row.names(airnow_monitors$meta)
  airnowMonitorDF$longitude <- airnow_monitors$meta$longitude
  airnowMonitorDF$latitude <- airnow_monitors$meta$latitude
  airnowIndexes <- intersect(which(airnow_monitors$data$datetime >= startDate),
                             which(airnow_monitors$data$datetime <= endDate))
  for (i in 1:nrow(airnowMonitorDF)){
    airnowMonitorDF$MaxPm25[i] <- max(airnow_monitors$data[,i+1][airnowIndexes], na.rm=TRUE)
  }
  
  ##### process satelite #####
  blueskyIndex <- which(names(bluesky_eventsList) == as.character(startdate))
  blueskyDF <- rbind(bluesky_eventsList[[blueskyIndex]],
                     bluesky_eventsList[[blueskyIndex+1]],
                     bluesky_eventsList[[blueskyIndex+2]])
  blueskyDF <- as.data.frame(blueskyDF[which(blueskyDF$state == 'WA'),])
  # blueskyDF$datetime <- strptime(blueskyDF$datestamp,'%Y%m%d', tz='UTC') + 7*60*60
  
  ##### process SMA #####
  ignitionTime <- as.numeric(janice_SMA$`Ignition time`)
  ignitionTime[is.na(ignitionTime)] <- 0
  ignitionHour <- floor(ignitionTime/100)
  ignitionMinute <- (floor(ignitionTime/10) - ignitionHour*10)*10
  ignitionSecond <- ignitionTime - ignitionHour*100 - ignitionMinute
  janice_SMA$datetimeAltered <- as.POSIXct(janice_SMA$datetime, tz='UTC') + 
    ignitionHour*60*60 + ignitionMinute*60 + ignitionSecond
  
  subJaniceSMA <- janice_SMA[intersect(which(janice_SMA$datetimeAltered >= startDate), 
                        which(janice_SMA$datetimeAltered <= endDate)),]
  subJaniceSMA <- as.data.frame(subJaniceSMA[!is.na(subJaniceSMA$Latitude),])
  
  ### plotting ###
  # par(mar = c(0,0,0,0))
  par(mar=c(15,14,13,13))
  map('county', 'wa', col='grey')
  
  # plot airsis points: filled dots colored by AQI
  airsisColIndexes <- .bincode(airsisMonitorDF$MaxPm25, breaks=AQI$breaks_24)
  points(airsisMonitorDF$longitude, airsisMonitorDF$latitude, pch=16, cex=3, col=AQI$colors[airsisColIndexes])
  text(airsisMonitorDF$longitude, airsisMonitorDF$latitude, labels=airsisMonitorDF$MaxPm25, pos=c(3,rep(4,7)))
  
  # plot airnow points: open dots colored by AQI
  airnowColIndexes <- .bincode(airnowMonitorDF$MaxPm25, breaks=AQI$breaks_24)
  points(airnowMonitorDF$longitude, airnowMonitorDF$latitude, pch=16, cex=1.5, lwd=2, col=AQI$colors[airnowColIndexes])
  text(airnowMonitorDF$longitude, airnowMonitorDF$latitude, labels=airnowMonitorDF$MaxPm25, pos=4)
  

  
  # plot SMA points: open triangle sized by SMAsizeBy 
  bSMA <- quantile(janice_SMA[SMAsizeBy][[1]], na.rm=TRUE)
  SMAsizeIndex <- .bincode(subJaniceSMA[SMAsizeBy][[1]], breaks=bSMA)
  points(subJaniceSMA$Longitude, subJaniceSMA$Latitude, pch=2, lwd=4, col='red', cex=SMAsizeIndex)
  
  # plot satelite poits: small filled triangle
  points(blueskyDF$longitude, blueskyDF$latitude, pch=17, cex=1, col='grey50')
}