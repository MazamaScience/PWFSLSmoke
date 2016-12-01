###############################################################################
# DNR Pilot Burn Pojrect (2928)
#
# The R code for the DNR pilot burn project data analysis is broken down into 
# the following steps, each associated with a separate file:
#
# * DNR_downloadData.R   -- download, QC, reshape and convert into .RData format
# * DNR_ingestData.R     -- ingest previously converted data and peroform any cleanup
#                           (e.g. convert negative values of PM2.5 to 0.0)
#
# Once all of this work is done, we are ready for the plotting scripts:
#
# * DNR_timeseriesPlot.R -- timeseries plot for a monitor of interest
# * DNR_burnMap.R        -- map of all burns near a monitoring location
###############################################################################

# This functions defined in DNR_utils.R provide additional analysis.

burnMonitorDistance <- function() {
  
  monitorLons <- c(airsis_monitors$meta$longitude, airnow_monitors$meta$longitude)
  monitorLats <- c(airsis_monitors$meta$latitude, airnow_monitors$meta$latitude)
  monitorNames <- names(monitorDict)
  
  distanceMatrix <- matrix(nrow=nrow(janice_SMA), ncol=length(monitorNames))
  
  for (row in 1:nrow(janice_SMA)) {
    
    print(row)
    targetLon <- janice_SMA$Longitude[row]
    targetLat <- janice_SMA$Latitude[row]
    if ( !is.na(targetLon) && !is.na(targetLat) ) {
      distances <- distance(targetLon, targetLat, monitorLons, monitorLats)
    } else {
      distances <- rep(NA, length(monitorNames))
    }
    
    distanceMatrix[row,] <- distances
    
  }
  
  distanceDF <- as.data.frame(distanceMatrix)
  names(distanceDF) <- paste0('Distance from ',monitorNames)
  
  janice_SMA_withDistance <- dplyr::bind_cols(janice_SMA, distanceDF)
  readr::write_csv(janice_SMA_withDistance, path='./localData/prescribed_burns_distance_from_monitors.csv')

}




###############################################################################
###############################################################################
###############################################################################

# # Calcualte an index for the probability that people will light their woodstoves.
# woodStoveIndex <- function(raw, tempVar='AT', humidityVar='RHx',
#                            tempHi=50, tempLo=32, unit='F', tempWeight=0.8,
#                            rollingWidth=1, rollingAlign='center') {
#   
#   # NOTE:  Assume incoming temperatures are in degrees C.
#   
#   # Pull out variables
#   temp <- raw[[tempVar]]
#   humidity <- raw[[humidityVar]]
#   
#   # Convert units
#   if ( unit == 'F' ) {
#     tempHi <- (tempHi - 32) * 5/9
#     tempLo <- (tempLo - 32) * 5/9
#   }
#   
#   # NOTE:  tempIndex goes from 0 for anything above tempHi to 1 for anything below tempLo.
#   
#   # Low temps have hi values
#   tempIndex <- tempHi - temp
#   # Anything above tempHi is 0
#   tempIndex <- ifelse(tempIndex > 0, tempIndex, 0)
#   # Scale values 
#   tempIndex <- tempIndex / (tempHi - tempLo)
#   # Anything below tempLo is 1.0
#   tempIndex <- ifelse(tempIndex < 1, tempIndex, 1)
# 
#   # humidityIndex goes from 0 to 1
#   humidityIndex <- humidity / 100
#   
#   # Final weighting
#   woodStoveIndex <- tempIndex * tempWeight + humidityIndex * (1 - tempWeight)
#   
#   # Apply rolling mean before returning
#   if (rollingWidth > 1) {
#     df <- data.frame(date=raw$datetime, index=woodStoveIndex)
#     df <- openair::rollingMean(df, pollutant='index', new.name='new_index', width=rollingWidth, align=rollingAlign)
#     woodStoveIndex <- df$new_index
#   }
#   
#   return(woodStoveIndex)
# }
# 
# 
# 
# DNR_timeseriesPlot <- function(ws_monitor, title='', tlim=NULL, ...) {
#   
#   # Style
#   col_1 <- adjustcolor('black',0.9)
#   col_3 <- adjustcolor('goldenrod', 0.9)
#   col_24 <- adjustcolor('purple', 0.5)
#   
#   pch_1 <- 1
#   cex_1 <- 1
#   
#   lwd_3 <- 2
#   lwd_24 <- 6
#   
#   # Data Prep
#   ws_monitor <- monitor_subset(ws_monitor, tlim=tlim)
#   ws_monitor_3hr <- monitor_rollingMean(ws_monitor, width=3, align="center")
#   ws_monitor_24hr <- monitor_rollingMean(ws_monitor, width=24, align="right")
#   
#   # Plotting
#   monitor_timeseriesPlot(ws_monitor, shadedNight=TRUE, pch=pch_1, cex=cex_1, col=col_1, ...)
#   monitor_timeseriesPlot(ws_monitor_3hr, type='l', col=col_3, lwd=lwd_3, add=TRUE)
#   monitor_timeseriesPlot(ws_monitor_24hr, type='l', col=col_24, lwd=lwd_24, add=TRUE)
#   title(title)
#   legend('topleft',
#          c("3-hr average (centered)","24-hr average (lagged)"),
#          col=c(col_3, col_24),
#          lwd=c(lwd_3, lwd_24))
# }
# 
# 
# DNR_stoveWindPlot <- function(ws_monitor, raw, 
#                               tempVar='AT', humidityVar='RHx', windVar='W.S',
#                               title='', tlim=NULL) {
#   
#   # Flusing Winds
#   windBin <- .bincode(raw$W.S, c(-Inf,1,2,5,10,Inf))
#   for (i in 5) { colors[i] <- adjustcolor('blue',(i-1)/5) }
# 
#   # Create fancy woodburning index
#   stoveIndex <- woodStoveIndex(raw, tempVar, humidityVar, rollingWidth=12, rollingAlign='left')
#   stoveSmoke <- ws_monitor$data[,2]
#   stoveSmoke[stoveIndex < 0.5] <- NA
# 
#   monitor_timeseriesPlot(ws_monitor, type='l', shadedNight=FALSE)
#   points(stoveSmoke ~ ws_monitor$data$datetime, pch=16, col='purple')
#   abline(v=raw$datetime, col=colors[windBin])
#   legend('topleft', legend=c("Woodstove Index", "0-1 m/s Wind", "1-2 m/s Wind", "2-5 m/s Wind"), fill=c('purple',colors[1],colors[2],colors[3]))
#   
# }
# 
# readBlueskyEmissions <- function(url) {
#   df <- readr::read_csv(url)
#   df$localtime <- parseDatetime(stringr::str_sub(df$date_time,1,12))
#   lubridate::tz(df$localtime) <- 'America/Los_Angeles'
#   df$utc <- lubridate::with_tz(df$localtime, 'UTC')
# }
# 
# 
# DNR_averagesPlot <- function(ws_monitor, title) {
#   layout(matrix(seq(2)))
#   DNR_timeseriesPlot(ws_monitor, title=title)
#   monitor_dailyBarPlot(ws_monitor)
# }
