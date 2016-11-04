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

# This DNR_timeseriesPlot.R script defines a function for creating timeseries plots
# for a named monitor with markers for nearby fires.

# Load and clean up all required data
source('localDNR/DNR_ingestData.R')

DNR_timeseriesPlot <- function(monitorName, tlim=c(20160901,20161015),
                               fireDistance=25, ...) {
  
  # ----- Style ---------------------------------------------------------------
  
  col_1 <- adjustcolor('black',0.9)
  col_3 <- adjustcolor('goldenrod', 0.9)
  col_24 <- adjustcolor('purple', 0.5)
  
  pch_1 <- 1
  cex_1 <- 1
  
  lwd_3 <- 2
  lwd_24 <- 6
  
  # ----- Data Preparation ----------------------------------------------------
  
  monitorID <- monitorDict[monitorName]
  if ( monitorID %in% rownames(airsis_monitors$meta) ) {
    title <- paste0("Temporary Monitor in ",monitorName," Washington")
    ws_monitor <- monitor_subset(airsis_monitors, monitorIDs=monitorID)
  } else {
    title <- paste0("Temporary Monitor in ",monitorName," Washington")
    ws_monitor <- monitor_subset(airnow_monitors, monitorIDs=monitorID)
  }
  
  ws_monitor <- monitor_subset(ws_monitor, tlim=tlim)
  ws_monitor_3hr <- monitor_rollingMean(ws_monitor, width=3, align="center")
  ws_monitor_24hr <- monitor_rollingMean(ws_monitor, width=24, align="right")
  
  # Get nearby prescribed burns and create a localtime column
  prescribedDistance <- distance(ws_monitor$meta$longitude, ws_monitor$meta$latitude, janice_SMA$Longitude, janice_SMA$Latitude)
  prescribedBurns <- janice_SMA[prescribedDistance <= fireDistance,]
  prescribedBurns$localtime <- lubridate::with_tz(prescribedBurns$datetime, ws_monitor$meta$timezone)
  
  # Get nearby events and create a localtime column
  eventsDistance <- distance(ws_monitor$meta$longitude, ws_monitor$meta$latitude, bluesky_events$longitude, bluesky_events$latitude)
  events <- bluesky_events[eventsDistance <= fireDistance,]
  events$localtime <- lubridate::with_tz(events$datetime, ws_monitor$meta$timezone)
  
  # ----- Plotting ------------------------------------------------------------
  
  monitor_timeseriesPlot(ws_monitor, shadedNight=TRUE, pch=pch_1, cex=cex_1, col=col_1)#, ...)
  monitor_timeseriesPlot(ws_monitor_3hr, type='l', col=col_3, lwd=lwd_3, add=TRUE)
  monitor_timeseriesPlot(ws_monitor_24hr, type='l', col=col_24, lwd=lwd_24, add=TRUE)
  
  # Get useful plot measures
  usr <- par('usr')
  xlo <- usr[1]
  xhi <- usr[2]
  ylo <- usr[3]
  yhi <- usr[4]
  xrange <- xhi - xlo
  yrange <- yhi - ylo
  
  # Annotations
  title(title)
  legend('topleft',
         c("3-hr average (centered)","24-hr average (lagged)"),
         col=c(col_3, col_24),
         lwd=c(lwd_3, lwd_24))
  
  abline(v=prescribedBurns$localtime, lwd=2, col='firebrick')
  abline(v=events$localtime, lwd=2, col='orange')
  
  
}