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

# Load and clean up all required data with:
#   source('localDNR/DNR_ingestData.R')

DNR_timeseriesPlot <- function(monitorName, tlim=c(20160901,20161015),
                               fireDistance=25,
                               ylim_pm25=NULL,
                               showShadedNight=TRUE,
                               show1Hr=TRUE,
                               show3Hr=FALSE,
                               show24Hr=TRUE,
                               showDaily=TRUE,
                               showPrescribedNonPilotBurns=TRUE,
                               showPrescribedPilotBurns=TRUE,
                               showBlueskyEvents=TRUE,
                               showAQILevels=TRUE) {
  
  # ----- Style ---------------------------------------------------------------
  
  col_aqi <- adjustcolor(AQI$colors[2:6], 0.4)
  col_1 <- adjustcolor('black', 0.5)
  col_3 <- adjustcolor('black', 0.5)
  col_24 <- adjustcolor('purple', 0.3)
  col_daily <- adjustcolor('black', 1.0)
  
  col_nonPilotBurns <- 'orange'
  col_pilotBurns <- 'red'
  col_events <- 'gray50'
  
  pch_1 <- 16
  pch_events <- 17
  
  cex_1 <- 1
  cex_events <- 3
  
  lwd_aqi <- 4
  lwd_3 <- 2
  lwd_24 <- 4
  lwd_daily <- 6
  
  # ----- Data Preparation ----------------------------------------------------
  
  # Pull out a single monitor
  monitorID <- monitorDict[[monitorName]]
  if ( monitorID %in% rownames(airsis_monitors$meta) ) {
    title <- paste0("Temporary Monitor in ",monitorName," Washington")
    ws_monitor <- monitor_subset(airsis_monitors, monitorIDs=monitorID)
  } else {
    # TODO:  Remove this time trimming
    tlim[2] <- min(tlim[2],20160930)
    title <- paste0("Permanent Monitor in ",monitorName," Washington")
    ws_monitor <- monitor_subset(airnow_monitors, monitorIDs=monitorID)
  }
  
  # Temporal subset
  ws_monitor <- monitor_subset(ws_monitor, tlim=tlim)
  
  # Create 3hr-rolling, 24hr-rolling and daily averages
  ws_monitor_3hr <- monitor_rollingMean(ws_monitor, width=3, align="center")
  ws_monitor_24hr <- monitor_rollingMean(ws_monitor, width=24, align="right")
  ws_monitor_daily <- monitor_dailyStatistic(ws_monitor, FUN=mean, dayStart='midnight')
  
  # Local timezone tlo and thi
  tlo <- parseDatetime(tlim[1], timezone=ws_monitor$meta$timezone)
  thi <- parseDatetime(tlim[2], timezone=ws_monitor$meta$timezone)
  
  # Get nearby prescribed burns
  prescribedDistance <- distance(ws_monitor$meta$longitude, ws_monitor$meta$latitude, janice_SMA$Longitude, janice_SMA$Latitude)
  distanceMask <- prescribedDistance <= fireDistance
  localTime <- lubridate::with_tz(janice_SMA$datetime, ws_monitor$meta$timezone)
  timeMask <- localTime >= tlo & localTime <= thi
  mask <- distanceMask & timeMask
  prescribedBurns <- janice_SMA[mask,]
  if ( nrow(prescribedBurns) > 0 ) {
    timeInfo <- timeInfo(prescribedBurns$datetime, ws_monitor$meta$longitude, ws_monitor$meta$latitude, ws_monitor$meta$timezone)
    prescribedBurns$sunset <- timeInfo$sunset
    # Separate non-pilot vs. pilot burns
    prescribedNonPilotBurns <- prescribedBurns[!prescribedBurns$DNR_Pilot.24.Hr.Advance,]
    prescribedPilotBurns <- prescribedBurns[prescribedBurns$DNR_Pilot.24.Hr.Advance,]
    burn_ylim <- c(0,max(prescribedBurns$Accomplished.Tons, na.rm=TRUE))
  } else {
    prescribedNonPilotBurns <- prescribedPilotBurns <- prescribedBurns
    burn_ylim <- c(0,100)
  }
  
  
  # Get nearby events
  eventsDistance <- distance(ws_monitor$meta$longitude, ws_monitor$meta$latitude, bluesky_events$longitude, bluesky_events$latitude)
  distanceMask <- eventsDistance <= fireDistance
  localTime <- parseDatetime(bluesky_events$datestamp, timezone=ws_monitor$meta$timezone)
  timeMask <-  localTime >= tlo & localTime <= thi
  mask <- distanceMask & timeMask
  events <- bluesky_events[mask,]
  if ( nrow(events) > 0 ) {
    timeInfo <- timeInfo(as.POSIXct(events$datetime), ws_monitor$meta$longitude, ws_monitor$meta$latitude, ws_monitor$meta$timezone)
    events$solarnoon <- timeInfo$solarnoon
  }
  
  # ----- Plotting ------------------------------------------------------------
  
  # Blank plot to get the axes
  x <- lubridate::with_tz(ws_monitor$data[,1], ws_monitor$meta$timezone)
  y <- ws_monitor$data[,2]
  if ( is.null(ylim_pm25) ) {
    plot(x, y, col='transparent', xlab='', ylab='', las=1)
  } else {
    plot(x, y, col='transparent', xlab='', ylab='', las=1, ylim=ylim_pm25)
    
  }
  
  # Get useful plot measures
  usr <- par('usr')
  xlo <- usr[1]
  xhi <- usr[2]
  ylo <- usr[3]
  yhi <- usr[4]
  xrange <- xhi - xlo
  yrange <- yhi - ylo
  
  if ( showShadedNight ) {
    timeInfo <- timeInfo(ws_monitor$data[,1], lon=ws_monitor$meta$longitude, lat=ws_monitor$meta$latitude)
    addShadedNights(timeInfo)
  }

  # AQI levels
  if ( showAQILevels ) {
    abline(h=AQI$breaks_24[2:6], col=col_aqi, lwd=lwd_aqi)
  }
  
  if ( showPrescribedNonPilotBurns ) {
    if ( nrow(prescribedNonPilotBurns) > 0 ) {
      # PrescribedBurn rectangles
      xleft <- prescribedNonPilotBurns$Ignition.time
      xright <- prescribedNonPilotBurns$sunset
      ybottom <- 0
      yval <- prescribedNonPilotBurns$Accomplished.Tons
      ytop <- yval * ((0.95*yhi)/burn_ylim[2])
      rect(xleft, ybottom, xright, ytop, density=20, angle=45,
           col=col_nonPilotBurns, border=col_nonPilotBurns, lty='solid', lwd=2)
    }
  }
  
  if ( showPrescribedPilotBurns ) {
    if ( nrow(prescribedPilotBurns) > 0 ) {
      # PrescribedBurn rectangles
      xleft <- prescribedPilotBurns$Ignition.time
      xright <- prescribedPilotBurns$sunset
      ybottom <- 0
      yval <- prescribedPilotBurns$Accomplished.Tons
      ytop <- yval * ((0.95*yhi)/burn_ylim[2])
      rect(xleft, ybottom, xright, ytop, density=20, angle=45,
           col=col_pilotBurns, border=col_pilotBurns, lty='solid', lwd=2)
    }
  }
  
  if ( show1Hr ) {
    monitor_timeseriesPlot(ws_monitor, add=TRUE, pch=pch_1, cex=cex_1, col=col_1)
  }
  
  if ( show3Hr ) {
    monitor_timeseriesPlot(ws_monitor_3hr, add=TRUE, type='l', col=col_3, lwd=lwd_3)
  }
  
  if ( show24Hr ) {
    monitor_timeseriesPlot(ws_monitor_24hr, add=TRUE, type='l', col=col_24, lwd=lwd_24)
  }
  
  if ( showDaily ) {
    # move time axis from noon to previous midnight for plotting with type='s'
    x <- ws_monitor_daily$data$datetime - lubridate::dhours(12)
    y <- ws_monitor_daily$data[,2]
    points(x, y, type='s', col=col_daily, lwd=lwd_daily)
  }
  
  if ( showBlueskyEvents ) {
    # Bluesky events
    ###text(events$solarnoon, (ylo + 0.02*yrange), labels='*', cex=3, col='firebrick')
    x <- events$solarnoon
    y <- rep((ylo+0.00*yrange), length(x)) 
    points(x, y, pch=pch_events, cex=cex_events, col=col_events)
  }
  
  
  # Annotations
  title(title)
#   legend('topleft',
#          c("3-hr average (centered)",
#            "24-hr average (lagged)"),
#          col=c(col_3, col_24),
#          lwd=c(lwd_3, lwd_24))
  
  
}


# 1) timeseris plot should only show hourly points above ~20,
#    prescribed burns, regulatory 24-hr and satellite detects
#    also need tons accomplished scale
# 2) terrain map over 1-3 days focused on a monitor and
#    nearby prescribed burns and wildfires
# 3) csv dumps of all data used


