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

# * hourly values at USG or above
# * daily 'S' as black line
# * all pilot burns as bars
# * events as baseline triangles
# * optional day/night
# * legend
# * two y-axes

# 1) timeseries plot should only show hourly points above ~20,
#    prescribed burns, regulatory 24-hr and satellite detects
#    also need tons accomplished scale
# 2) terrain map over 1-3 days focused on a monitor and
#    nearby prescribed burns and wildfires
# 3) csv dumps of all data used


if (FALSE) {
  
  for (monitorName in names(monitorDict)) {
    png(filename=paste0(monitorName,'.png'), width=1200, height=900)
    DNR_timeseriesPlot(monitorName)
    dev.off()
  }
  
}

DNR_timeseriesPlot <- function(monitorName, tlim=c(20160901,20161015),
                               fireDistance=25,
                               ylim_pm25=c(0,120),
                               ylim_burnTons=c(0,2000),
                               showShadedNight=FALSE,
                               show1Hr=TRUE,
                               show3Hr=TRUE,
                               show24Hr=FALSE,
                               showDaily=TRUE,
                               showPrescribedNonPilotBurns=TRUE,
                               showPrescribedPilotBurns=TRUE,
                               showBlueskyEvents=TRUE,
                               showAQILevels=TRUE,
                               showAQIBackground=FALSE,
                               hourlyThreshold=AQI$breaks_24[2]) {
  
  # ----- Style ---------------------------------------------------------------
  
  col_aqi <- adjustcolor(AQI$colors[2:6], 0.4)
  col_1 <- adjustcolor('black', 0.4)
  col_3 <- adjustcolor('black', 0.2)
  col_24 <- adjustcolor('purple', 0.3)
  col_daily <- adjustcolor('black', 1.0)
  
  col_nonPilotBurns <- 'wheat4'
  col_pilotBurns <- 'red'
  col_events <- 'gray50'
  
  pch_1 <- 16
  pch_events <- 17
  
  cex_1 <- 1
  cex_events <- 3
  
  lwd_aqi <- 4
  lwd_3 <- 2
  lwd_24 <- 4
  lwd_daily <- 4
  
  # ----- Data Preparation ----------------------------------------------------
  
  # Cardinal Directions for use with bearings (0=N, clockwise)
  cardinalDirections <- c('N','NE','E','SE','S','SW','W','NW')

  # Pull out a single monitor
  monitorID <- monitorDict[[monitorName]]
  if ( monitorID %in% rownames(airsis_monitors$meta) ) {
    title <- paste0(monitorName," Washington Smoke and Burns (within ",fireDistance," km)")
    ws_monitor <- monitor_subset(airsis_monitors, monitorIDs=monitorID)
  } else {
    title <- paste0(monitorName," Washington Smoke and Burns (within ",fireDistance," km)")
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
  janice_SMA$distanceToMonitor <- prescribedDistance
  # Get the bearing
  p1 <- as.matrix(janice_SMA[,c('Longitude','Latitude')])
  p2 <- c(ws_monitor$meta$longitude, ws_monitor$meta$latitude)
  bearing <- geosphere::bearing(p1,p2)
  janice_SMA$bearingToMonitor <- bearing
  direction <- cardinalDirections[ .bincode(bearing-22.5, seq(-23,360,45))]
  janice_SMA$directionToMonitor <- direction
  # Subset based on distance
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
    if ( is.null(ylim_burnTons) ) {
      ylim_burnTons <- c(0,max(prescribedBurns$Accomplished.Tons.from.FS.fireportal, na.rm=TRUE))
    }
  } else {
    prescribedNonPilotBurns <- prescribedPilotBurns <- prescribedBurns
  }
  
  
  # Get nearby events
  eventsDistance <- distance(ws_monitor$meta$longitude, ws_monitor$meta$latitude, bluesky_events$longitude, bluesky_events$latitude)
  bluesky_events$distanceToMonitor <- eventsDistance
  # Get the bearing
  p1 <- as.matrix(bluesky_events[,c('longitude','latitude')])
  p2 <- c(ws_monitor$meta$longitude, ws_monitor$meta$latitude)
  bearing <- geosphere::bearing(p1,p2)
  bluesky_events$bearingToMonitor <- bearing
  direction <- cardinalDirections[ .bincode(bearing-22.5, seq(-23,360,45))]
  bluesky_events$directionToMonitor <- direction
  # Subset based on distance
  distanceMask <- eventsDistance <= fireDistance
  localTime <- parseDatetime(bluesky_events$datestamp, timezone=ws_monitor$meta$timezone)
  timeMask <-  localTime >= tlo & localTime <= thi
  mask <- distanceMask & timeMask
  events <- bluesky_events[mask,]
  if ( nrow(events) > 0 ) {
    timeInfo <- timeInfo(as.POSIXct(events$datetime), ws_monitor$meta$longitude, ws_monitor$meta$latitude, ws_monitor$meta$timezone)
    events$solarnoon <- timeInfo$solarnoon
  }
  
  # ----- Save CSV version of burns and events displayed in the plot ----------
  
  readr::write_csv(prescribedBurns, paste0(monitorName,'_prescribd_burns.csv'))
  readr::write_csv(events, paste0(monitorName,'_event_detects.csv'))
  
  # ----- Plotting ------------------------------------------------------------
  
  old_mar <- c(5.1,4.1,4.1,2.1)
  par(mar=c(5.1,7.1,4.1,7.1))
  
  # Blank plot to get the axes
  x <- lubridate::with_tz(ws_monitor$data[,1], ws_monitor$meta$timezone)
  y <- ws_monitor$data[,2]
  if ( is.null(ylim_pm25) ) {
    plot(x, y, col='transparent', xlab='Date', ylab='', las=1)
  } else {
    plot(x, y, col='transparent', xlab='Date', ylab='', las=1, ylim=ylim_pm25)
  }
  
  # Zero line is nice
  abline(h=0, lwd=2, col=col_events)
  
  # Get useful plot measures
  usr <- par('usr')
  xlo <- as.POSIXct(usr[1], tz=ws_monitor$meta$timezone, origin=lubridate::origin)
  xhi <- as.POSIXct(usr[2], tz=ws_monitor$meta$timezone, origin=lubridate::origin)
  ylo <- usr[3]
  yhi <- usr[4]
  xrange <- xhi - xlo
  yrange <- yhi - ylo
  
  if ( showShadedNight ) {
    timeInfo <- timeInfo(ws_monitor$data[,1], lon=ws_monitor$meta$longitude, lat=ws_monitor$meta$latitude)
    addShadedNights(timeInfo)
  }
  
  # AQI background
  if ( showAQIBackground ) {
    for (i in 1:6) {
      rect(xlo, AQI$breaks_24[i], xhi, AQI$breaks_24[i+1], col=adjustcolor(AQI$colors[i], 0.1), border=NULL)
    }
  }
  
  # AQI levels
  if ( showAQILevels ) {
    abline(h=AQI$breaks_24[2:6], col=col_aqi, lwd=lwd_aqi)
  }
  
  # Show daily average underneath the pilot burns
  if ( showDaily ) {
    # move time axis from noon to previous midnight for plotting with type='s'
    x <- ws_monitor_daily$data$datetime - lubridate::dhours(12)
    y <- ws_monitor_daily$data[,2]
    points(x, y, type='s', col=col_daily, lwd=lwd_daily)
    x <- ws_monitor_daily$data$datetime + lubridate::dhours(12)
    points(x, y, type='S', col=col_daily, lwd=lwd_daily)
  }
  
  
  if ( showPrescribedNonPilotBurns ) {
    if ( nrow(prescribedNonPilotBurns) > 0 ) {
      # PrescribedBurn rectangles
      xleft <- prescribedNonPilotBurns$Ignition.time
      ###xright <- prescribedNonPilotBurns$sunset # sunset
      xright <- lubridate::ceiling_date(xleft, "day") # midnight
      ybottom <- 0
      yval <- prescribedNonPilotBurns$Accomplished.Tons.from.FS.fireportal
      ytop <- yval * ((0.95*yhi)/ylim_burnTons[2])
      rect(xleft, ybottom, xright, ytop, density=20, angle=45,
           col=col_nonPilotBurns, border=col_nonPilotBurns, lty='solid', lwd=2)
    }
  }
  
  if ( showPrescribedPilotBurns ) {
    if ( nrow(prescribedPilotBurns) > 0 ) {
      # PrescribedBurn rectangles
      xleft <- prescribedPilotBurns$Ignition.time
      ###xright <- prescribedPilotBurns$sunset # sunset
      xright <- lubridate::ceiling_date(xleft, "day") # midnight
      ybottom <- 0
      yval <- prescribedPilotBurns$Accomplished.Tons.from.FS.fireportal
      ytop <- yval * ((0.95*yhi)/ylim_burnTons[2])
      rect(xleft, ybottom, xright, ytop, density=20, angle=-45,
           col=col_pilotBurns, border=col_pilotBurns, lty='solid', lwd=2)
    }
  }
  
  if ( show1Hr ) {
    ws_monitor_highValues <- monitor_subset(ws_monitor, vlim=c(hourlyThreshold,Inf)) # USG is level 3
    if ( any(!is.na(ws_monitor_highValues$data[,2])) ) {
      monitor_timeseriesPlot(ws_monitor_highValues, add=TRUE, pch=pch_1, cex=cex_1, col=col_1)
    }
    ###monitor_timeseriesPlot(ws_monitor, add=TRUE, pch=pch_1, cex=cex_1, col=col_1)
  }
  
  if ( show3Hr ) {
    monitor_timeseriesPlot(ws_monitor_3hr, add=TRUE, type='l', col=col_3, lwd=lwd_3)
  }
  
  if ( show24Hr ) {
    monitor_timeseriesPlot(ws_monitor_24hr, add=TRUE, type='l', col=col_24, lwd=lwd_24)
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

  # Top label for axis 2
  text(xlo, yhi*1.05, labels='PM2.5', pos=2, xpd=NA)
  text(xlo, yhi*1.02, labels='(ug/m3)', pos=2, xpd=NA)

  # AQI labels  
  if ( showAQILevels ) {
    text(xlo, AQI$breaks[2:6]+2, labels=AQI$names[2:6], pos=4)
  }
  
  # Add a second Y axis for burnTons
  if ( showPrescribedPilotBurns || showPrescribedNonPilotBurns ) {
    burnTicks <- pretty(ylim_burnTons)
    at <- burnTicks * (ylim_pm25[2]/ylim_burnTons[2])
    labels <- as.character(burnTicks)
    axis(side=4, at=at, labels=labels, las=1)
    # Top label for axis 2
    text(xhi, yhi*1.05, labels='Tons', pos=4, xpd=NA)
    text(xhi, yhi*1.02, labels='Accomplished', pos=4, xpd=NA)
  }
  
  # Satellite Detects
  if ( showBlueskyEvents ) {
    # ylo should always be a negative number
    text(xlo,ylo+1.5,'Satellite Detects',pos=4, col=col_events)
  }
  
  # Legend for pm25
  if ( show1Hr && show3Hr && showDaily) {
    legend('topleft', title="Smoke",
           c("Hourly (high values)",
             "Rolling mean (3hr)",
             "Daily mean"),
           col=c(col_1, col_3, col_daily),
           pch=c(pch_1, NA, NA),
           lwd=c(NA, lwd_3, lwd_daily))
  }
  
  # Legend for burns
  if ( showPrescribedPilotBurns || showPrescribedNonPilotBurns ) {
    legend('topright', title="Prescribed Burns",
           c("Non-24 hour",
             "24-hr"),
           fill=c(col_nonPilotBurns, col=col_pilotBurns),
           density=20, angle=c(45,-45))
  }
  
  # Reset margins
  par(mar=old_mar)
}

