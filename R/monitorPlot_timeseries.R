#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Timeseries Plot
#' @param ws_monitor ws_monitor object
#' @param monitorID monitor ID for one or more monitor in the ws_monitor object
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param localTime create plot in local time; applies to tlim as well
#' @param style AQI styling, one of \code{'Airfire| | '}... TODO: Fill in other options
#' @param shadedNight Shade nighttime values if TRUE, provided all monitors in the same time zone.
#' @param add A logical specifying whether you want to add the data points on top of an existing time series plot
#' @param gridPos position of grid lines either 'over', 'under' ('' for no grid lines)
#' @param gridCol grid line color
#' @param gridLwd grid line width
#' @param gridLty grid line type
#' @param dayLwd day marker line width
#' @param hourLwd hour marker line width
#' @param hourInterval interval for grid (max=12)
#' @param ... additional arguments to be passed to points()
#' @description Creates a time series plot of PM2.5 data from a ws_monitor object (see note below). Optional arguments
#' color code by AQI index, add shading to indicate nighttime, and adjust the time display (GMT vs. local).
#' @note Remember that a ws_monitor object can contain data from more than one monitor, and thus, this function may produce
#' a time series of data from multiple monitors. To plot a time series of an individual monitor's data, use the monitor_subset
#' function (see example below).
#' @examples
#' \dontrun{ 
#' airnow <- airnow_load(20150801, 20150831)
#' Roseburg <- monitor_subset(airnow, monitorIDs=c('410190002'))
#' monitorPlot_timeseries(Roseburg)
#' }

monitorPlot_timeseries <- function(ws_monitor,
                                   monitorID=NULL,
                                   tlim=NULL,
                                   localTime=TRUE,
                                   style='AQI',
                                   shadedNight=FALSE,
                                   add=FALSE,
                                   gridPos='',
                                   gridCol='black',
                                   gridLwd=1,
                                   gridLty='solid',
                                   dayLwd=0,
                                   hourLwd=0,
                                   hourInterval=6,
                                   ...) {
  
  # ----- Data Preparation ------------
  
  # Set monitorID if not passed in as argument
  if ( is.null(monitorID) ) {
    monitorID <- ws_monitor$meta$monitorID
  }
  
  # When tlim is specified in whole days we add hours to get the requsted full days
  if ( !is.null(tlim) ) {
    tlimStrings <- as.character(tlim)
    if ( stringr::str_length(tlimStrings)[1] == 8 ) {
      tlim[1] <- paste0(tlim[1],'00')
    }
    if ( stringr::str_length(tlimStrings)[2] == 8 ) {
      tlim[2] <- paste0(tlim[2],'23')
    }
  }
  
  # Identify timezone(s)
  timezone <- unique(ws_monitor$meta[monitorID,"timezone"])
  
  # Force timezone to UTC and disable shadedNight if >1 timezone in metadata for monitorIDs
  if ( length(timezone)>1 ) { # note that we will only enter this condition if localTime==TRUE
    if ( localTime ) {
      warning(">1 timezone in metadata for selected monitorIDs: Timezone (including tlim, if specified) forced to UTC")
      timezone <- "UTC"
    }
    if ( shadedNight ) {
      warning(">1 timezone in metadata for selected monitorIDs: Shaded Night disabled")
      shadedNight <- FALSE
    }    
  }
  
  # Set timezone to UTC if localTime==FALSE
  if ( !localTime ) {
    timezone <- "UTC"
  }
  
  # Subset ws_monitor object by monitorID(s) and tlim, using timezone from above
  mon <- monitor_subset(ws_monitor, monitorIDs=monitorID, tlim=tlim, timezone=timezone)
  # TODO: Consider fixing bug here where a monitor that has no valid data in the tlim (and thus will not be included in subset) will still force timezone='UTC' and shadedNight=FALSE

  # Pull out meta data and hour data
  meta <- mon$meta
  data <- mon$data
  
  # Pull out time data
  times <- lubridate::with_tz(data$datetime, tzone=timezone)
  
  # Set transparency based on number of points 
  dims <- dim(as.matrix(data[,-1]))
  num_na <- length(which(is.na(data[,-1])))
  size <- dims[1]*dims[2] - num_na
  transparency <- min(8/log(size), 1)
  
  # Cap hour interval
  if ( hourInterval>12 ) {
    warning("Hour interval capped at 12 hours")
    hourInterval <- min(hourInterval,12)
  }
  
  # ----- Args List ----------------------
  
  argsList <- list(...)
  
  argsList$x=times
  argsList$y=data[,2]
  
  # TODO: better ylim smarts
  # set range for plotting
  if ( !('ylim' %in% names(argsList)) ) {
    argsList$ylim <- max(data[,-1], na.rm=TRUE)
    argsList$ylim <- c(0, argsList$ylim*1.1)
  }
  
  if ( !('xlab' %in% names(argsList)) ) {
    if ( timezone=='UTC' ) {
      argsList$xlab <- 'Date and time (UTC)'
    } else {
      argsList$xlab <- 'Date and time (local)'
    }
  }
  
  if ( !('ylab' %in% names(argsList)) ) {
    argsList$ylab <- "PM2.5"
  }
  
  if ( !('main' %in% names(argsList)) ) {
    argsList$main <- "Hourly PM2.5"
  } 
  
  # ----- Args List for Blank ------------
  argsListBlank <- argsList
  
  argsListBlank$col <- 'transparent'
  argsListBlank$axes <- FALSE
  argsListBlank$main <- NULL
  
  # ----- Plotting -----------------------
  
  # Base plot for background
  if ( !add ) {
    
    # Create blank plot
    do.call(plot,argsListBlank)
    
    # Shaded Night
    if ( shadedNight ) {

      # Lat/lon for shadedNight
      lat <- mean(mon$meta$latitude)
      lon <- mean(mon$meta$longitude)
      timeInfo <- PWFSLSmoke::timeInfo(times, lon, lat, timezone)
      addShadedNights(timeInfo)
    
      # # OPTION: Loop for each location...may need to increase tranparency based on # of monitors
      # for (monitor in mon$meta$monitorID) {
      #   lat <- mon$meta[monitor,"latitude"]
      #   lon <- mon$meta[monitor,"longitude"]
      #   timeInfo <- PWFSLSmoke::timeInfo(times, lon, lat, timezone)
      #   addShadedNights(timeInfo)
      # }
      
    }
    
    # Add vertical lines to denote days and/or hour breaks
    hour_indices <- which(as.numeric(strftime(times,format="%H",tz=timezone)) %% hourInterval == 0)
    day_indices <- which(as.numeric(strftime(times,format="%H",tz=timezone)) %% 24 == 0)
    abline(v=times[hour_indices], lwd=hourLwd) # at beginning of hour
    abline(v=times[day_indices], lwd=dayLwd) # at beginning of day
    
    # Add horizontal grid lines (before points if grid=='under')
    if ( gridPos == 'under' ) {
      abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
    }
    
    # Put a box around the plot area
    box()
    
    # Add axes if we are not adding points on top of an existing plot
    axis(2, las=1)
    
    # TODO: better x axis smarts, e.g. keep from saying "Monday, Tuesday" etc...
    axis.POSIXct(1, times)
    
  }
  
  # TODO: Add more style options
  
  # choose AQI breaks 
  if ( style == 'AQI' ) {
    
    breaks <- AQI$breaks_24
    
    for (id in meta$monitorID) {
      # set style options 
      argsList$y <- data[[id]] # same as data[,id]
      levels <- .bincode(argsList$y, breaks)
      argsList$col <- AQI$colors[levels]
      argsList$col <- adjustcolor(argsList$col, alpha.f=transparency)
      argsList$cex <- argsList$y / 200 + .3
      argsList$cex <- pmin(argsList$cex,2)
      
      # Add the points
      do.call(points,argsList)
      
    }
  } else {
    
    for (id in meta$monitorID) {
      # set style options 
      argsList$y <- data[[id]] # same as data[,id]
      
      do.call(points,argsList)
    }
  }
  
  # Add horizontal grid lines (on top of points if grid=='over')
  if ( gridPos == 'over' ) {
    abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
  }

}
