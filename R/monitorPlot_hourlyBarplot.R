#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Hourly Barplot
#' @param ws_monitor ws_monitor object
#' @param monitorID monitor ID for a specific monitor in the given ws_monitor object (optional
#' if only one monitor in the ws_monitor object)
#' @param tlim time limit for barplot
#' @param localTime logical specifying whether \code{tlim} is in UTC or local time
#' @param style named style specification ('AirFire')
#' @param title plot title
#' @param shadedNight add nighttime shading
#' @param grid include grid 'under' or 'over' the hour bars
#' @param gridCol grid color
#' @param gridLwd grid line width
#' @param gridLty grid line type
#' @param ... additional arguments to be passed to barplot()
#' @description Creates a bar plot showing hourly PM 2.5 values for a specific monitor in a ws_monitor object.
#' Colors are assigned to one of the following styles:
#' \itemize{
#' \item{\code{AirFire} -- hourly values colored by AQI 24-hour breaks}
#' }
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20150701, 20150710)
#' title <- "Hourly PM2.5 for Colleville, WA"
#' monitorPlot_hourlyBarplot(airnow, monitorID="530650004", title=title)
#' }

monitorPlot_hourlyBarplot <- function(ws_monitor,
                                      monitorID=NULL,
                                      tlim=NULL,
                                      localTime=TRUE,
                                      style='AirFire',
                                      title=NULL,
                                      shadedNight=TRUE,
                                      grid='over',
                                      gridCol='white',
                                      gridLwd=2,
                                      gridLty='dotted',
                                      ...) {
  
  # Style ---------------------------------------------------------------------
  
  shadedNightCol <- 'gray80'
  markerInterval <- 6
  
  # Data Preparation ----------------------------------------------------------
  
  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) ) {
    if ( nrow(ws_monitor$meta) == 1 ) {
      monitorID <- ws_monitor$meta$monitorID
    } else {
      stop(paste0("ws_monitor object contains data for >1 monitor. Please specify a monitorID from: '",
                  paste(ws_monitor$meta$monitorID,collapse="', '"),"'"))
    }
  }
  
  # When tlim is specified in whole days we should add hours to get the requsted full days
  if ( !is.null(tlim) ) {
    tlimStrings <- as.character(tlim)
    if ( stringr::str_length(tlimStrings)[1] == 8 ) {
      tlim[1] <- paste0(tlim[1],'00')
    }
    if ( stringr::str_length(tlimStrings)[2] == 8 ) {
      tlim[2] <- paste0(tlim[2],'23')
    }
  }
  
  # Assign timezone to pass to monitor_subset() below, or use UTC if localTime==FALSE
  if ( localTime ) {
    timezone <- ws_monitor$meta[monitorID,"timezone"]
  } else {
    timezone <- "UTC"
  }
  
  # Subset to a single monitor
  mon <- monitor_subset(ws_monitor, monitorIDs=monitorID, tlim=tlim, timezone=timezone)
  
  # Assign datetime based on timezone (i.e. local vs. UTC)
  datetime <- lubridate::with_tz(mon$data$datetime,timezone)
  
  # Assign lat/lon and localTimeZone for shadedNight
  lon <- mon$meta$longitude
  lat <- mon$meta$latitude
  localTimeZone <- mon$meta$timezone
  
  if ( localTime ) {
    localDateTime <- datetime
  } else {
    localDateTime <- lubridate::with_tz(mon$data$datetime,localTimeZone)
  }
  
  # Pull out monitoring data
  pm25 <- as.numeric(mon$data[,monitorID])
  
  # Plot command default arguments ---------------------------------------------
  
  argsList <- list(...)
  
  argsList$height <- pm25

  # TODO:  We will eventually need a couple of different choices for assigning colors based on
  # TODO:  hourly, nowcast, AQI, etc.
    
  # 'AirFire' colors use hourly values with 24 hour colors
  if ( style == 'AirFire' ) {
    aqiColors <- adjustcolor(AQI$colors, 0.5)
    argsList$col <- aqiColors[ .bincode(pm25, AQI$breaks_24, include.lowest=TRUE) ]
  } else if ( style == 'NOT_YET_IMPLEMENTED' ) {
    # modify pm25 with either nowcast, rolling mean or aqi
    # modify color breaks and levels as needed
  }
  
  # X axis labeling
  argsList$xlab <- ifelse('xlab' %in% names(argsList), argsList$xlab, "Date")
  if ( !('names.arg' %in% names(argsList)) ) {
    argsList$names.arg <- strftime(datetime, "%b %d", tz = timezone)
  }
  
  # NOTE:  For mathematical notation in R see:
  # NOTE:    http://vis.supstat.com/2013/04/mathematical-annotation-in-r/
  
  # Y axis labeling
  if ( !('ylab' %in% names(argsList)) ) {
    argsList$ylab <- expression(paste("PM"[2.5]*" (",mu,"g/m"^3*")"))
  }
  
  # Additional small tweaks
  argsList$las <- ifelse('las' %in% names(argsList), argsList$las, 1)
  
  # Default = no space between bars
  if ( !('space' %in% names(argsList)) ) {
    argsList$space <- 0
  }
  
  # Default = white borders
  if ( !('border' %in% names(argsList)) ) {
    argsList$border <- 'white'
  }
  
  ## Default bar width
  #if ( !('width' %in% names(argsList)) ) {
  #  argsList$width <- 1
  #}
  
  # Plotting ------------------------------------------------------------------
  
  # Create and modify second argsList for blank plot slate
  argsList2 <- argsList
  argsList2$col <- "white"
  argsList2$border <- NA
  argsList2$axes <- FALSE
  argsList2$names.arg <- NULL
  argsList2$ylab <- NA
  argsList2$xlab <- NA
  
  do.call(barplot, argsList2)
  argsList$add <- TRUE
  argsList2$add <- TRUE
  
  # Add horizontal grid lines (first if grid=='under')
  if ( grid == 'under' ) {
    abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
  }
  
  if ( shadedNight ) {
    if ( length(unique(mon$meta$timezone))>1 ) {
      stop("Can't do shaded night for more than one time zone!!")
    } else {
      timeInfo <- PWFSLSmoke::timeInfo(localDateTime, lon, lat, localTimeZone)

      # Set argsList2 for shadedNight plotting
      argsList2$height <- (!timeInfo$day)*max(pm25)
      argsList2$col <- shadedNightCol
      
      # Set width and spacing to ensure solid block of shaded night
      argsList2$space <- 0
      argsList2$width <- (1+argsList$space) # * argsList$width
      
      # Plot shadedNight from bottom of plot to top
      do.call(barplot,argsList2)
        
      # Replot in white over same height as colored bars so not visible when color bar opacity < 1
      argsList2$height <- pm25
      argsList2$col <- 'white'
      do.call(barplot,argsList2)
    }
  }
  
  # Plot the actual colored bars
  do.call(barplot, argsList)
  
  # Add horizontal grid lines on top if grid=='over'
  if ( grid == 'over' ) {
    abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
  }
  
  # Add a title if none was specified
  if ( is.null(title) ) {
    title <- expression(paste("Hourly Average PM"[2.5]))
  }
  title(title)
  
}
