#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Hourly Barplot
#' @param ws_monitor \emph{ws_monitor} object
#' @param monitorID monitor ID for a specific monitor in \code{ws_monitor} (optional
#' if \code{ws_monitor} only has one monitor)
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param localTime logical specifying whether \code{tlim} is in local time or UTC
#' @param style named style specification ('AirFire')
#' @param shadedNight add nighttime shading
#' @param gridPos position of grid lines either 'over', 'under' ('' for no grid lines)
#' @param gridCol grid color
#' @param gridLwd grid line width
#' @param gridLty grid line type
#' @param labels_x_nudge nudge x labels to the left
#' @param labels_y_nudge nudge y labels down
#' @param dayCol day boundary color
#' @param dayLwd day boundary line width (set to 0 to omit day lines)
#' @param dayLty day boundary type
#' @param hourCol hour boundary color
#' @param hourLwd hour boundary line width (set to 0 to omit  hour lines)
#' @param hourLty hour boundary type
#' @param hourInterval interval for hour boundary lines
#' @param ... additional arguments to be passed to \code{barplot(})
#' @description Creates a bar plot showing hourly PM 2.5 values for a specific monitor in a \emph{ws_monitor} object.
#' Colors are assigned to one of the following styles:
#' \itemize{
#' \item{\code{AQI} -- hourly values colored with AQI colors using AQI 24-hour breaks}
#' \item{\code{brownScaleAQI} -- hourly values colored with brownscale colors using AQI 24-hour breaks}
#' \item{\code{grayScaleAQI} -- hourly values colored grayscale colors using AQI 24-hour breaks}
#' }
#' @details The \code{labels_x_nudge} and \code{labels_y_nudge} can be used to
#' tweak the date labeling. Units used are the same as those in the plot.
#' @examples
#' C_V <- monitor_subset(Carmel_Valley, tlim=c(2016080800,2016081023),
#'                       timezone='America/Los_Angeles')
#' monitor_hourlyBarplot(C_V, main='1-Hourly Average PM2.5',
#'                       labels_x_nudge=1, labels_y_nudge=0)

monitor_hourlyBarplot <- function(ws_monitor,
                                  monitorID = NULL,
                                  tlim = NULL,
                                  localTime = TRUE,
                                  style = 'AQI',
                                  shadedNight = TRUE,
                                  gridPos = '',
                                  gridCol = 'black',
                                  gridLwd = 0.5,
                                  gridLty = 'solid',
                                  labels_x_nudge = 0,
                                  labels_y_nudge = 0,
                                  dayCol = 'black',
                                  dayLwd = 2,
                                  dayLty = 'solid',
                                  hourCol = 'black',
                                  hourLwd = 1,
                                  hourLty = 'solid',
                                  hourInterval = 6,
                                  ...) {

  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) {
    stop("ws_monitor object contains zero monitors")
  }

  # Style ---------------------------------------------------------------------

  shadedNightCol <- 'gray90'

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
  datetime <- lubridate::with_tz(mon$data$datetime, timezone)

  # Assign lat/lon and localTimeZone for shadedNight
  lon <- mon$meta$longitude
  lat <- mon$meta$latitude
  localTimeZone <- mon$meta$timezone

  if ( localTime ) {
    localDatetime <- datetime
  } else {
    localDatetime <- lubridate::with_tz(mon$data$datetime, localTimeZone)
  }

  # Pull out monitoring data
  pm25 <- as.numeric(mon$data[,monitorID])

  # Plot command default arguments ---------------------------------------------

  argsList <- list(...)

  argsList$height <- pm25

  # TODO:  We will eventually need a couple of different choices for assigning colors based on
  # TODO:  hourly, nowcast, AQI, etc.

  # TODO:  cleanup unused options and use aqiColors() throughout?

  # 'AirFire' colors use hourly values with 24 hour colors
  if ( style == 'AQI' ) {
    argsList$col <- aqiColors(pm25)
  } else if ( style == 'grayscaleAQI' ) {
    # NOTE:  Greyscale coded
    aqiColors <- RColorBrewer::brewer.pal(6,'Greys')
    argsList$col <- aqiColors[ .bincode(pm25, AQI$breaks_24, include.lowest=TRUE) ]
  } else if ( style == 'brownscaleAQI' ) {
    # NOTE:  Brownscale coded
    aqiColors <- rev(RColorBrewer::brewer.pal(11,'BrBG')[1:6])
    argsList$col <- aqiColors[ .bincode(pm25, AQI$breaks_24, include.lowest=TRUE) ]
  } else if ( style == 'aqiLevel' ) {
    # NOTE:  Special use case for creating mini barplots where the ws_monitor object
    # NOTE:  has been modified so that the pm25 value is just the AQI index level (1:5)
    aqiColors <- AQI$colors
    argsList$col <- aqiColors[ pm25 ]
  } else if ( style == 'NOT_YET_IMPLEMENTED' ) {
    # modify pm25 with either nowcast, rolling mean or aqi
    # modify color breaks and levels as needed
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

  # Title
  if ( !('main' %in% names(argsList)) ) {
    argsList$main <- expression(paste("Hourly Average PM"[2.5]))
  }

  # Explicitly declare defaults for use in creating the x axis
  argsList$axes <- ifelse('axes' %in% names(argsList), argsList$axes, TRUE)
  argsList$space <- ifelse('space' %in% names(argsList), argsList$space, 0.2)
  argsList$cex.names <- ifelse('cex.names' %in% names(argsList), argsList$cex.names, par("cex.axis"))

  # Plotting ------------------------------------------------------------------

  # Create and modify second argsList for blank plot slate
  argsList2 <- argsList
  argsList2$col <- "white"
  argsList2$border <- NA
  argsList2$axes <- FALSE
  argsList2$names.arg <- NULL
  argsList2$ylab <- NA
  argsList2$xlab <- NA
  argsList2$main <- NA

  if ( shadedNight ) {
    if ( length(unique(mon$meta$timezone))>1 ) {
      stop("Can't do shaded night for more than one time zone!!")
    } else {
      timeInfo <- PWFSLSmoke::timeInfo(localDatetime, lon, lat, localTimeZone)

      # Set argsList2 for shadedNight plotting
      maxHeight <- ifelse( 'ylim' %in% names(argsList2), argsList2$ylim[2], max(pm25, na.rm=TRUE) )
      argsList2$height <- (!timeInfo$day) * maxHeight
      argsList2$col <- shadedNightCol

      # Set shadedNight width and spacing to ensure solid block of color (may remove these)
      argsList2$space <- 0
      argsList2$width <- (1+argsList$space) # * argsList$width

      # Plot shadedNight from bottom of plot to top
      do.call(barplot,argsList2)
    }
  } else {
    do.call(barplot,argsList2)
  }

  # Add horizontal grid lines (first if grid=='under')
  if ( gridPos == 'under' ) {
    abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
  }

  # Plot the actual colored bars
  argsList$add <- TRUE
  do.call(barplot, argsList)

  # Add vertical lines to denote days and/or hour breaks
  hour_indices <- which(as.numeric(strftime(datetime,format="%H",tz=timezone)) %% hourInterval == 0)
  day_indices <- which(as.numeric(strftime(datetime,format="%H",tz=timezone)) %% 24 == 0)
  abline(v=hour_indices-1, col=hourCol, lwd=hourLwd, lty=hourLty) # at beginning of hour
  abline(v=day_indices-1, col=dayCol, lwd=dayLwd, lty=dayLty) # at beginning of day

  # Add default X axis
  if ( argsList$axes && !('names.arg' %in% names(argsList)) ) {

    # Days
    if ( dayLwd > 0 ) {
      labels_x <- day_indices-1
      labels_y <- -0.08 * (par('usr')[4] - par('usr')[3])
      labels <- strftime(datetime, "%b %d", tz=timezone)[day_indices]
      text(labels_x - labels_x_nudge, labels_y - labels_y_nudge, labels, srt=45, cex=argsList$cex.names, xpd=NA)
      axis(1, at=labels_x, labels=FALSE, lwd=0, lwd.ticks=dayLwd)
    }

    if ( hourLwd > 0 ) {
      # Hours
      labels_x <- hour_indices-1
      labels_y <- -0.03 * (par('usr')[4] - par('usr')[3])
      if ( hourInterval == 12 ) {
        labels <- c('00','12')
      } else if (hourInterval == 6) {
        labels <- c('00','6a','12','6p')
      } else if (hourInterval == 3) {
        labels <- c('00','3a','6a','9a','12','3p','6p','9p')
      } else {
        labels <- seq(0,23,hourInterval)
      }
      if ( dayLwd > 0 ) { labels[1] <- '' }
      text(labels_x, labels_y, labels, srt=00, cex=argsList$cex.names*0.6, xpd=NA)

    }

  }

  # Add horizontal grid lines on top if grid=='over'
  if ( gridPos == 'over' ) {
    abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
  }

}
