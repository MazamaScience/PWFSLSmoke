#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Hourly Cumulative Plot
#' @param ws_monitor \emph{ws_monitor} object
#' @param monitorID id for a specific monitor in the ws_monitor object
#' @param ylim y limits for the plot
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param shadedNight add nighttime shading based on of middle day in selected period
#' @param aqiLines diagonal lines indicating AQI levels
#' @param title plot title
#' @param ... additional arguments to pass to \code{lines()}
#' @description Creates a spaghetti plot of PM2.5 levels by hour for one or more days. The average by hour over
#' the period is also calculated and plotted as a thick red line.

monitorPlot_hourlyCumulative <- function(ws_monitor,
                                         monitorID=NULL,
                                         tlim=NULL,
                                         ylim=NULL,
                                         aqiLines=TRUE,
                                         shadedNight=TRUE,
                                         title=NULL,
                                         ...) {

  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) {
    stop("ws_monitor object contains zero monitors")
  }

  # Plot Style ----------------------------------------------------------------

  # lwd_day <- 2
  # lwd_mean <- 3
  lwd_aqi <- 6

  # line colors; to futz with transparency later
  # col_day <- adjustcolor('gray50',.5)
  # col_mean <- 'black'
  col_aqi <- adjustcolor(AQI$colors[2:6], 0.6)
  col_shadedNight <- adjustcolor('black',0.1)

  # ----- Data Preparation ----------------------------------------------------

  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) ) {
    if ( nrow(ws_monitor$meta) == 1 ) {
      monitorID <- ws_monitor$meta$monitorID[1]
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

  # Subset to a single monitor and specified time limits
  timezone <- as.character(ws_monitor$meta[monitorID,'timezone'])
  mon <- monitor_subset(ws_monitor, monitorIDs=monitorID, tlim=tlim, timezone=timezone)

  # X axis is time
  x <- mon$data$datetime

  # Replace NAs with zero in order to have cumsum work
  dataBrick <- as.matrix(mon$data[,-1])
  dataBrick[is.na(dataBrick)] <- 0
  y <- cumsum(dataBrick[,1])

  # Create diagonal AQI lines
  count <- length(x)
  aqi_y2 <- cumsum(rep(AQI$breaks_24[2], times=count))
  aqi_y3 <- cumsum(rep(AQI$breaks_24[3], times=count))
  aqi_y4 <- cumsum(rep(AQI$breaks_24[4], times=count))
  aqi_y5 <- cumsum(rep(AQI$breaks_24[5], times=count))
  aqi_y6 <- cumsum(rep(AQI$breaks_24[6], times=count))


  # ----- Plotting ------------------------------------------------------------

  # Any changes to overall graphical paramter, eg. mar,

  # old_mar <- c(5.1,4.1,4.1,2.1)
  # par(mar=c(5.1,7.1,4.1,7.1))

  # Probalby set up a blank plot with axes

  # Blank plot to get the scaling
  plot(x, y, type='s', col='transparent', axes=FALSE,
       xlab='Local Time',
       ylab='PM2.5 (ug)')

  # Shaded Night
  if ( shadedNight ) {

    # Create local time
    localTime <- lubridate::with_tz(mon$data$datetime, timezone)

    # Get the sunrise/sunset information
    ti <- timeInfo(localTime, longitude=mon$meta$longitude, latitude=mon$meta$latitude, timezone=timezone)

    addShadedNight(ti)
    #
    # # Extract the middle row
    # ti <- ti[round(nrow(ti)/2),]
    #
    # # Get sunrise and sunset in units of hours
    # sunrise <- lubridate::hour(ti$sunrise) + lubridate::minute(ti$sunrise)/60
    # sunset <- lubridate::hour(ti$sunset) + lubridate::minute(ti$sunset)/60
    #
    # # Left edge to sunrise
    # rect(par('usr')[1], ybottom=par('usr')[3],
    #      xright=sunrise, ytop=par('usr')[4],
    #      col=col_shadedNight, lwd=0)
    #
    # # Sunset to right edge
    # rect(xleft=sunset, ybottom=par('usr')[3],
    #      xright=par('usr')[2], ytop=par('usr')[4],
    #      col=col_shadedNight, lwd=0)

  }

  # Add AQI lines
  if ( aqiLines ) {
    points(x, aqi_y2, type='l', lwd=2, col=AQI$colors[2])
    points(x, aqi_y3, type='l', lwd=2, col=AQI$colors[3])
    points(x, aqi_y4, type='l', lwd=2, col=AQI$colors[4])
    points(x, aqi_y5, type='l', lwd=2, col=AQI$colors[5])
    points(x, aqi_y6, type='l', lwd=2, col=AQI$colors[6])
  }

  # Add cumulative PM2.5 on top
  points(x, y, type='s')  # TODO:  use '...'


  # Sometimes you want to create custom axes if "axes=FALSE" above
  # Add box and Y-axis
  box()
  axis(2) # Lots of room to cutomize if needed
  # TODO: better x axis smarts, e.g. keep from saying "Monday, Tuesday" etc...
  axis.POSIXct(1, localTime)


  # ...

  # Annotations
  title(paste0(mon$meta$siteName, ' -- Cumulative PM2.5 Exposure'))

  # Labels scattered around

  # Lines and rectangles (if they need to go underneath, plot them first)

  # Add a second Y axis if needed

  # Add a legend

  # Add another

  # Add the final things

  # Reset margins and other graphical parameters
  # par(mar=old_mar)

}

