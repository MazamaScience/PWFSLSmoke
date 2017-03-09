#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Time of Day Spaghetti Plot
#' @param ws_monitor ws_monitor object
#' @param monitorID id for a specific monitor in the ws_monitor object
#' @param ylim y limits for the plot
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param shadedNight add nighttime shading based on of middle day in selected period
#' @param aqiLines horizontal lines indicating AQI levels
#' @param title plot title
#' @param ... additional arguments to pass to lines()
#' @description Creates a spaghetti plot of PM2.5 levels by hour for one or more days. The average by hour over 
#' the period is also calculated and plotted as a thick red line.
#' @examples
#' monitorPlot_timeOfDaySpaghetti(CarmelValley, tlim=c(20160801,20160809))

monitorPlot_timeOfDaySpaghetti <- function(ws_monitor,
                                           monitorID=NULL,
                                           tlim=NULL,
                                           ylim=NULL,
                                           aqiLines=TRUE,
                                           shadedNight=TRUE,
                                           title=NULL,
                                           ...) {
  
  # Plot Style ----------------------------------------------------------------
  
  lwd_day <- 2
  lwd_mean <- 3
  lwd_aqi <- 6
  
  # line colors; to futz with transparency later
  col_day <- adjustcolor('gray50',.5)
  col_mean <- 'black'
  col_aqi <- adjustcolor(AQI$colors[2:6], 0.6)
  col_shadedNight <- adjustcolor('black',0.1)
  
  # Data Preparation ----------------------------------------------------------
  
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
  
  # Insitu data requested
  pm25 <- mon$data[[monitorID]]

  # Create local time dates and hours
  localTime <- lubridate::with_tz(mon$data$datetime, timezone)
  date <- lubridate::date(localTime)
  hour <- lubridate::hour(localTime)
  
  # Create a new dataframe with columns we can use to uniquely identify separate days
  df <- data.frame(localTime,pm25,date,hour)
  uniqueDays <- unique(df$date)
  dayCount <- length(uniqueDays)
  
  # Plot command default arguments --------------------------------------------
  
  # NOTE:  The argsList will only be used for the mean line. Here we remove arguments
  # NOTE:  we need for the initial plot and add some defaults
  
  argsList <- list(...)
  
  # Pull out 'xlab' for use in the initial blank plot
  if ( 'xlab' %in% names(argsList) ) {
    xlab <- argsList$xlab
    argsList$xlab <- NULL
  } else {
    xlab <- paste0('Hour (local time)')
  }

  # Pull out 'ylab' for use in the initial blank plot
  if ( 'ylab' %in% names(argsList) ) {
    ylab <- argsList$ylab
    argsList$ylab <- NULL
  } else {
    ylab <- expression(paste("PM"[2.5]*" (",mu,"g/m"^3*")"))
  }
  
  # Default to the Y axis using horizontal tick labels
  las <- ifelse('las' %in% names(argsList), argsList$las, 1)
  
  # Add default color and linewidth for the mean line
  argsList$col <- ifelse('col' %in% names(argsList), argsList$col, col_mean)
  argsList$lwd <- ifelse('lwd' %in% names(argsList), argsList$lwd, lwd_mean)
  
  # Plotting ------------------------------------------------------------------
  
  # Blank plot to set up limits
  plot(df$pm25 ~ df$hour, col='transparent',
       xlab=xlab, ylab=ylab,
       ylim=ylim,
       axes=FALSE)

  # Add axes
  box()
  axis(1, at=seq(0,24,3))
  axis(2, las=las)

  # Add a title
  if ( is.null(title) ) {
    mtext(bquote(paste('Daily PM'[2.5],' Values and ', .(dayCount),'-day Mean')), line=2, cex=1.5)
    mtext(paste(strftime(df$localTime[1], '%b. %d - '), strftime(utils::tail(df$localTime,1), '%b. %d %Y')), line=.5, cex=1.5)
  } else {
    title(title)
  }
  
  # Shaded Night
  if ( shadedNight ) {
    
    # Get the sunrise/sunset information
    ti <- timeInfo(localTime, lon=mon$meta$longitude, lat=mon$meta$latitude, timezone=timezone)
    
    # Extract the middle row
    ti <- ti[round(nrow(ti)/2),]
    
    # Get sunrise and sunset in units of hours
    sunrise <- lubridate::hour(ti$sunrise) + lubridate::minute(ti$sunrise)/60
    sunset <- lubridate::hour(ti$sunset) + lubridate::minute(ti$sunset)/60
    
    # Left edge to sunrise
    rect(par('usr')[1], ybottom=par('usr')[3],
         xright=sunrise, ytop=par('usr')[4],
         col=col_shadedNight, lwd=0)
    
    # Sunset to right edge
    rect(xleft=sunset, ybottom=par('usr')[3],
         xright=par('usr')[2], ytop=par('usr')[4],
         col=col_shadedNight, lwd=0)
    
  }
  
  # AQI Lines
  if ( aqiLines ) {
    abline(h=AQI$breaks_24[2:6], col=col_aqi, lwd=lwd_aqi)
  }
  
  # Lines for each day
  for (thisDay in uniqueDays) {
    dayDF <- df[df$date == thisDay,]    
    lines(dayDF$pm25 ~ dayDF$hour, col=col_day)
  }
  
  # Add mean line with do.call()
  df %>% group_by(as.factor(hour)) %>%
    summarize(pm25=mean(pm25,na.rm=TRUE)) ->
    hourMeanDF
  
  argsList$x <- seq(0,23,1)
  argsList$y <- hourMeanDF$pm25
  
  do.call(lines, argsList)
  
}
