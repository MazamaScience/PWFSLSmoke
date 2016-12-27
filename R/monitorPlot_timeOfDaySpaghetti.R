#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Time of Day Spaghetti Plot
#' @param ws_monitor ws_monitor object
#' @param monitorID id for a specific monitor in the ws_monitor object
#' @param tlim time limit for plot
#' @param ylim optional y limits for plot
#' @param aqiLines horizontal lines indicating AQI levels
#' @param shadedNight shade nights based on sunrise/sunset times of middle day in selected period
#' @param ... additional arguments to pass to lines()
#' @description Creates a spaghetti plot of PM2.5 levels by hour for one or more days. The average by hour over 
#' the period is also calculated and plotted as a thick red line.
#' @examples
#' \dontrun{
#' ws_monitor <- airsis_load(20150901, 20150930)
#' monitor <- ws_monitor$meta$monitorID[3]
#' monitorPlot_timeOfDaySpaghetti(ws_monitor, monitor,tlim=c(20150914,20150930))
#' }

monitorPlot_timeOfDaySpaghetti <- function(ws_monitor,
                                           monitorID=NULL,
                                           tlim=NULL,
                                           ylim=NULL,
                                           aqiLines=TRUE,
                                           shadedNight=TRUE,
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
      stop(paste0("ws_monitor object contains data for >1 monitor. Please specify a monitorID from: '",paste(ws_monitor$meta$monitorID,collapse="', '"),"'"))
    }
  }
  
  # NOTE:  Incomting insituTime is GMT
  
  # Insitu data requested
  pm25 <- ws_monitor$data[[monitorID]]
  utc <- ws_monitor$data$datetime
  index <- which(ws_monitor$meta$monitorID %in% monitorID)
  
  lat <- ws_monitor$meta$latitude[index]
  lon <- ws_monitor$meta$longitude[index]
  timezone <- ws_monitor$meta$timezone[index]
  
  localTime <- lubridate::with_tz(utc,timezone)
  day <- lubridate::date(localTime) # NOTE: this was previously using "day()", which could result in double counting data for days with same day of month in different months
  hour <- lubridate::hour(localTime)
  
  # Create a new dataframe for local use
  df <- data.frame(localTime,pm25,day,hour)
  
  # Time limit application
  if ( !is.null(tlim) ) {
    # TODO: add logic to check for tlim format
    timeMask <- localTime >= lubridate::ymd(tlim[1]) & localTime < lubridate::ymd(tlim[2])+lubridate::days(1)
    if ( sum(timeMask)==0 ) {
      PWFSLSmoke::monitorPlot_noData(ws_monitor)
      stop("No data contained within specified time limits, please try again.")
    }
    df <- df[timeMask,]
  }
  
  # Set y Limits
  if ( is.null(ylim) ) {
    ylim <- c(min(0,min(df$pm25,na.rm=TRUE)),max(df$pm25,na.rm=TRUE))
  }
  
  # Sunrise and Sunset times for shaded night
  uniqueDays <- unique(df$day)
  dayCount <- length(uniqueDays)
  middleDay <- df$localTime[which(df$day == uniqueDays[floor(median(seq(dayCount)*100)/100)])][1]
  print(paste('Sunrise/sunset times based on the middle of the period:',middleDay))
  
  coords <- matrix(c(lon, lat), nrow=1)
  sunrise <- maptools::sunriset(coords, middleDay, direction="sunrise", POSIXct.out=TRUE)[["time"]]
  sunset <- maptools::sunriset(coords, middleDay, direction="sunset", POSIXct.out=TRUE)[["time"]]
  
  sunrise <- lubridate::hour(sunrise)+lubridate::minute(sunrise)/60
  sunset <- lubridate::hour(sunset)+lubridate::minute(sunset)/60
  
  # Plotting ------------------------------------------------------------------
  
  # Blank plot to set up limits
  plot(df$pm25 ~ df$hour, col='transparent',
       xlab='', ylab='',
       ylim=ylim,
       axes=FALSE)
  
  # Shaded Night
  if ( shadedNight ) {
    
    # Left edge to first sunrise
    rect(par('usr')[1], ybottom=par('usr')[3],
         xright=sunrise, ytop=par('usr')[4],
         col=col_shadedNight, lwd=0)
    
    # Last sunset to right edge
    rect(xleft=sunset, ybottom=par('usr')[3],
         xright=par('usr')[2], ytop=par('usr')[4],
         col=col_shadedNight, lwd=0)
    
  }
  
  # Complete axes
  axis(1,at=seq(0,24,3))
  axis(2,las=1)
  mtext(expression(paste("PM"[2.5]*" (",mu,"g/m"^3*")")),2,line=2.5)
  mtext(paste0('Hour (local)'),1,line=3)
  mtext(bquote(paste('Daily PM'[2.5],' Values and ', .(dayCount),'-day Mean')),line=2,cex = 1.5)
  mtext(paste(strftime(df$localTime[1], '%b. %d - '),strftime(utils::tail(df$localTime,1), '%b. %d %Y')),line=.7,cex=1.5)
  
  # AQI Lines
  if ( aqiLines ) {
    abline(h=AQI$breaks_24[2:6], col=col_aqi, lwd=lwd_aqi)
  }

  # Lines for each day
  for (thisDay in uniqueDays) {
    dayDF <- df[df$day == thisDay,]    
    lines(dayDF$pm25 ~ dayDF$hour, col=col_day)
  }
  
  # Add mean line
  df %>% group_by(as.factor(hour)) %>%
    summarize(pm25=mean(pm25,na.rm=TRUE)) ->
    hourMeanDF
  
  lines(hourMeanDF$pm25 ~ seq(0,23,1), col=col_mean, lwd=lwd_mean, ...)
  
}