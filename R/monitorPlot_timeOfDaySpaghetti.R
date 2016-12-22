#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Time of Day Spaghetti Plot
#' @param ws_monitor ws_monitor object
#' @param monitorID id for a specific monitor in the ws_monitor object
#' @description Creates a spaghetti plot of PM2.5 levels by hour for one or more days. The average by hour over 
#' the period is also calculated and plotted as a thick black line.
#' @examples
#' \dontrun{
#' ws_monitor <- wrcc_load(20150801, 20150810)
#' monitor <- ws_monitor$meta$monitorID[1]
#' monitor_timeOfDaySpaghettiPlot(ws_monitor, monitor)
#' }

monitor_timeOfDaySpaghettiPlot <- function(ws_monitor, monitorID=NULL) {
  
  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) && nrow(ws_monitor$meta) == 1 ) {
    monitorID <- ws_monitor$meta$monitorID[1]
  }
  
  # Plot Style ----------------------------------------------------------------
  
  lwd_day <- 2
  lwd_mean <- 3
  col_day <- adjustcolor('black',0.05)
  col_mean <- 'black'
  
  # Data Preparation ----------------------------------------------------------
  
  # NOTE:  Incomting insituTime is GMT
  
  # Insitu data requested 
  pm25 <- ws_monitor$data[[monitorID]]
  GMTTime <- ws_monitor$data$datetime 
  index <- which(ws_monitor$meta$monitorID %in% monitorID)
  
  localTime <- lubridate::with_tz(GMTTime,ws_monitor$meta$timezone[index])
  day <- lubridate::day(localTime)
  hour <- lubridate::hour(localTime)
  
  # Create a new dataframe for local use
  df <- data.frame(localTime,pm25,day,hour)
  
  
  # Plotting ------------------------------------------------------------------
  
  # Blank plot to set up limits
  plot(df$pm25 ~ df$hour, col='transparent',
       xlab='', ylab='',
       axes=FALSE)
  axis(1,at=seq(0,24,3))
  axis(2,las=1)
  mtext('PM 2.5',2,line=3)
  mtext(paste0('Hour'),1,line=3)
  title(paste0('Daily PM2.5 values and 10-day Mean\n', strftime(GMTTime[1], '%b. %d - '),
               strftime(utils::tail(GMTTime, 1), '%b. %d %Y')))
  
  # Simple line plot for each day
  lastDays <- unique(day)
  for ( dayIndex in seq(length(lastDays)) ) {
    
    thisDay <- lastDays[dayIndex]
    dayDF <- df[df$day == thisDay,]
    
    for (i in seq(dayIndex)) lines(dayDF$pm25 ~ dayDF$hour, col=col_day)
    
  }
  
  # Add mean line
  df %>% group_by(as.factor(hour)) %>%
    summarize(pm25=mean(pm25,na.rm=TRUE)) ->
    hourMeanDF
  
  lines(hourMeanDF$pm25 ~ seq(0,23,1), col=col_mean, lwd=lwd_mean)
  
  # Return to default setting
  par(mar=c(5,4,4,2)+.1)
  
}

