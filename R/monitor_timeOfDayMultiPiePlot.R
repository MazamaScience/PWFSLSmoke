#' @export
#' @import graphics
#' @title Time of Day Multi Pie Plot
#' @param ws_monitor a ws_monitor object
#' @param monitorID id for a specific monitor in the ws_monitor
#' @description Pie plot that shows 9 PM 2.5 levels based on time of day. 
#' @examples
#' \dontrun{
#' ws_monitor <- wrcc_load(20150801, 20150810)
#' monitor <- ws_monitor$meta$monitorID[1]
#' monitor_timeOfDayMultiPiePlot(ws_monitor, monitor)
#' }

monitor_timeOfDayMultiPiePlot <- function(ws_monitor, monitorID=NULL) {
  
  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) && nrow(ws_monitor$meta) == 1 ) {
    monitorID <- ws_monitor$meta$monitorID[1]
  }
  
  # Plot Style
  
  aqiColors <- adjustcolor(AQI$colors, 0.5)

  cex_pie = 1.0
  lwd_pie = 3
  col_pieLines = 'white'
  labelRadius = 0.80
  col_nighttime = adjustcolor('black',0.2)
  col_missing <- 'gray80'
  
  # Data Preparation ----------------------------------------------------------
  
  # NOTE:  Incomting insituTime is GMT
  
  # Insitu data requested 
  pm25 <- ws_monitor$data[[monitorID]]
  GMTTime <- ws_monitor$data$datetime 
  index <- which(ws_monitor$meta$monitorID %in% monitorID)
  
  localTime <- lubridate::with_tz(GMTTime,ws_monitor$meta$timezone[index])
  day <- lubridate::day(localTime)
  hour <- lubridate::hour(localTime)
  
  dayChunk <- as.factor(.bincode(hour,breaks=seq(0,24,3),include.lowest=TRUE))
  
  # Create a new dataframe for local use
  df <- data.frame(localTime,pm25,day,hour,dayChunk)
  
  # Set up to do a plot with the last four days 
  lastDays <- rev(unique(day))[1:9]
  lastDays <- c(rev(lastDays[1:3]),
                rev(lastDays[4:6]),
                rev(lastDays[7:9]))
  
  # I want the oldest plot in the lower left and the newest in the upper right
  layout(matrix(c(7,8,9,4,5,6,1,2,3),nrow=3,byrow=TRUE))
  
  # Modify the margins
  par(mar=c(3,3,3,3)+.1)
  
  # Simple "danger-pie" plot for each day
  for (thisDay in lastDays) {
    
    dayDF <- df[df$day == thisDay,]
    
    dayDF %>% group_by(dayChunk) %>%
      summarize(pm25=mean(pm25,na.rm=TRUE)) ->
      dayChunkMean
    
    
    # Plotting ------------------------------------------------------------------
    
    # Colors come from pm25Daily values
    cols <- aqiColors[ .bincode(dayChunkMean$pm25, AQI$breaks_24, include.lowest=TRUE) ]
    
    # Handle incomplete days
    if (length(cols) < 8) {
      cols <- c(cols,rep(NA,(8-length(cols))))
    }
    
    # Replace NA with gray
    cols[is.na(cols)] <- col_missing
    
    x <- rep(1,length(levels(dayChunk)))
    
    pie(x,
        clockwise=TRUE, init.angle=-90,
        labels=NA,
        col=cols, border=cols)
    
    title(paste0(strftime(dayDF$localTime[1],"%b %d")),line=0)
    
    abline(h=0,col=col_pieLines,lwd=lwd_pie)
    abline(v=0,col=col_pieLines,lwd=lwd_pie)
    abline(0,1,col=col_pieLines,lwd=lwd_pie)
    abline(0,-1,col=col_pieLines,lwd=lwd_pie)
    
    text(0,-labelRadius*0.7,'Midnight',pos=1)
    text(-labelRadius,-0,'6 am',pos=2)
    text(0,labelRadius*0.7,'Noon',pos=3)
    text(labelRadius,-0,'6 pm',pos=4)
    
    
    ###rect(-labelRadius,-1,labelRadius,0, col=col_nighttime, border=col_nighttime)
    rect(-1,-1,1,-0.05, col=col_nighttime, border=col_nighttime)
    
  }
  
  # Return to default settings
  layout(1)
  par(mar=c(5,4,4,2)+.1)
  
}

