#' @export
#' @title timeOfDayPlot
#' @param ws_monitor a ws_monitor object
#' @param monitorID a id for a specific monitor in the ws_monitor
#' @description Pie plot that shows PM 2.5 levels based on time of day. 
#' @examples
#' \dontrun{
#' ws_monitor <- wrcc_load(20150801, 20150802)
#' monitor <- ws_monitor$meta$monitorID[1]
#' monitor_timeOfDayPlot(ws_monitor, monitor)
#' }

library(dplyr)

monitor_timeOfDayPlot <- function(ws_monitor, monitorID) {
  
  # Plot Style
  
  # Latest aqiBreaks from http://www.arb.ca.gov/carpa/toolkit/data-to-mes/wildfire-smoke-guide.pdf
  # NOTE:  The low end of each break category is used as the breakpoint.
  # Latest aqiColors from http://aqicn.org/faq/2013-09-09/revised-pm25-aqi-breakpoints/
  aqiBreaks_24  <- c(0, 12, 35.5, 55.5, 150.5, 250.5, 10000)
  
  aqiColors <- c("#009966","#FFDE33","#FF9933","#CC0033","#660099","#730023")
  aqiColors <- adjustcolor(aqiColors, 0.5)
  aqiNames <- c('good','moderate','USG','unhealthy','very unhealthy','extreme')
  
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
  lastDays <- rev( rev(unique(day))[1:4] )
  
  # I want the oldest plot in the lower left and the newest in the upper right
  layout(matrix(c(3,4,1,2),nrow=2,byrow=TRUE))
  
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
    cols <- aqiColors[ .bincode(dayChunkMean$pm25, aqiBreaks_24, include.lowest=TRUE) ]
    
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
    
    text(0,-labelRadius,'Midnight',pos=1)
    text(-labelRadius,-0,'6 am',pos=2)
    text(0,labelRadius,'Noon',pos=3)
    text(labelRadius,-0,'6 pm',pos=4)
    
    
    ###rect(-labelRadius,-1,labelRadius,0, col=col_nighttime, border=col_nighttime)
    rect(-1,-1,1,-0.05, col=col_nighttime, border=col_nighttime)
    
  }
  
  # Return to default setting
  par(mar=c(5,4,4,2)+.1)
  
}

