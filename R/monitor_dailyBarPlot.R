#' @export
#' @title dailyBarPlot
#' @param ws_monitor ws_monitor object
#' @param monitorID id for a specific monitor in the ws_monitor
#' @description Pie plot that shows PM 2.5 levels based on time of day. 
#' @examples
#' \dontrun{
#' ws_monitor <- wrcc_load(20150801, 20150802)
#' monitor <- ws_monitor$meta$monitorID[1]
#' monitor_timeOfDayPlot(ws_monitor, monitor)
#' }

monitor_dailyBarPlot <- function(ws_monitor, monitorID) {
  # Data Preparation ----------------------------------------------------------
  
  # NOTE:  Incomting insituTime is GMT
  
  # Insitu data requested 
  pm25 <- ws_monitor$data[[monitorID]]
  GMTTime <- ws_monitor$data$datetime 
  index <- which(ws_monitor$meta$monitorID %in% monitorID) 
  stationMetadata <- ws_monitor$meta
  
  # TODO:  When AirNow timezones are correct and always available we can do this
  
  # NOTE:  We will functions from the lubridate package to assign an integer
  # NOTE:  dayNum to each date. These need to be aligned so that each day begins
  # NOTE:  at midnight in the local timezone.  We can then use functionality
  # NOTE:  from the dplyr package to group data by dayNum and then summarize
  # NOTE:  to get the average time and average value for each day.
  
  # Convert to local time
  localTime <- lubridate::with_tz(GMTTime, stationMetadata$timezone)
  
  # Create a vector of Julian day numbers
  dayNum <- lubridate::yday(localTime)
  
  # Put all required elements into a new dataframe
  df <- data.frame(localTime, pm25, dayNum)
  
  # Create a new dataframe with daily means
  df %>% 
    group_by(dayNum) %>%
    summarize(localTime=mean(localTime,na.rm=TRUE), pm25=mean(pm25,na.rm=TRUE)) ->
    dfMean
  
  # Full days will have a local time mean at 11:30:00 so we can find full days like this
  fullDayMask <- lubridate::hour(dfMean$localTime) == 11
  
  # Subset to only have full days
  dfMean <- dfMean[fullDayMask,]
  
  
  # Plotting ------------------------------------------------------------------
  
  # Latest aqiBreaks from http://www.arb.ca.gov/carpa/toolkit/data-to-mes/wildfire-smoke-guide.pdf
  # NOTE:  The low end of each break category is used as the breakpoint.
  # Latest aqiColors from http://aqicn.org/faq/2013-09-09/revised-pm25-aqi-breakpoints/
  aqiBreaks_24  <- c(0, 12, 35.5, 55.5, 150.5, 250.5, 10000)
  
  aqiColors <- c("#009966","#FFDE33","#FF9933","#CC0033","#660099","#730023")
  aqiColors <- adjustcolor(aqiColors, 0.5)
  aqiNames <- c('good','moderate','USG','unhealthy','very unhealthy','extreme')
  
  # Colors come from pm25Daily values
  cols <- aqiColors[ .bincode(dfMean$pm25, aqiBreaks_24, include.lowest=TRUE) ]
  
  # Create time labels we can use for the X axis
  dayLabels <- strftime(dfMean$localTime, "%b %d")
  
  # Plot
  barplot(dfMean$pm25, las=1,
          col=cols,
          names.arg=dayLabels,
          ylab="PM 2.5")
  
  grid(nx=NA, ny=NULL, col='white', lwd=2)
  
  # Annotations
  title('Daily Average PM2.5')
  
}