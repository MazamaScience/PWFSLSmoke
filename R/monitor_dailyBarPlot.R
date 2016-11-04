#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Daily Bar Plot
#' @param ws_monitor ws_monitor object
#' @param monitorID monitor ID for a specific monitor in the given ws_monitor object (optional
#' if only one monitor in the ws_monitor object)
#' @description Creates a bar plot showing daily average PM 2.5 values for a specific monitor in a ws_monitor object.
#' Each bar is colored according to its AQI category.
#' @examples
#' \dontrun{
#' ws_monitor <- wrcc_load(20150801, 20150820)
#' # Pull out first monitorID from 'meta' dataframe
#' monitorID <- ws_monitor$meta$monitorID[1]
#' monitor_dailyBarPlot(ws_monitor, monitorID)
#' }

monitor_dailyBarPlot <- function(ws_monitor, monitorID=NULL) {
  # Data Preparation ----------------------------------------------------------
  
  # NOTE:  Incomting insituTime is GMT
  
  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) && nrow(ws_monitor$meta) == 1 ) {
    monitorID <- ws_monitor$meta$monitorID[1]
  }
  
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
  localTime <- lubridate::with_tz(GMTTime, stationMetadata$timezone[index])
  
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
  
  if (nrow(dfMean) == 0) {
    futile.logger::flog.warn('There are no full days to plot in monitor_dailyBarPlot. Try a ws_monitor that covers more days.')
  }
  
  
  # Plotting ------------------------------------------------------------------

  # Colors come from pm25Daily means
  aqiColors <- adjustcolor(AQI$colors, 0.5)
  cols <- aqiColors[ .bincode(dfMean$pm25, AQI$breaks_24, include.lowest=TRUE) ]
  
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
