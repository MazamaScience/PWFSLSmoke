#' @keywords monitor
#' @export
#' @title Calculate Daily Fraction Over Threshold from Monitor Data
#' @param ws_monitor data list of class \code{ws_monitor}
#' @param threshold AQI level name (e.g. 'unhealthy') or threshold value at or above which a measurement is counted
#' @param dayStart one of "sunset|midnight|sunrise"
#' @param na.rm logical value indicating whether NA values should be ignored
#' @description Calculate daily threshold statistics for each monitor in the \code{ws_monitor} data list.
#' @details Sunrise and sunset times are calculated based on the first monitor encountered.
#' This should be accurate enough for all use cases involving co-located monitors. Monitors
#' from different regions should have daily statistics calculated separately.
#' @return Monitor data list with daily threshold statistics.
#' @examples 
#' \dontrun{
#' AirNow <- airnow_load(20150801, 20150930)
#' 
#' WA_IDS <- AirNow$meta$monitorID[AirNow$meta$stateCode == 'WA']
#' WA <- monitor_subset(AirNow, monitorIDs=WA_IDS)
#' monitor_leaflet(WA)
#' 
#' # Pull out specific locations
#' Twisp <- monitor_subset(WA, monitorIDs='530470009')
#' Winthrop <- monitor_subset(WA, monitorIDs='530470010')
#' Omak <- monitor_subset(WA, monitorIDs='530470013')
#' 
#' # Generate 
#' Twisp_daily <- monitor_dailyThreshold(Twisp, 89, dayStart='midnight')
#' Winthrop_daily <- monitor_dailyThreshold(Winthrop, 89, dayStart='midnight')
#' Omak_daily <- monitor_dailyThreshold(Omak, 89, dayStart='midnight')
#' 
#' # Plot
#' cols <- c(adjustcolor('orange',0.6),
#'           adjustcolor('red',0.6),
#'           adjustcolor('blue',0.6))
#' legend <- c('Twisp','Winthrop','Omak')
#' monitor_timeseriesPlot(Twisp_daily, pch=16, col=cols[1])
#' monitor_timeseriesPlot(Winthrop_daily, pch=16, col=cols[2], add=TRUE)
#' monitor_timeseriesPlot(Omak_daily, pch=16, col=cols[3], add=TRUE)
#' legend('topright', legend=legend, col=cols, pch=16)
#' title('Hours per day Above Threshold (PM 2.5 = 89)')
#' }

# TODO:  Add an argument specifying the minimum number of hours required per day

monitor_dailyThreshold <- function(ws_monitor, threshold="unhealthy", dayStart="midnight", na.rm=TRUE) {
  
  # Pull out dataframes
  data <- ws_monitor$data
  meta <- ws_monitor$meta
  
  # Sanity check the timezones
  timezoneCount <- length(unique(meta$timezone))
  if ( timezoneCount > 1 ) {
    warning(paste0('Found ',timezoneCount,' timezones. Only the first will be used'))      
  }
  
  # Check if official AQI level name is provided
  if ( typeof(threshold) == "character" ) {
    index <- which(AQI$names == threshold)
    threshold <- AQI$breaks_1_3[index]
  }
  
  # NOTE:  We will generate only a single timeInfo dataframe to guarantee that we apply
  # NOTE:  the same daily aggregation logic to all monitors. Otherwise we could potentially
  # NOTE:  have edge cases with different numbers of days for monitors in different timezones.
  timeInfo <- timeInfo(data[,1], meta$longitude[1], meta$latitude[1], meta$timezone[1])
  
  # Create the day vector
  day <- 0
  dayNum <- 1
  for ( i in 1:nrow(timeInfo) ) {
    
    if (dayStart == "sunset") {
      hoursAfter <- difftime(timeInfo$localTime[i],timeInfo$sunset[i],units="hours")
      if (hoursAfter > 0 & hoursAfter <= 1) dayNum <- dayNum + 1
    } else if (dayStart == "sunrise") {
      hoursAfter <- difftime(timeInfo$localTime[i],timeInfo$sunrise[i],units="hours")
      if (hoursAfter > 0 & hoursAfter <= 1) dayNum <- dayNum + 1
    } else { # midnight by default
      if (lubridate::hour(timeInfo$localTime[i]) == 0) dayNum <- dayNum + 1
    }

    day[i] <- dayNum
    
  }
  
  # Determine which hours are above the threshold
  # NOTE:  Numeric comparison works for POSIXct so we leave the first column in placee
  aboveThreshold <- ifelse(data > threshold,1,0)
  data[,-1] <- aboveThreshold[,-1]
  
  # Create the aggregated dataset
  # NOTE:  Some functions don't work on the POSIXct datetime column.
  # NOTE:  But we still want to keep it. So well start by calculating the mean
  # NOTE:  so as to have an "average" POSIXct for each grouping. Then we'll convert
  # NOTE:  it to numeric so that it can be operated on by the likes of 'sum'. Finally
  # NOTE:  we'll restore the average datetime.
  meanDF <- aggregate(data, by=list(day), FUN=get('mean'), na.rm=na.rm)
  data$datetime <- as.numeric(data$datetime)
  df <- aggregate(data, by=list(day), FUN=get('sum'), na.rm=na.rm)
  # Now divide by the number of hours available for each day
  df <- df / table(day)
  df$datetime <- meanDF$datetime
  
  # Only retain the original columns (omit "Group.1", etc.)
  df <- df[,names(data)]
  
  # Convert from fraction back to hours (excluding the 'datetime' columm)
  df[,-1] <- df[,-1] * 24
  
  # Set df$datetime to noon for each day
  lubridate::hour(df$datetime) <- 12
  lubridate::minute(df$datetime) <- 00
  lubridate::second(df$datetime) <- 00
  

  # Create a new ws_monitor object
  ws_monitor <- list(meta=meta, data=df)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return (ws_monitor)
}
