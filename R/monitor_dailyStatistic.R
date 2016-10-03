#' @keywords monitor
#' @export
#' @title Calculate Daily Statistics from Monitor Data
#' @param ws_monitor data list of class \code{ws_monitor}
#' @param FUN function used to collapse a day's worth of data into a single number
#' @param dayStart one of "sunset|midnight|sunrise"
#' @param na.rm logical value indicating whether NA values should be ignored
#' @description Calculate daily statistics for each monitor in the \code{ws_monitor} data list.
#' @details Sunrise and sunset times are calculated based on the first monitor encountered.
#' This should be accurate enough for all use cases involving co-located monitors. Monitors
#' from different regions should have daily statistics calculated separately.
#' @return Monitor data list with daily statistics.
#' @examples 
#' \dontrun{
#' setSmokeDataDir('~/Data/Smoke')
#' airnow <- airnow_load(20150801, 20150831)
#' WA_smoky <- monitor_subset(airnow, stateCodes='WA', vlim=c(55,Inf))
#' WA_smoky_dailyMean <- monitor_dailyStatistic(WA_smoky, FUN=get('mean'), dayStart='midnight')
#' monitor_timeseriesPlot(WA_smoky_dailyMean, type='s')
#' }

# TODO:  Add an argument specifying the minimum number of hours required per day

monitor_dailyStatistic <- function(ws_monitor, FUN=get("mean"), dayStart="midnight", na.rm=TRUE) {
  
  # Pull out dataframes
  data <- ws_monitor$data
  meta <- ws_monitor$meta
  
  # Sanity check the timezones
  timezoneCount <- length(unique(meta$timezone))
  if ( timezoneCount > 1 ) {
    warning(paste0('Found ',timezoneCount,' timezones. Only the first will be used'))      
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
  
  # Create the aggregated dataset
  # NOTE:  Some functions don't work on the POSIXct datetime column.
  # NOTE:  But we still want to keep it. So well start by calculating the mean
  # NOTE:  so as to have an "average" POSIXct for each grouping. Then we'll convert
  # NOTE:  it to numeric so that it can be operated on by the likes of 'sum'. Finally
  # NOTE:  we'll restore the average datetime.
  meanDF <- stats::aggregate(data, by=list(day), FUN=get('mean'), na.rm=na.rm)
  data$datetime <- as.numeric(data$datetime)
  df <- stats::aggregate(data, by=list(day), FUN=FUN, na.rm=na.rm)
  df$datetime <- meanDF$datetime
  
  # Only retain the original columns (omit "Group.1", etc.)
  df <- df[,names(data)]
  
  # Set df$datetime to noon for each day
  lubridate::hour(df$datetime) <- 12
  lubridate::minute(df$datetime) <- 00
  lubridate::second(df$datetime) <- 00
  

  # Create a new ws_monitor object
  ws_monitor <- list(meta=meta, data=df)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return (ws_monitor)
}
