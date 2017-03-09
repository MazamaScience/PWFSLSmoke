#' @keywords ws_monitor
#' @export
#' @title Calculate Daily Statistics for each Monitor in a ws_monitor Object
#' @param ws_monitor ws_monitor object
#' @param FUN function used to collapse a day's worth of data into a single number for each monitor in the ws_monitor object
#' @param dayStart one of \code{sunset|midnight|sunrise}
#' @param na.rm logical value indicating whether NA values should be ignored
#' @param minHours minimum number of valid data hours required to calculate each daily statistic
#' @return A ws_monitor object with daily statistics for the local timezone.
#' @description Calculates daily statistics for each monitor in \code{ws_monitor}.
#' @details Sunrise and sunset times are calculated based on the first monitor encountered.
#' This should be accurate enough for all use cases involving co-located monitors. Monitors
#' from different regions should have daily statistics calculated separately.
#' @note Note that the incoming \code{ws_monitor} object should have UTC (GMT)
#' times and that this function calculates daily statistics based on local (clock) time.
#' If you choose a date range based on UTC times this may result in an insufficient 
#' number of hours in the first and last daily records of the returned \code{ws_monitor}
#' object.
#' 
#' The returned \code{ws_monitor} object has a daily time axis where each time is set to 00:00, local time.
#' @examples 
#' N_M <- Northwest_Megafires
#' wa_unhealthy <- monitor_subset(N_M, stateCodes='WA', tlim=c(20150701,20150831), vlim=c(55,Inf))
#' wa_unhealthy_dailyMean <- monitor_dailyStatistic(wa_unhealthy, FUN=get('mean'), dayStart='midnight')
#' monitorPlot_timeseries(wa_unhealthy_dailyMean, style='gnats')
#' addAQILines()
#' title('Washington Unhealthy Sites - Daily Average, 2015')

monitor_dailyStatistic <- function(ws_monitor, FUN=get("mean"), dayStart="midnight", na.rm=TRUE,
                                   minHours=24) {
  
  # Pull out dataframes
  data <- ws_monitor$data
  meta <- ws_monitor$meta
  
  # Sanity check the timezones
  timezoneCount <- length(unique(meta$timezone))
  if ( timezoneCount > 1 ) {
    warning(paste0('Found ',timezoneCount,' timezones. Only the first will be used'))      
  }
  timezone <- meta$timezone[1]
  
  # TODO:  For single monitor, 'midnight-to-midnight', the monitorPlot_dailyBarplot in v0.8.16 had
  # TODO:  a dplyr method that seemed significantly faster than this method.
  
  # NOTE:  We will generate only a single timeInfo dataframe to guarantee that we apply
  # NOTE:  the same daily aggregation logic to all monitors. Otherwise we could potentially
  # NOTE:  have edge cases with different numbers of days for monitors in different timezones.
  timeInfo <- timeInfo(data[,1], meta$longitude[1], meta$latitude[1], timezone)
  
  # Create the day vector
  day <- rep(0,nrow(timeInfo))
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
  # NOTE:  But we still want to keep it. So we'll start by calculating the mean
  # NOTE:  so as to have an "average" POSIXct for each grouping. Then we'll convert
  # NOTE:  it to numeric so that it can be operated on by the likes of 'sum'. Finally
  # NOTE:  we'll restore the average datetime.
  meanDF <- stats::aggregate(data, by=list(day), FUN=get('mean'), na.rm=na.rm)
  data$datetime <- as.numeric(data$datetime)
  df <- stats::aggregate(data, by=list(day), FUN=FUN, na.rm=na.rm)
  df$datetime <- meanDF$datetime
  
  # Check on the number of hours per day
  # NOTE:  The table will use day # as the names. We create hoursPerday
  # NOTE:  which is a named vector whose names will match df$Group.1.
  hoursPerDay <- unlist(table(day))
  fullDayMask <- hoursPerDay[as.character(df$Group.1)] >= minHours
  
  df[!fullDayMask,names(data)] <- NA
  
  # Only retain the original columns (omit "Group.1", etc.)
  df <- df[,names(data)]
  
  # NOTE:  It appears that aggregating a day with all NAs will result in NaN
  # Convert any NaN to NA
  nanMask <- is.nan(as.matrix(df))
  df[nanMask] <- NA

  # Restore POSIXct daily datetimes
  df$datetime <- meanDF$datetime
  
  # Set df$datetime to noon for each day
  lubridate::hour(df$datetime) <- 00
  lubridate::minute(df$datetime) <- 00
  lubridate::second(df$datetime) <- 00
  lubridate::tz(df$datetime) <- timezone
  
  # Create a new ws_monitor object
  ws_monitor <- list(meta=meta, data=df)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return (ws_monitor)
}
