#' @keywords ws_monitor
#' @export
#' @title Calculate Daily Counts of Values At or Above a Threshold
#' @param ws_monitor ws_monitor object
#' @param threshold AQI level name (e.g. \code{"unhealthy"}) or numerical threshold at or above which a measurement is counted
#' @param dayStart one of \code{"sunset|midnight|sunrise"}
#' @param minHours minimum number of hourly observations required
#' @param na.rm logical value indicating whether NA values should be ignored
#' @return A ws_monitor object with a daily count of hours at or above \code{threshold}.
#' @details \strong{NOTE:} The returned counts include values at OR ABOVE the given threshold; this applies to both categories and values. 
#' For example, passing a \code{threshold} argument = "unhealthy" will return a daily count of values that are unhealthy, 
#' very unhealthy, or extreme (i.e. >= 55.5), as will passing a \code{threshold} argument = 55.5.
#' @details AQI levels for \code{threshold} argument = one of \code{"good|moderate|USG|unhealthy|very unhealthy|extreme"}
#' @description Calculates the number of hours per day each monitor in a ws_monitor object was at or above a given threshold
#' @details Sunrise and sunset times are calculated based on the first monitor encountered.
#' This should be accurate enough for all use cases involving co-located monitors. Monitors
#' from different regions should have daily statistics calculated separately.
#' 
#' The returned \code{ws_monitor} object has a daily time axis where each time is set to 00:00, local time.
#' @examples 
#' N_M <- monitor_subset(Northwest_Megafires, tlim=c(20150801,20150831))
#' # monitorLeaflet(N_M) # to identify specific monitorIDs
#' 
#' # Pull out specific locations
#' Twisp <- monitor_subset(N_M, monitorIDs='530470009')
#' Winthrop <- monitor_subset(N_M, monitorIDs='530470010')
#' Omak <- monitor_subset(N_M, monitorIDs='530470013')
#' 
#' # Generate 
#' Twisp_daily <- monitor_dailyThreshold(Twisp, "unhealthy", dayStart='midnight', minHours=1)
#' Winthrop_daily <- monitor_dailyThreshold(Winthrop, "unhealthy", dayStart='midnight', minHours=1)
#' Omak_daily <- monitor_dailyThreshold(Omak, "unhealthy", dayStart='midnight', minHours=1)
#' 
#' # Plot
#' cols <- c(adjustcolor('orange',0.6),
#'           adjustcolor('red',0.6),
#'           adjustcolor('blue',0.6))
#' legend <- c('Twisp','Winthrop','Omak')
#' monitorPlot_timeseries(Twisp_daily, pch=15, cex=2, col=cols[1], ylab="Hours")
#' monitorPlot_timeseries(Winthrop_daily, pch=15, cex=2, col=cols[2], add=TRUE)
#' monitorPlot_timeseries(Omak_daily, pch=15, cex=2, col=cols[3], add=TRUE)
#' legend('topright', legend=legend, col=cols, pch=15)
#' title("Washington Towns with Multiple Hours per day Above 'Unhealthy', 2015")

monitor_dailyThreshold <- function(ws_monitor, threshold="unhealthy", dayStart="midnight", minHours=0, na.rm=TRUE) {
  
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
    threshold <- AQI$breaks_24[index]
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
  meanDF <- stats::aggregate(data, by=list(day), FUN=get('mean'), na.rm=na.rm)
  data$datetime <- as.numeric(data$datetime)
  df <- stats::aggregate(data, by=list(day), FUN=get('sum'), na.rm=na.rm)
  # Now divide by the number of hours available for each day
  df <- df / table(day)
  df$datetime <- meanDF$datetime
  
  # Only retain the original columns (omit "Group.1", etc.)
  df <- df[,names(data)]
  
  # Convert from fraction back to hours (excluding the 'datetime' columm)
  df[,-1] <- df[,-1] * 24
  
  # Set df$datetime to noon for each day
  lubridate::hour(df$datetime) <- 00
  lubridate::minute(df$datetime) <- 00
  lubridate::second(df$datetime) <- 00
  
  df[,2] <- ifelse(df[,2] >= minHours, df[,2], NA)

  # Create a new ws_monitor object
  ws_monitor <- list(meta=meta, data=df)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return (ws_monitor)
}
