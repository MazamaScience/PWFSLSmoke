#' @keywords ws_monitor
#' @export
#' @title Create a dataframe of current monitor data
#' @param ws_monitor \emph{ws_monitor} object
#' @return A tibble of current status data with the following columns: 
#' monitorID, pm25, nowcast, yesterdayAqi, lastValidUpdateTime, processingTime.
#' @description Extracts current status data from a ws_monitor object. This data includes the following:
#' \itemize{
#' \item{monitorID - the PWFSL monitor ID}
#' \item{lastValidUpdateTime - UTC POSIXct time corresponding to the last valid pm25 datum}
#' \item{processingTime - UTC POSIXct when the function is run (= 'now')}
#' \item{latency - (difference between processingTime -1 hr) and lastValidUpdateTime, floored to the hour}
#' \item{PM2.5_nowcast - NowCast value at lastValidUpdateTime}
#' \item{PM2.5_latest_1 - PM2.5 value at lastValidUpdateTime (should never be null)}
#' \item{PM2.5_latest_3 - mean of the three hours preceeding lastValidUpdateTime}
#' \item{PM2.5_yesterday - local time midnight-to-midnight 24-hour mean for the day prior to processingTime}
#' }
#' @note
#' Data are assigned to the beginning of the hour they represent. So a PM2.5 value assigned to 2pm
#' will represent data averaged over the period 14:00 - 14:59. This is in keeping with a day representing
#' 00:00 - 23:59 and a month representing the beginning of the month until the end of the month.
#' 
#' Because of this, data for 2pm should never be available until just after 3pm. If it is currently 3:15pm
#' then we need to subtract 1 hour from the current \code{processingTime} before subtracting the \code{lastValidUpdateTime}
#' to generate the \code{latency}.
#' @examples
#' \dontrun{
#' wa <- loadLatest() %>% monitor_subset(stateCodes = 'WA')
#' wa_current <- monitor_currentData(wa)
#' }

monitor_currentData <- function(ws_monitor) {
  
  # Pull out data
  data <- ws_monitor$data
  meta <- ws_monitor$meta
  
  # Initialize currentData tbl with monitorIDs
  monitorIDs <- ws_monitor$meta$monitorID
  currentData <- tibble(monitorID = monitorIDs)
  
  if ( nrow(currentData) == 0 ) {
    stop('No sites found with PM2.5 data')
  }
  
  # Apply the nowcast algorithm 
  nowcast <- monitor_nowcast(ws_monitor, includeShortTerm = TRUE)
  data_nowcast <- nowcast$data
  
  # Add lastValidUdateTime and latency columns

  lastIndex <- apply(as.matrix(data), 2, function(x) { max(which(!is.na(x))) }) # this is a named vector
  lastUTCTime <- data$datetime[lastIndex]
  currentData$lastValidUpdateTime <- lastUTCTime[-1] # remove 'datetime'
  processingTime <- lubridate::now('UTC')
  currentData$processingTime <- processingTime
  # Calculate the latency in hours
  # NOTE:  According to https://docs.airnowapi.org/docs/HourlyDataFactSheet.pdf
  # NOTE:  a datum assigned to 2pm represents the average of data between 2pm and 3pm.
  # NOTE:  So, if we check at 3:15 and see that we have a value for 2pm but not 3pmm 
  # NOTE:  then the data are completely up-to-date with zero latency. That's why we
  # NOTE:  subtract an hour from 'processingTime' in the line below.
  zeroLatencyTime <- lubridate::floor_date(processingTime, unit="hour") - lubridate::dhours(1)
  currentData$latency <- as.numeric( difftime(zeroLatencyTime, currentData$lastValidUpdateTime, units="hour") )
  
  # ----- Add data values to currentData tbl -----------------------------------
  
  # Add new columns of the proper type
  currentData$PM2.5_nowcast <- as.numeric(NA)
  currentData$PM2.5_latest_1 <- as.numeric(NA)
  currentData$PM2.5_latest_3 <- as.numeric(NA)
  currentData$PM2.5_yesterday <- as.numeric(NA)
  
  # Additional values that must be calculated per-monitoring-site
  for ( monitorID in currentData$monitorID ) {
    
    lastRow <- lastIndex[monitorID]
    last3Rows <- (lastRow-2):lastRow
    
    # NOTE:  If a monitor was recently installed, the nowcast algorithm will return NAs for the first 12 hours.
    
    # latest nowcast value
    currentData[currentData$monitorID == monitorID,'PM2.5_nowcast'] <- round(data_nowcast[lastRow,monitorID], digits=1)
    
    # latest hourly data
    currentData[currentData$monitorID == monitorID,'PM2.5_latest_1'] <- round(data[lastRow,monitorID], digits=1)
    
    # latest 3-hour mean
    threeHourMean <- round(mean(data[last3Rows,monitorID], na.rm=FALSE), digits=1)
    if ( !is.nan(threeHourMean) ) { # you get NaN when all input data are NA
      currentData[currentData$monitorID == monitorID,'PM2.5_latest_3'] <- threeHourMean
    }
    
    # Determine monitor-local-time 'yesterdayMask'
    localDatetime <- lubridate::with_tz(data$datetime, meta[monitorID,'timezone'])
    localNow <- lubridate::with_tz(lubridate::now(), meta[monitorID,'timezone'])
    yesterdayEnd <- lubridate::floor_date(localNow,'day')
    yesterdayStart <- yesterdayEnd - lubridate::ddays(1)
    yesterdayMask <- localDatetime >= yesterdayStart & localDatetime < yesterdayEnd
    
    # 24-hour mean for yesterday
    yesterdayValues <- data[yesterdayMask, monitorID]
    # Use data.thresh=75 just as in openair::rollingMean() (missing 6 or fewer/24 hours)
    if ( length(yesterdayValues) >= 18 && sum(is.na(yesterdayValues)) <= 6 ) {
      currentData[currentData$monitorID == monitorID,'PM2.5_yesterday'] <- round(mean(yesterdayValues, na.rm=TRUE), digits=1)
    }
    
  }
  
  return(currentData)
  
} 

