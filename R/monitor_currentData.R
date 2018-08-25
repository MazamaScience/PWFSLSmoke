#' @keywords ws_monitor
#' @export
#' @title Create a dataframe of current monitor data
#' @param ws_monitor \emph{ws_monitor} object
#' @param monitoringUrlBase base URL for constructing a link to the PWFSL Smoke Monitoring site
#' @return A tibble of "lastValid" data and associated timing information.
#' @description Extracts current status data from a ws_monitor object. In addition to monitor metadata, the returned data include the following:
#' \itemize{
#' \item{\code{monitorID} - the PWFSL monitor ID}
#' \item{\code{lastValidTime} - UTC POSIXct time corresponding to the last valid pm25 datum}
#' \item{\code{lastValidLocalTimestamp} - ASCII version of \code{lastValidTime} in monitor-local timezone}
#' \item{\code{processingTime} - UTC POSIXct when the function is run (\emph{i.e.} 'now')}
#' \item{\code{latency} - (difference between processingTime -1 hr) and lastValidTime, floored to the hour}
#' \item{\code{lastValid_PM2.5_nowcast} - NowCast value at lastValidTime}
#' \item{\code{lastValid_PM2.5_1hr} - PM2.5 value at lastValidTime (should never be null)}
#' \item{\code{lastValid_PM2.5_3hr} - mean of the three hours preceeding lastValidTime}
#' \item{\code{yesterday_PM2.5_24hr} - local time midnight-to-midnight 24-hour mean for the day prior to processingTime}
#' \item{\code{monitoringSiteUrl} - URL to PWFSL Monitoring v4 site, initialized with a specific monitor}
#' }
#' The three-hour average will remove missing values and may represent an average of 1-3 hours.
#' 
#' The yesterday average will remove up to six missing values, returning \code{NA} if more than six hours of yesterday's data are missing.
#' 
#' \strong{NOTE:} \code{yesterday_PM2.5_24hr} represents yesterday relative to \code{processingTime} -- \emph{i.e.} actually yesterday.
#' The various 'lastValid' values describe the most recent valid values, whenever they occurred.
#' @details
#' Data are assigned to the beginning of the hour they represent. So a PM2.5 value assigned to 2pm
#' will represent data averaged over the period 14:00 - 14:59. This is in keeping with a day representing
#' 00:00 - 23:59 and a month representing the beginning of the month until the end of the month.
#' 
#' Because of this, data for 2pm should never be available until just after 3pm. If it is currently 3:15pm
#' then we need to subtract 1 hour from the current \code{processingTime} before subtracting the \code{lastValidTime}
#' to generate the \code{latency}.
#' @examples
#' \dontrun{
#' wa <- loadLatest() %>% monitor_subset(stateCodes = 'WA')
#' wa_current <- monitor_currentData(wa)
#' }

monitor_currentData <- function(ws_monitor,
                                monitoringUrlBase = 'http://tools.airfire.org/monitoring/v4/#/?monitors=') {
  
  
  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) stop("ws_monitor object contains zero monitors")
  
  # Pull out data
  data <- ws_monitor$data
  meta <- ws_monitor$meta
  
  # Convert meta to tbl without rownames
  currentData <- as_tibble(meta, rownames = NULL)
  
  if ( nrow(currentData) == 0 ) {
    stop('No sites found with PM2.5 data')
  }
  
  # Add processingTime
  processingTime <- lubridate::now('UTC')
  currentData$processingTime <- processingTime
  
  # Add lastValidTime
  lastIndex <- apply(as.matrix(data), 2, function(x) { max(which(!is.na(x))) }) # this is a named vector
  lastUTCTime <- data$datetime[lastIndex]
  currentData$lastValidTime <- lastUTCTime[-1] # remove 'datetime'
  
  # Add lastValidLocalTimestamp
  currentData$lastValidLocalTimestamp <- ""
  for ( i in 1:nrow(currentData) ) { # loop because strftime is not vectorized over tz
    result <- try({
      currentData$lastValidLocalTimestamp[i] <- strftime(currentData$lastValidTime[i], format="%Y-%m-%d %H:%M:%S %Z", tz=meta$timezone[i])
    }, silent=TRUE)
    if ( "try-error" %in% class(result) ) {
      currentData$lastValidLocalTimestamp[i] <- strftime(meta$lastValidTime[i], format="%Y-%m-%d %H:%M:%S %Z", tz='UTC')
    }
  }
  
  # Calculate the latency in hours
  # NOTE:  According to https://docs.airnowapi.org/docs/HourlyDataFactSheet.pdf
  # NOTE:  a datum assigned to 2pm represents the average of data between 2pm and 3pm.
  # NOTE:  So, if we check at 3:15 and see that we have a value for 2pm but not 3pmm 
  # NOTE:  then the data are completely up-to-date with zero latency. That's why we
  # NOTE:  subtract an hour from 'processingTime' in the line below.
  zeroLatencyTime <- lubridate::floor_date(processingTime, unit="hour") - lubridate::dhours(1)
  currentData$latency <- as.numeric( difftime(zeroLatencyTime, currentData$lastValidTime, units="hour") )

  # ----- Add data values to currentData tbl -----------------------------------

  # Apply the nowcast algorithm 
  nowcast <- monitor_nowcast(ws_monitor, includeShortTerm = TRUE)
  data_nowcast <- nowcast$data
  
  # Add new columns of the proper type
  currentData$lastValid_PM2.5_nowcast <- as.numeric(NA)
  currentData$lastValid_PM2.5_1hr <- as.numeric(NA)
  currentData$lastValid_PM2.5_3hr <- as.numeric(NA)
  currentData$yesterday_PM2.5_24hr <- as.numeric(NA)
  
  # Values that must be calculated per-monitoring-site
  for ( monitorID in currentData$monitorID ) {
    
    lastRow <- lastIndex[monitorID] # lastIndex is a named vector
    last3Rows <- (lastRow-2):lastRow
    
    # lastValid nowcast value
    currentData[currentData$monitorID == monitorID,'lastValid_PM2.5_nowcast'] <- round(data_nowcast[lastRow,monitorID], digits=1)
    
    # lastValid hourly data
    currentData[currentData$monitorID == monitorID,'lastValid_PM2.5_1hr'] <- round(data[lastRow,monitorID], digits=1)
    
    # lastValid 3-hour mean
    threeHourMean <- round(mean(data[last3Rows,monitorID], na.rm=TRUE), digits=1)
    if ( !is.nan(threeHourMean) ) { # you get NaN when all input data are NA
      currentData[currentData$monitorID == monitorID,'lastValid_PM2.5_3hr'] <- threeHourMean
    }
    
    # Determine monitor-local-time 'yesterdayMask'
    localDatetime <- lubridate::with_tz(data$datetime, meta[monitorID,'timezone'])
    localNow <- lubridate::with_tz(lubridate::now('UTC'), meta[monitorID,'timezone'])
    yesterdayEnd <- lubridate::floor_date(localNow,'day')
    yesterdayStart <- yesterdayEnd - lubridate::ddays(1)
    yesterdayMask <- localDatetime >= yesterdayStart & localDatetime < yesterdayEnd
    
    # 24-hour mean for yesterday
    yesterdayValues <- data[yesterdayMask, monitorID]
    # Use data.thresh=75 just as in openair::rollingMean() (missing 6 or fewer/24 hours)
    if ( length(yesterdayValues) >= 18 && sum(is.na(yesterdayValues)) <= 6 ) {
      currentData[currentData$monitorID == monitorID,'yesterday_PM2.5_24hr'] <- round(mean(yesterdayValues, na.rm=TRUE), digits=1)
    }
    
    # Monitoring Site Url
    currentData$monitoringSiteUrl = paste0(monitoringUrlBase,monitorID)
    
  }
  
  return(currentData)
  
} 

