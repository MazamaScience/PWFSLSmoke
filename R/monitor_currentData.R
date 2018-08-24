#' @keywords ws_monitor
#' @export
#' @title Create a dataframe of current monitor data
#' @param ws_monitor \emph{ws_monitor} object
#' @return A tibble of current status data with the following columns: 
#' monitorID, pm25, nowcast, yesterdayAqi, lastValidData, processingTime.
#' @description Extracts current status data from a ws_monitor object. This data includes the following:
#' \itemize{
#' \item{monitorID - the PWFSL monitor ID}
#' \item{lastValidData - the UTC timestamp corresponding to the last valid pm25 datum}
#' \item{processingTime - the UTC timestamp that the function is run}
#' \item{latency - the difference between processingTime and lastValidData, floored to the hour}
#' \item{PM2.5_nowcast - the NowCast value at lastValidData}
#' \item{PM2.5_latest_1 - the PM2.5 value at lastValidData (should never be null)}
#' \item{PM2.5_latest_3 - the mean of the three hours preceeding lastValidData}
#' \item{PM2.5_yesterday - the 24-hour mean from midnight-to-midnight of the day before processingTime}
#' }
#' @examples
#' wa <- monitor_subset(Northwest_Megafires)
#' wa_current <- monitor_currentData(wa)

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

  lastIndex <- apply(as.matrix(data),2, function(x) { max(which(!is.na(x))) }) # this is a named vector
  lastUTCTime <- data$datetime[lastIndex]
  currentData$lastValidData <- lastUTCTime[-1] # remove 'datetime'
  processingTime <- lubridate::now('UTC')
  currentData$processingTime <- processingTime
  # Calculate the latency in hours
  # NOTE:  According to https://docs.airnowapi.org/docs/HourlyDataFactSheet.pdf
  # NOTE:  a datum assigned to 2pm represents the average of data between 2pm and 3pm.
  # NOTE:  So, if we check at 3:15 and see that we have a value for 2pm but not 3pmm 
  # NOTE:  then the data are completely up-to-date with zero latency. That's why we
  # NOTE:  subtract an hour from 'now' in the line below.
  nowIndex <- max(which(data$datetime <= (processingTime - lubridate::dhours(1)))) # this is a single number
  currentData$latency <- (nowIndex - lastIndex)[-1] # remove 'datetime'
  
  # Add localTimestamp
  currentData$localTimestamp <- ""
  for ( i in 1:nrow(currentData) ) { # loop to ignore errors due to invalid timezone
    result <- try({
      currentData$localTimestamp[i] <- strftime(currentData$lastValidData[i], format="%Y-%m-%d %l%p, %Z", tz=meta$timezone[i])
    }, silent=TRUE)
    if ( "try-error" %in% class(result) ) {
      meta$localTimestamp[i] <- strftime(meta$lastValidData[i], format="%Y-%m-%d %l%p, %Z", tz='UTC')
    }
  }
  
  
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

