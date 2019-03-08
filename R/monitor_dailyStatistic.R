#' @keywords ws_monitor
#' @export
#' @title Calculate Daily Statistics
#' @param ws_monitor \emph{ws_monitor} object
#' @param FUN function used to collapse a day's worth of data into a single number for each monitor in the ws_monitor object
#' @param dayStart one of \code{sunset|midnight|sunrise}
#' @param na.rm logical value indicating whether NA values should be ignored
#' @param minHours minimum number of valid data hours required to calculate each daily statistic
#' @return A \emph{ws_monitor} object with daily statistics for the local timezone.
#' @description Calculates daily statistics for each monitor in \code{ws_monitor}.
#' @details Sunrise and sunset times are calculated based on the first monitor encountered.
#' This should be accurate enough for all use cases involving co-located monitors. Monitors
#' from different regions should have daily statistics calculated separately.
#' @note Note that the incoming \emph{ws_monitor} object should have UTC (GMT)
#' times and that this function calculates daily statistics based on local (clock) time.
#' If you choose a date range based on UTC times this may result in an insufficient
#' number of hours in the first and last daily records of the returned \emph{ws_monitor}
#' object.
#'
#' The returned \emph{ws_monitor} object has a daily time axis where each \code{datetime} is set to
#' the beginning of each day, 00:00:00, local time.
#' @examples
#' \dontrun{
#' N_M <- monitor_subset(Northwest_Megafires, tlim=c(20150801,20150831))
#' WinthropID <- '530470010_01'
#' TwispID <- '530470009_01'
#' MethowValley <- monitor_subset(N_M, tlim=c(20150801,20150831), monitorIDs=c(WinthropID,TwispID))
#' MethowValley_dailyMean <- monitor_dailyStatistic(MethowValley, FUN=get('mean'), dayStart='midnight')
#' # Get the full Y scale
#' monitor_timeseriesPlot(MethowValley, style='gnats', col='transparent')
#' monitor_timeseriesPlot(MethowValley, style='gnats', monitorID=TwispID,
#'                        col='forestgreen', add=TRUE)
#' monitor_timeseriesPlot(MethowValley, style='gnats', monitorID=WinthropID,
#'                        col='purple', add=TRUE)
#' monitor_timeseriesPlot(MethowValley_dailyMean, type='s', lwd=2, monitorID=TwispID,
#'                        col='forestgreen', add=TRUE)
#' monitor_timeseriesPlot(MethowValley_dailyMean, type='s', lwd=2, monitorID=WinthropID,
#'                        col='purple', add=TRUE)
#' addAQILines()
#' addAQILegend("topleft", lwd=1, pch=NULL)
#' title("Winthrop & Twisp, Washington Daily Mean PM2.5, 2015")
#' }

monitor_dailyStatistic <- function(ws_monitor,
                                   FUN=get("mean"),
                                   dayStart="midnight",
                                   na.rm=TRUE,
                                   minHours=18) {

  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) stop("ws_monitor object contains zero monitors")

  # Pull out dataframes
  data <- ws_monitor$data
  meta <- ws_monitor$meta

  # Sanity check the timezones
  timezoneCount <- length(unique(meta$timezone))
  if ( timezoneCount > 1 ) {
    warning(paste0('Found ',timezoneCount,' timezones. Only the first will be used'))
  }
  timezone <- meta$timezone[1]

  # NOTE:  We will generate only a single timeInfo dataframe to guarantee that we apply
  # NOTE:  the same daily aggregation logic to all monitors. Otherwise we could potentially
  # NOTE:  have edge cases with different numbers of days for monitors in different timezones.
  timeInfo <- timeInfo(data[,1], meta$longitude[1], meta$latitude[1], timezone)

  # Create the day vector
  day <- rep(0,nrow(timeInfo))
  dayNum <- 0
  for ( i in seq_len(nrow(timeInfo)) ) {

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

  # Only retain days with enough hours
  goodDays <- names( which( table(day) >= minHours ) ) # table() names == day #
  goodDayMask <- day %in% goodDays
  data <- data[goodDayMask,]
  day <- day[goodDayMask]

  # Create the aggregated dataset
  # NOTE:  Some functions don't work on the POSIXct datetime column.
  # NOTE:  But we still want to keep it. So we'll start by calculating the mean
  # NOTE:  so as to have an "average" POSIXct for each grouping. Then we get the
  # NOTE:  dayStart for each day.
  dailyMean <- stats::aggregate(data, by=list(day), FUN=get("mean"), na.rm=na.rm)
  # NOTE:  aggregate() resets datetime to the computer timezone which causes trouble later on.
  dailyMean$datetime <- lubridate::with_tz(dailyMean$datetime, timezone)
  dayStarts <- lubridate::floor_date(dailyMean$datetime, unit="day")

  # Sanity check
  if ( any(duplicated(dayStarts)) ) {
    stop(paste0("Duplicate dayStarts created in monitor_dailyStatistic.\n",
                "Is the 'data$datetime' column properly ordered?"))
  }

  # Convert the dayStart back to numeric so that it can be operated on by the likes of 'sum'.
  data$datetime <- as.numeric(data$datetime)

  # Get the daily count of valid data points
  validData <- !is.na(data)
  dailyValids <- stats::aggregate(validData, by=list(day), FUN=get("sum"))

  # Get the daily statistic
  dailyStats <- stats::aggregate(data, by=list(day), FUN=FUN, na.rm=na.rm)
  dailyStats$datetime <- dayStarts

  # Only retain the original columns (omit "Group.1", etc.)
  dailyValids <- dailyValids[,names(data)]
  dailyStats <- dailyStats[,names(data)]

  # Mask for days with enough valid data points
  insufficientDataMask <- dailyValids < minHours # returns a matrix
  insufficientDataMask[,1] <- FALSE # never mask out the first ('datetime') column

  # Apply the mask
  dailyStats[insufficientDataMask] <- NA

  # Create a new ws_monitor object
  ws_monitor <- list(meta=meta, data=dailyStats)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))

  return (ws_monitor)
}
