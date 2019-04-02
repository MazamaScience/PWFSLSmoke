#' @title Get current status of monitors
#'
#' @description
#' This function augments the metadata from a \emph{ws_monitor}
#' object with summarized and aggregate data from the \emph{ws_monitor} object.
#'
#' @param ws_monitor \emph{ws_monitor} object.
#' @param endTime Time to which the status of the monitors will be current. By
#'   default, it is the most recent time in \code{ws_monitor}. This time can be
#'   given as a POSIXct time, or a string/numeric value in ymd format (eg.
#'   20190301). This time converted to UTC.
#' @param monitorURLBase A URL prefix pointing to where more information about a
#'   monitor can be found. By default, it points to the AirFire monitoring site.
#'
#' @return A table containing the current status information for all the
#'   monitors in \emph{ws_monitor}.
#'
#' @section "Last" and "Previous":
#' The goal of this function is to provide useful information about what
#' happened recently with each monitor in the provided \emph{ws_monitor} object.
#' Monitors sometimes don't consistently report data, however, and it's not
#' useful to have \code{NA}'s reported when there is still valid data at other
#' times. To address this, \code{monitor_getCurrentStatus} uses \emph{last} and
#' \emph{previous} valid times. These are the time when a monitor most recently
#' reported data, and the most recent time of valid data before that,
#' respectively. By reporting on these times, the function ensures that valid
#' data is returned and provides information on how outdated this information
#' is.
#'
#' @section Calculating latency:
#' According to https://docs.airnowapi.org/docs/HourlyDataFactSheet.pdf
#' a datum assigned to 2pm represents the average of data between 2pm and 3pm.
#' So, if we check at 3:15pm and see that we have a value for 2pm but not 3pm
#' then the data are completely up-to-date with zero latency.
#'
#' \code{monitor_getCurrentStatus()} defines latency as the difference in time
#' between the given time index and the next most recent time index. If there is
#' no more recent time index, then the difference is measured to the given
#' \code{endTime} parameter. These differences are recorded in hours.
#'
#' For example, if the recorded values for a monitor are
#' \code{[16.2, 15.8, 16.4, NA, 14.0, 12.5, NA, NA, 13.3, NA]}, then the last
#' valid time index is 9, and the previous valid time index is 6. The last
#' latency is then 1 (hour), and the previous latency is 3 (hours).
#'
#' @section Summary data:
#' The table created by \code{monitor_getCurrentStatus()} includes summary
#' information for the data part of the given \emph{ws_monitor} object. The
#' summaries included are listed below with a description:
#'
#' \tabular{ll}{
#'   yesterday_pm25_24hr   \tab Daily AQI value for the day prior to
#'                              \code{endTime}\cr
#'   last_nowcast_1hr      \tab Last valid NowCast measurement\cr
#'   last_PM2.5_1hr        \tab Last valid raw PM2.5 measurement\cr
#'   last_PM2.5_3hr        \tab Mean of the last valid raw PM2.5 measurement
#'                              with the preceding two measurements\cr
#'   previous_nowcast_1hr  \tab Previous valid NowCast measurement\cr
#'   previous_PM2.5_1hr    \tab Previous valid raw PM2.5 measurement\cr
#'   previous_PM2.5_3hr    \tab Mean of the previous valid raw PM2.5 measurement
#'                              with the preceding two measurements\cr
#'   last_nowcastLevel     \tab NowCast level at the last valid time\cr
#'   previous_nowcastLevel \tab NowCast level at the previous valid time
#' }
#'
#' It should be noted that all averages are "right-aligned", meaning that the
#' three hour mean of data at time \code{n} will comprise of the data at times
#' \code{[n-2, n-1, n]}. Data for \code{n-2} and \code{n-1} is not guaranteed to
#' exist, so a three hour average may include 1 to 3 data points.
#'
#' @section Event flags:
#' The table created by \code{monitor_getCurrentStatus()} also includes binary
#' flags representing events that may have occurred for a monitor within the
#' bounds of the specified end time and data in the \emph{ws_monitor} object.
#' Each flag is listed below with its corresponding meaning:
#'
#' \tabular{ll}{
#'   USG6 \tab NowCast level increased to Unhealthy for Sensitive Groups in the
#'             last 6 hours\cr
#'   U6   \tab NowCast level increased to Unhealthy in the last 6 hours\cr
#'   VU6  \tab NowCast level increased to Very Unhealthy in the last 6 hours\cr
#'   HAZ6 \tab NowCast level increased to Hazardous in the last 6 hours\cr
#'   MOD6 \tab NowCast level decreased to Moderate or Good in the last 6 hours\cr
#'   NR6  \tab Monitor not reporting for more than 6 hours\cr
#'   NEW6 \tab New monitor reporting in the last 6 hours\cr
#'   MAL6 \tab Monitor malfunctioning the last 6 hours (not currently implemented)
#' }
#'
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' \dontrun{
#' ws_monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "WA")
#' statusTbl <- monitor_getCurrentStatus(ws_monitor)
#' }

monitor_getCurrentStatus <- function(
  ws_monitor,
  endTime = NULL,
  monitorURLBase = "http://tools.airfire.org/monitoring/v4/#!/?monitors="
) {

  logger.debug("----- monitor_getCurrentStatus() -----")

  # Sanity checks --------------------------------------------------------------
  logger.trace("Performing sanity checks on `ws_monitor` parameter.")

  if (!monitor_isMonitor(ws_monitor))
    errMsg <- "Not a valid `ws_monitor` object."
    logger.error(errMsg)
    stop(errMsg)

  if (monitor_isEmpty(ws_monitor))
    errMsg <- "`ws_monitor` object contains zero monitors."
    logger.error(errMsg)
    stop(errMsg)


  # Prepare parameters ---------------------------------------------------------
  logger.trace("Preparing time range and subsetting data.")

  if (is.null(endTime)) {
    endTime <- max(ws_monitor[["data"]][["datetime"]])
  }

  # parseDateTime will fail if it produces NA
  endTime <- parseDatetime(endTime)
  endTimeInclusive <- endTime %>%
    lubridate::floor_date(unit = "hour") %>%
    magrittr::subtract(lubridate::dhours(1))

  startTime <- min(ws_monitor[["data"]][["datetime"]])

  ws_monitor <- ws_monitor %>%
    monitor_subset(tlim = c(startTime, endTimeInclusive))

  # Check again to make sure subset includes data
  if (monitor_isEmpty(ws_monitor)) {
    errMsg <- paste0(
      "ws_monitor object contains zero monitors with data from before ", endTime
    )
    logger.error(errMsg)
    stop(errMsg)
  }


  # Prepare data ---------------------------------------------------------------
  logger.trace("Separating 'meta' and 'data' and calculating nowcast.")

  processingTime <- lubridate::now("UTC")

  ws_data <- ws_monitor %>% monitor_extractData() %>% as_tibble()
  ws_meta <- ws_monitor %>% monitor_extractMeta() %>% as_tibble(rownames = NULL)

  nowcast_data <- ws_monitor %>%
    monitor_nowcast(includeShortTerm = TRUE) %>%
    monitor_extractData() %>%
    as_tibble()


  # Initialize output ----------------------------------------------------------
  logger.trace("Augmenting metadata with initial information.")

  currentStatus <-
    ws_meta %>%
    mutate(
      `monitorURL` = paste0(monitorURLBase, .data$monitorID),
      `processingTime` = processingTime,
      `endTime` = endTime
    )


  # Generate latency data ------------------------------------------------------
  logger.trace("Calculating latency data.")

  ## NOTE:
  #  The number of levels of valid time indices is set by `indexLevels`, with
  #  lower numbers corresponding to more recent times.

  ## NOTE on anonymous functions ("~") in purrr:
  #  The "~" in R is generally used to create formulas, but in the `purrr`
  #  package it has the specialized purpose of defining succinct anonympus
  #  functions.
  #
  #  Prefixing a function with "~" within `purrr` allows you to alter the
  #  arguments the function accepts, by wrapping arguments in other functions,
  #  or setting an argument to a constant value. `.x` and `.y` are two variables
  #  `purrr` will treat as the mapping variables in `map`, `map2`, etc.
  #
  #  For more details, see `?purrr::map` or purrr.tidyverse.org/reference/map

  indexLevels <- 1:2

  validTimeIndices <- ws_data %>%
    arrange(.data$datetime) %>%
    select(-.data$datetime) %>%
    purrr::map(~rev(which(!is.na(.x)))[indexLevels]) %>%
    tibble::enframe("monitorID", "index") %>%
    tidyr::unnest(.data$index) %>%
    group_by(.data$monitorID) %>%
    mutate(key = row_number()) %>%
    tidyr::spread(.data$key, .data$index) %>%
    ungroup()

  ## NOTE:
  #  The last and previous valid time indices are created as named vectors for
  #  easy portability to helper functions later on.

  lastValidTimeIndex <- validTimeIndices %>%
    select(.data$monitorID, .data$`1`) %>%
    tibble::deframe()
  previousValidTimeIndex <- validTimeIndices %>%
    select(.data$monitorID, .data$`2`) %>%
    tibble::deframe()

  latencyStatus <-
    validTimeIndices %>%
    left_join(ws_meta[c("monitorID", "timezone")], by = "monitorID") %>%
    transmute(
      `monitorID` = .data$monitorID,
      `last_validTime` = ws_data[["datetime"]][.data$`1`],
      `last_latency` = as.numeric(difftime(
        endTimeInclusive, .data$last_validTime, units = "hour"
      )),
      `previous_validTime` = ws_data[["datetime"]][.data$`2`],
      `previous_latency` = as.numeric(difftime(
        .data$last_validTime, .data$previous_validTime,  units = "hour"
      )),
      `last_validLocalTimestamp` =
        lubridate::with_tz(.data$last_validTime, tzone = .data$timezone) %>%
        strftime(format = "%Y-%m-%d %H:%M:%S %Z"),
      `previous_validLocalTimestamp` =
        lubridate::with_tz(.data$previous_validTime, tzone = .data$timezone) %>%
        strftime(format = "%Y-%m-%d %H:%M:%S %Z")
    )


  # Generate summary data ------------------------------------------------------
  logger.trace("Calculating summary data.")

  ## Note: see documentation for summary descriptions

  summaryData <-
    list(
      .yesterday_avg(ws_monitor, endTime, "yesterday_pm25_24hr"),

      .averagePrior(nowcast_data, lastValidTimeIndex,     1, "last_nowcast_1hr"),
      .averagePrior(ws_data,      lastValidTimeIndex,     1, "last_pm25_1hr"),
      .averagePrior(ws_data,      lastValidTimeIndex,     3, "last_pm25_3hr"),
      .averagePrior(nowcast_data, previousValidTimeIndex, 1, "previous_nowcast_1hr"),
      .averagePrior(ws_data,      previousValidTimeIndex, 1, "previous_pm25_1hr"),
      .averagePrior(ws_data,      previousValidTimeIndex, 3, "previous_pm25_3hr")
    ) %>%
    purrr::reduce(
      left_join, by = "monitorID",
      .init = ws_meta["monitorID"]
    )


  # Generate event data --------------------------------------------------------
  logger.trace("Calculating event data.")

  ## Note: See documentation for event descriptions

  eventData <-
    list(
      latencyStatus[c("monitorID", "last_latency", "previous_latency")],

      .aqiLevel(nowcast_data, lastValidTimeIndex,     "last_nowcastLevel"),
      .aqiLevel(nowcast_data, previousValidTimeIndex, "previous_nowcastLevel"),

      .isNotReporting(ws_data, 6,     endTimeInclusive, "NR6"),
      .isNewReporting(ws_data, 6, 48, endTimeInclusive, "NEW6")
    ) %>%
    purrr::reduce(
      left_join, by = "monitorID",
      .init = ws_meta["monitorID"]
    ) %>%
    mutate(
      `USG6` = .levelChange(.data, 3, 6, "increase"),
      `U6`   = .levelChange(.data, 4, 6, "increase"),
      `VU6`  = .levelChange(.data, 5, 6, "increase"),
      `HAZ6` = .levelChange(.data, 6, 6, "increase"),
      `MOD6` = .levelChange(.data, 2, 6, "decrease") | .levelChange(.data, 1, 6, "decrease"),
      `MAL6` = NA
    ) %>%
    select(-.data$last_latency, -.data$previous_latency)


  # Combine and return data ----------------------------------------------------
  logger.trace("Combining data and returning results.")

  output <- list(
    currentStatus,
    latencyStatus,
    summaryData,
    eventData
  ) %>%
  purrr::reduce(left_join, by = "monitorID")

  return(output)

}


# Debugging --------------------------------------------------------------------

if (FALSE) {

  ws_monitor <- monitor_loadLatest() %>%
    monitor_subset(stateCodes = "WA")
  endTime <-  lubridate::now("UTC")
  monitorURLBase <- NULL

}


# Helper functions -------------------------------------------------------------

## - Summary functions
## - Event functions


# * Summary functions  ---------------------------------------------------------

#' @title Get the 'n' prior hour average for monitors
#'
#' @description
#' From the given \code{timeIndices}, get the n-hour, right-aligned, air quality
#' average for monitors from the given data, presented as a dataframe.
#'
#' @param data The 'data' part of a \emph{ws_monitor} object.
#' @param timeIndices Named vector of valid time indices for each monitor in
#'   \code{data}.
#' @param n Number of time units to include in average.
#' @param colTitle Title of the column of averaged values in the returned
#'   dataframe.
#'
#' @return A dataframe consisting of monitor IDs and the average air quality
#'   values over the last n time units.
#'
#' @noRd
.averagePrior <- function(data, timeIndices, n, colTitle) {

  logger.trace("Calculating %d hour average for column '%s'.", n, colTitle)

  get_nAvg <- function(monitorDataColumn, timeIndex, n) {

    if (timeIndex < n) return(NA)

    avg <- monitorDataColumn %>%
      magrittr::extract((timeIndex - n + 1):timeIndex) %>%
      mean(na.rm = TRUE) %>%
      round(digits = 1)

    if (is.nan(avg)) return(NA)
    else return(avg)

  }

  ## NOTE on tidy evaluation:
  #  `dplyr` and other tidyverse packages often use non-standard evaluation to
  #  make writing code for exploratory analysis easier. This, however, can make
  #  programming with these packages difficult, as they don't behave as you
  #  would expect. To get around this, 'tidyeval' is used. Functions like
  #  `enquo()`, `!!`, and `:=` all serve to make complex programming with these
  #  packages possible.
  #
  #  For more details and examples, see
  #  `vignette("programming", package = "dplyr")`, or
  #  dplyr.tidyverse.org/articles/programming

  # See NOTE on anonymous functions ("~") in purrr

  avgData <-
    purrr::map2_dfc(
      select(data, -.data$datetime), timeIndices,
      ~get_nAvg(.x, .y, n)
    ) %>%
    tidyr::gather("monitorID", !!colTitle)

  return(avgData)

}


#' @title Get yesterday's AQI value for monitors
#'
#' @description
#' From the given \emph{ws_monitor} object, get the AQI values for the the date
#' one day prior to the given UTC time (in the timezone of the monitor) for
#' every monitor, presented as a dataframe.
#'
#' @param ws_monitor \emph{ws_monitor} object.
#' @param endTimeUTC UTC time. AQI values are calculated for the previous date.
#' @param colTitle Title of the column of AQI values in the returned dataframe.
#'
#' @return A dataframe consisting of monitor IDs and the AQI value for the date
#'   before \code{endTimeUTC}.
#'
#' @noRd
.yesterday_avg <- function(ws_monitor, endTimeUTC, colTitle) {

  logger.trace("Calculating yesterday average for column '%s'.", colTitle)

  get_previousDayAvg <- function(ws_data, timezone, endTimeUTC) {

    # Get previous day in timezone of monitors
    previousDayStart <- endTimeUTC %>%
      lubridate::with_tz(timezone) %>%
      lubridate::floor_date(unit = "day") %>%
      magrittr::subtract(lubridate::ddays(1))

    # Pull out daily averages from chosen date for each monitor
    aqiData <- ws_data %>%
      filter(.data$datetime == previousDayStart) %>%
      select(-.data$datetime) %>%
      round(digits = 1)

    return(aqiData)

  }

  aqiDataList <- ws_monitor %>%
    monitor_dailyStatisticList() %>%
    purrr::transpose() %>%
    magrittr::use_series("data")

  # See NOTE on tidy evaluation for explanation of `!!`
  # See NOTE on anonymous functions ("~") in purrr

  previousDayAQI <- aqiDataList %>%
    purrr::imap_dfc(~get_previousDayAvg(.x, .y, endTimeUTC)) %>%
    tidyr::gather("monitorID", !!colTitle)

  return(previousDayAQI)

}


# * Event functions ------------------------------------------------------------

#' @title Get AQI Level for data values at specific times
#'
#' @description
#' Given \emph{ws_monitor} data and a vector of times corresponding to each
#' monitor in the data, this function returns the AQI level of each monitor at
#' its given time, presented as a dataframe.
#'
#' @param data The 'data' part of a \emph{ws_monitor} object (either raw pm2.5
#'   or NowCast).
#' @param timeIndices Named vector of valid time indices for each monitor in
#'   \code{data}.
#' @param colTitle Title of the column of AQI levels in the returned dataframe.
#'
#' @return A dataframe consisting of monitor IDs and the AQI level for the data
#'   at the given \code{timeIndices}.
#'
#' @noRd
.aqiLevel <- function(data, timeIndices, colTitle) {

  logger.trace("Calculating AQI levels for column '%s'.", colTitle)

  levels <- as.matrix(data[, -1]) %>%
    magrittr::extract(cbind(unname(timeIndices), seq_along(timeIndices))) %>%
    .bincode(AQI$breaks_24, include.lowest = TRUE)

  # See NOTE on tidy evaluation for explanation of `!!` and `:=`

  return(tibble(`monitorID` = names(timeIndices), !!colTitle := levels))

}

#' @title Check for a change in AQI level between two times
#'
#' @description
#' This function returns a boolean vector with values for each monitorID in
#' \code{data}, indicating of the AQI level has increased/decreased to or past
#' the given \code{level} between the previous and last recorded levels.
#'
#' If the difference in time betwen the previous and last recored levels is
#' greater than \code{n} hours, FALSE is returned.
#'
#' @param data A dataframe containing columns for the last and previous
#'   NowCast AQI levels, as well as the latencies for both of those times.
#' @param level Check if the AQI level has changed to or past the given level
#'   between the last and previous AQI levels.
#' @param n Maximum number of hours between last and previous time values for
#'   data to be considered relevent. FALSE is recorded for any time differences
#'   greater than n.
#' @param direction Check if the AQI level change results in an increase or
#'   decrease in AQI level, moving forward in time (valid options:
#'   'increase' | 'decrease').
#'
#' @return A vector of Boolean values indicating if the specified AQI level
#'   change occurred.
#'
#' @noRd
.levelChange <- function(data, level, n, direction) {

  logger.trace(
    "Checking if AQI levels had an %s to level %d in the last %d hours.",
    direction, level, n
  )

  if (direction == "increase") {
    result <-
      (data$last_nowcastLevel >= level) &
      (data$previous_nowcastLevel < level)
  } else if (direction == "decrease") {
    result <-
      (data$previous_nowcastLevel > level) &
      (data$last_nowcastLevel <= level)
  } else {
    stop("'direction' must be either 'increase' or 'decrease'.")
  }

  return(result & (data$last_latency + data$previous_latency <= n))

}

#' @title Check if a monitor has stopped reporting data
#'
#' @description
#' This function returns a dataframe with boolean values for each monitorID in
#' \code{data}, indicating if the monitor has stopped reporting data.
#'
#' A monitor is considered to not be reporting data if it does not have data for
#' the \code{n} hours leading up to \code{entTimeUTC}.
#'
#' @param data The 'data' part of a \emph{ws_monitor} object.
#' @param n The number of hours prior to \code{endTimeUTC} in which a monitor
#'   should not have data.
#' @param endTimeUTC Time considered to be most recent in \code{data}.
#' @param colTitle Title of the column of boolean values in the returned
#'   dataframe.
#'
#' @return A dataframe consiting of monitor IDs and boolean values indicating if
#'   that monitor has stopped reporting data.
#'
#' @noRd
.isNotReporting <- function(data, n, endTimeUTC, colTitle) {

  logger.trace(
    "Checking if monitors have not reported in the last %d hours for column '%s'.",
    n, colTitle
  )

  startTimeInclusive <- endTimeUTC %>%
    magrittr::subtract(lubridate::dhours(n - 1))

  # See NOTE on tidy evaluation for explanation of `!!`
  # See NOTE on anonymous functions ("~") in purrr

  result <- data %>%
    filter(.data$datetime >= startTimeInclusive) %>%
    select(-.data$datetime) %>%
    summarise_all(~all(is.na(.))) %>%
    tidyr::gather("monitorID", !!colTitle)

  return(result)
}

#' @title Check if a new monitor is reporting in the data
#'
#' @description
#' This function returns a dataframe with boolean values for each monitorID in
#' \code{data}, indicating if the monitor is newly reporting data.
#'
#' A monitor is considered to be newly reporting if data for that monitor
#' doesn't exist a \code{buffer} number of hours before \code{endTimeUTC},
#' except for data existing in the \code{n} hours before \code{endTimeUTC}.
#'
#' For example, if \code{buffer = 48}, and \code{n = 6}, then a true value
#' would indicate that a monitor which has not been reporting for the past two
#' days has started reporting in the last six hours.
#'
#' @param data The 'data' part of a \emph{ws_monitor} object.
#' @param n The number of hours prior to \code{endTimeUTC} in which a monitor
#'   should have data.
#' @param buffer The number of hours minus \code{n}, before \code{n}, in which
#'   a monitor should not have data
#' @param endTimeUTC Time considered to be most recent in \code{data}.
#' @param colTitle Title of the column of boolean values in the returned
#'   dataframe.
#'
#' @return A dataframe consiting of monitor IDs and boolean values indicating if
#'   that monitor is newly reporting data.
#' @noRd
.isNewReporting <- function(data, n, buffer, endTimeUTC, colTitle) {

  logger.trace(
    "Checking if new monitors are reporting for column '%s'.",
    colTitle
  )

  bufferEndTime <- endTimeUTC - lubridate::dhours(n)

  recentReport <- .isNotReporting(data, n, endTimeUTC, "recent")
  bufferReport <- .isNotReporting(data, buffer, bufferEndTime, "buffer")

  # See NOTE on tidy evaluation for explanation of `!!` and `:=`

  result <-
    left_join(recentReport, bufferReport, by = "monitorID") %>%
    mutate(!!colTitle := .data$buffer & !.data$recent) %>%
    select(-.data$buffer, -.data$recent)

  return(result)

}
