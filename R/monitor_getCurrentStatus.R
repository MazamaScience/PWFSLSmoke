
monitor_getCurrentStatus <- function(ws_monitor,
                                     endTime = lubridate::now("UTC"),
                                     monitorURLBase = NULL) {


# Sanity checks -----------------------------------------------------------

  if (!monitor_isMonitor(ws_monitor)) stop("Not a valid `ws_monitor` object.")
  if (monitor_isEmpty(ws_monitor)) stop("`ws_monitor` object contains zero monitors.")

  # parseDateTime will fail if it produces NA
  endTimeInclusive <- endTime %>%
    parseDatetime() %>%
    lubridate::floor_date(unit = "hour") %>%
    magrittr::subtract(lubridate::dhours(1))

  startTime <- min(ws_monitor[["data"]][["datetime"]])

  ws_monitor <- ws_monitor %>%
    monitor_subset(tlim = c(startTime, endTimeInclusive))

  if (monitor_isEmpty(ws_monitor)) {
    stop("ws_monitor object contains zero monitors with data from before ", endTime)
  }


# Prepare parameters ------------------------------------------------------

  if (is.null(monitorURLBase)) {
    monitorURLBase <- "http://tools.airfire.org/monitoring/v4/#!/?monitors="
  }


# Prepare data ------------------------------------------------------------

  ws_data <- ws_monitor %>% monitor_extractData() %>% as_tibble()
  ws_meta <- ws_monitor %>% monitor_extractMeta() %>% as_tibble(rownames = NULL)

  nowcast_data <- ws_monitor %>%
    monitor_nowcast(includeShortTerm = TRUE) %>%
    monitor_extractData() %>%
    as_tibble()


# Calculate monitor latency -----------------------------------------------

  ## TODO:
  #  how are different timezones handled when compared against endTime
  #  and processTime?

  processTime <- lubridate::now("UTC")

  validTimeIndices <- ws_data %>%
    arrange(.data$datetime) %>%
    select(-.data$datetime) %>%
    purrr::map(~rev(which(!is.na(.x)))[1:2]) %>%
    purrr::transpose()

  lastValidTimeIndex <- unlist(validTimeIndices[[1]])
  previousValidTimeIndex <- unlist(validTimeIndices[[2]])

  lastValidTime <- ws_data[["datetime"]][lastValidTimeIndex]
  previousValidTime <- ws_data[["datetime"]][previousValidTimeIndex]


# Initialize output -------------------------------------------------------

  ## NOTE about calculating latency in hours:
  #  According to https://docs.airnowapi.org/docs/HourlyDataFactSheet.pdf
  #  a datum assigned to 2pm represents the average of data between 2pm and 3pm.
  #  So, if we check at 3:15 and see that we have a value for 2pm but not 3pm
  #  then the data are completely up-to-date with zero latency.

  currentStatus <-
    tibble(
      `monitorID` = names(lastValidTimeIndex),
      `monitorURL` = paste0(monitorURLBase, .data$monitorID),
      `processTime` = processTime,
      `endTime` = endTime,
      `last_validTime` = lastValidTime,
      `last_latency` = difftime(endTimeInclusive, lastValidTime, units = "hour"),
      `previous_validTime` = previousValidTime,
      `previous_latency` = difftime(.data$last_validTime, .data$previous_validTime,  units = "hour")
    )


# Add summary data --------------------------------------------------------

  ## Summary data:
  #
  #  * lastValid_nowcast_1hr --
  #  * lastValid_PM2.5_1hr --
  #  * lastValid_PM2.5_3hr --
  #  * yesterday_AQI --
  #  * last_nowcastLevel --
  #  * previous_nowcastLevel --

  summaryData <-
    list(
      .yesterday_AQI(ws_monitor, endTime, "yesterday_AQI"),
      .averagePrior(nowcast_data, lastValidTimeIndex, 1, "lastValid_nowcast_1hr"),
      .averagePrior(ws_data, lastValidTimeIndex, 1, "lastValid_pm25_1hr"),
      .averagePrior(ws_data, lastValidTimeIndex, 3, "lastValid_pm25_3hr"),
      .aqiLevel(nowcast_data, lastValidTimeIndex, "last_nowcastLevel"),
      .aqiLevel(nowcast_data, previousValidTimeIndex, "previous_nowcastLevel")
    ) %>%
    purrr::reduce(left_join, by = "monitorID")


# Add events --------------------------------------------------------------

  ## Events:
  #
  #  * USG6 -- NowCast level increased to Unhealthy for Sensitive Groups in the
  #            last 6 hours
  #  * U6   -- NowCast level increased to Unhealthy in the last 6 hours
  #  * VU6  -- NowCast level increased to Very Unhealthy in the last 6 hours
  #  * HAZ6 -- NowCast level increased to Hazardous in the last 6 hours
  #  * MOD6 -- NowCast level decreased to Moderate or Good in the last 6 hours
  #  * NR6  -- Monitor not reporting for more than 6 hours
  #  * MAL6 -- Monitor malfunctioning the last 6 hours
  #  * NEW6 -- New monitor reporting in the last 6 hours

  eventData <- currentStatus %>%
    left_join(summaryData, by = "monitorID") %>%
    mutate(
      `USG6` = .levelChnage(.data, 3, 6, "increase"),
      `U6` = .levelChnage(.data, 4, 6, "increase"),
      `VU6` = .levelChnage(.data, 5, 6, "increase"),
      `HAZ6` = .levelChnage(.data, 6, 6, "increase"),
      `MOD6` = .levelChnage(.data, 2, 6, "decrease") | .levelChnage(.data, 1, 6, "decrease")
    ) %>%
    list(
      .isNotReporting(ws_data, 6, endTimeInclusive, "NR6"),
      .isNewReporting(ws_data, 6, 48, endTimeInclusive, "NEW6")
    ) %>%
    purrr::reduce(left_join, by = "monitorID") %>%
    mutate(`MAL6` = NA)


# Return output -----------------------------------------------------------

  # Note: option to not append meta info?

  outputData <- ws_meta %>%
    left_join(eventData, by = "monitorID")

  return(outputData)

}


# Debugging ---------------------------------------------------------------

if (FALSE) {

  devtools::load_all()

  ws_monitor <- monitor_loadLatest() %>%
    monitor_subset(stateCodes = "WA")
  endTime <-  lubridate::now("UTC")
  monitorURLBase <- NULL

}


# Helper functions --------------------------------------------------------

.averagePrior <- function(data, timeIndices, n, colTitle) {

  qColTitle <- quo_name(enquo(colTitle))

  get_nAvg <- function(monitorDataColumn, timeIndex, n) {

    if (timeIndex < n) return(NA)

    avg <- monitorDataColumn %>%
      magrittr::extract((timeIndex - n + 1):timeIndex) %>%
      mean(na.rm = TRUE) %>%
      round(digits = 1)

    if (is.nan(avg)) return(NA)
    else return(avg)

  }

  avgData <-
    purrr::map2_dfc(
      select(data, -.data$datetime), timeIndices,
      ~get_nAvg(.x, .y, n)
    ) %>%
    tidyr::gather("monitorID", !!qColTitle)

  return(avgData)

}


.yesterday_AQI <- function(ws_monitor, endTimeUTC, colTitle) {

  qColTitle <- quo_name(enquo(colTitle))

  get_previousDayStart <- function(endTimeUTC, timezone) {

    previousDayStart <- endTimeUTC %>%
      lubridate::with_tz(timezone) %>%
      lubridate::floor_date(unit = "day") %>%
      magrittr::subtract(lubridate::ddays(1))

    return(previousDayStart)

  }

  get_previousDayAQI <- function(ws_data, previousDayStart) {

    aqiData <- ws_data %>%
      filter(.data$datetime == !!enquo(previousDayStart)) %>%
      select(-.data$datetime)

    return(round(aqiData, 1))

  }

  aqiDataList <- ws_monitor %>%
    monitor_dailyStatisticList() %>%
    purrr::transpose() %>%
    magrittr::use_series("data")

  previousDayStarts <- names(aqiDataList) %>%
    purrr::map(~get_previousDayStart(endTimeUTC, .x))

  previousDayAQI <-
    purrr::map2_dfc(
      aqiDataList, previousDayStarts,
      ~get_previousDayAQI(.x, .y)
    ) %>%
    tidyr::gather("monitorID", !!qColTitle)

  return(previousDayAQI)

}

.aqiLevel <- function(data, timeIndices, colTitle) {

  qColTitle <- quo_name(enquo(colTitle))

  levels <- as.matrix(data[, -1]) %>%
    magrittr::extract(cbind(unname(timeIndices), seq_along(timeIndices))) %>%
    .bincode(AQI$breaks_24, include.lowest = TRUE)

  return(tibble(`monitorID` = names(timeIndices), !!qColTitle := levels))

}

.levelChnage <- function(data, level, n, direction) {

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

.isNotReporting <- function(data, n, endTimeUTC, colTitle) {

  qColTitle <- quo_name(enquo(colTitle))

  startTimeInclusive <- endTimeUTC %>%
    magrittr::subtract(lubridate::dhours(n - 1))

  result <- data %>%
    filter(.data$datetime >= startTimeInclusive) %>%
    select(-.data$datetime) %>%
    summarise_all(~all(is.na(.))) %>%
    tidyr::gather("monitorID", !!qColTitle)

  return(result)
}

.isNewReporting <- function(data, n, buffer, endTimeUTC, colTitle) {

  qColTitle <- quo_name(enquo(colTitle))

  bufferEndTime <- endTimeUTC - lubridate::dhours(n)

  recentReport <- .isNotReporting(data, n, endTimeUTC, "recent")
  bufferReport <- .isNotReporting(data, buffer, bufferEndTime, "buffer")

  result <-
    left_join(recentReport, bufferReport, by = "monitorID") %>%
    mutate(!!colTitle := .data$buffer & !.data$recent) %>%
    select(-.data$buffer, -.data$recent)

  return(result)

}
