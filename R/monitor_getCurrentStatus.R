
monitor_getCurrentStatus <- function(ws_monitor,
                                     endTime = lubridate::now("UTC"),
                                     maxLatency = 6,
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
    monitor_nowcast() %>%
    monitor_extractData() %>%
    as_tibble()


# Calculate monitor latency -----------------------------------------------

  ## TODO:
  #  how are different timezones handled when compared against endTime
  #  and processTime?

  processTime <- lubridate::now("UTC")

  lastValidTimeIndex <- ws_data %>%
    arrange(.data$datetime) %>%
    select(-.data$datetime) %>%
    purrr::map_int(~max(which(!is.na(.x))))

  lastValidTime <- ws_data[["datetime"]][lastValidTimeIndex]


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
      `lastValidTime` = lastValidTime,
      `latency` = difftime(endTimeInclusive, lastValidTime, units = "hour")
    ) %>%
    filter(.data$latency <= lubridate::hours(maxLatency))


# Add summary data --------------------------------------------------------

  ## Summary data:
  #
  #  * lastValid_nowcast_1hr --
  #  * lastValid_PM2.5_1hr --
  #  * lastValid_PM2.5_3hr --
  #  * yesterday_PM2.5_24hr --

  lastValid_nowcast_1hr <- .lastValid_n_hr_avg(nowcast_data, lastValidTimeIndex, 1)
  lastValid_PM2.5_1hr <- .lastValid_n_hr_avg(ws_data, lastValidTimeIndex, 1)
  lastValid_PM2.5_3hr <- .lastValid_n_hr_avg(ws_data, lastValidTimeIndex, 3)
  yesterday_PM2.5_24hr <- .yesterday_AQI_24hr(ws_monitor, endTime)


  summaryData <- yesterday_PM2.5_24hr %>%
    left_join(lastValid_nowcast_1hr, by = "monitorID") %>%
    left_join(lastValid_PM2.5_1hr, by = "monitorID") %>%
    left_join(lastValid_PM2.5_3hr, by = "monitorID")



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



# Return output -----------------------------------------------------------

  # Note: option to not append meta info?

  return(currentStatus)

}


# DEBUGGING
if (FALSE) {

  ws_monitor <- monitor_loadLatest() %>%
    monitor_subset(stateCodes = "WA")
  endTime <-  lubridate::now("UTC")
  maxLatency <-  6

}


# Helper functions --------------------------------------------------------

.lastValid_n_hr_avg <- function(data, lastValidTimeIndex, n) {

  get_nHrAvg <- function(monitorData, latestIndex, n) {

    if (latestIndex < n) return(NA)

    avg <- monitorData %>%
      magrittr::extract((latestIndex - n + 1):latestIndex) %>%
      mean(na.rm = TRUE) %>%
      round(digits = 1)

    if (is.nan(avg)) return(NA)
    else return(avg)

  }

  avgData <-
    purrr::map2_dfc(
      select(data, -.data$datetime), lastValidTimeIndex,
      ~get_nHrAvg(.x, .y, n)
    ) %>%
    tidyr::gather("monitorID", "avg_pm25")

  return(avgData)

}


.yesterday_AQI_24hr <- function(ws_monitor, endTimeUTC) {

  get_previousDayStart <- function(endTimeUTC, timezone) {

    previousDayStart <- endTime %>%
      lubridate::with_tz(timezone) %>%
      lubridate::floor_date(unit = "day") %>%
      magrittr::subtract(lubridate::ddays(1))

    return(previousDayStart)

  }

  get_previousDayAQI <- function(ws_data, previousDayStart) {

    aqiData <- ws_data %>%
      filter(.data$datetime == !!enquo(previousDayStart)) %>%
      select(-.data$datetime)

    return(aqiData)

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
    tidyr::gather("monitorID", "AQI") %>%
    mutate(`AQI` = round(.data$AQI, 1))

  return(previousDayAQI)

}
