
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
