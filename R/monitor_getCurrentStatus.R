


monitor_getCurrentStatus <- function(ws_monitor,
                                     endTime = lubridate::now("UTC"),
                                     maxLatency = 6) {


# Sanity checks -----------------------------------------------------------

  if (monitor_isMonitor(ws_monitor)) stop("Not a valid `ws_monitor` object.")
  if (monitor_isEmpty(ws_monitor)) stop("`ws_monitor` object contains zero monitors.")

  # parseDateTime will fail if it produces NA
  endTime <- parseDatetime(endTime) %>%
    lubridate::floor_date(unit = "hour")

  endTimeInclusive <- endTime - lubridate::dhours(1)
  startTime <- min(ws_monitor[["data"]][["datetime"]])

  ws_monitor <- ws_monitor %>%
    monitor_subset(tlim = c(startTime, endTimeInclusive))

  if (monitor_isEmpty(ws_monitor)) {
    stop("ws_monitor object contains zero monitors with data from before ", endTime)
  }


# Calculate data latency --------------------------------------------------

  ## TODO: how are different timezones handled when compared against endTime
  #  and processTime?

  processTime <- lubridate::now("UTC")

  latencyTbl <- ws_monitor %>%
    monitor_toTidy() %>%
    group_by(.data$monitorID) %>%
    filter(!is.na(.data$pm25)) %>%
    summarize(lastUpdate = max(.data$datetime)) %>%
    ungroup(.data$monitorID) %>%
    mutate(latency = endTimeInclusive - .data$lastUpdate) %>%
    filter(.data$latency <= lubridate::dhours(maxLatency))


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

}


# DEBUGGING
if (FALSE) {

  ws_monitor <- monitor_loadLatest() %>%
    monitor_subset(stateCodes = "WA")
  endTime <-  lubridate::now("UTC")
  maxLatency <-  6

}
