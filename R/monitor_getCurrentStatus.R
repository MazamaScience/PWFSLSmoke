


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


}
