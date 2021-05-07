library(dplyr)
library(PWFSLSmoke)

summary <- monitor_generateSummary(
  agency = "airsis",
  filePath = "~/test_monitor_generateSummary.csv"
)

monitor_generateSummary <- function(
  agency = NULL,
  filePath = NULL
) {

  if ( is.null(agency) )
    stop("Parameter 'agency' must not be null")

  if ( is.null(filePath) )
    stop("Parameter 'filePath' must not be null")

  agency_lower <- tolower(agency)

  if ( agency_lower == "airsis" ) {
    statusTbl <-
      airsis_loadLatest() %>%
      monitor_getCurrentStatus()
  } else if ( agency_lower == "airnow") {
    statusTbl <-
      airnow_loadLatest() %>%
      monitor_getCurrentStatus()
  } else if ( agency_lower == "wrcc" ) {
    statusTbl <-
      wrcc_loadLatest() %>%
      monitor_getCurrentStatus()
  } else {
    stop("Parameter 'agency' must be one of 'airsis', 'airnow', or 'wrcc'.")
  }

  statusTbl$agency <-
    stringr::str_replace(statusTbl$telemetryAggregator, paste0("\\.", agency_lower), "") %>%
    toupper

  # dropped columns:
  # siteID, pwfslID, instrumentID : that metadata is combined and stored in monitorID
  # telemetryAggregator : will be cut into 'agency'
  # monitorUrl : I don't think this is necessary
  # processingTime, endTime, last_validTime, last_latency, previous_validTime,
  # previous_latency, lats_validLocalTimestamp, previous_validLocalTimestamp : I don't think these are necessary
  #
  statusTbl <- statusTbl %>%
    dplyr::select(
      monitorID, longitude, latitude, elevation, timezone, countryCode, stateCode,
      siteName, agencyName, countyName, msaName, monitorType, aqsID, pwfslDataIngestSource,
      agency, last_latency, previous_latency, yesterday_pm25_24hr, last_nowcast_1hr,
      last_pm25_1hr, last_pm25_3hr, last_nowcastLevel, previous_nowcastLevel,
      NR6, NEW6, USG6, U6, VU6, HAZ6, MOD6, MAL6
    )

  statusTbl$preparedAt <- lubridate::now()

  tryCatch({
    if (file.exists(filePath) ) {
     previousData <- readr::read_csv(filePath, col_types = readr::cols())
     statusTbl <- rbind(previousData, statusTbl)
    }
    write.csv(statusTbl, filePath, row.names = FALSE)
  }, warning = function(warn) {
    warning("Invalid path name.")
  }, error   = function(err) {
    stop(paste0("Error"))
  })

  return(statusTbl)

}
