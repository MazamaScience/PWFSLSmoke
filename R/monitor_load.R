#' @keywords AirNow
#' @keywords AIRSIS
#' @keywords WRCC
#' @export
#' @title Load PM2.5 monitoring data
#' @param startdate Desired start date (integer or character in ymd[hms] format
#'        or \code{POSIXct}).
#' @param enddate Desired end date (integer or character in ymd[hms] format
#'        or \code{POSIXct}).
#' @param monitorIDs Optional vector of monitorIDs.
#' @param parameter Parameter of interest.
#' @param baseUrl Base URL for data files.
#' @param dataDir Local directory containing monitoring data files.
#' @param aqsPreference Preferred data source for AQS data when annual data
#' files are available from both `epa` and `airnow`.
#' @description Loads monitoring data for a given time range. Data from AirNow,
#' AIRSIS and WRCC are combined into a single \emph{ws_monitor} object.
#'
#' Archival datasets are joined with 'daily' and 'latest' datasets as needed to
#' satisfy the requested date range.
#' @note Joining datasets is a computationally expensive task when many monitors
#' are involved. It is highly recommend that \code{monitorIDs} be specified when
#' loading recent data with this function.
#' @seealso \code{\link{loadDaily}}
#' @seealso \code{\link{loadLatest}}
#' @return A \emph{ws_monitor} object with PM2.5 monitoring data.
#' @examples
#' \dontrun{
#' ca <- monitor_load(20170601,20171001) %>% monitor_subset(stateCodes='CA')
#' }

monitor_load <- function(startdate = NULL,
                         enddate = NULL,
                         monitorIDs = NULL,
                         parameter="PM2.5",
                         baseUrl="https://haze.airfire.org/monitoring",
                         dataDir = NULL,
                         aqsPreference = "airnow") {

  # Validate parameters --------------------------------------------------------

  if ( is.null(startdate) ) {
    stop(paste0("Required parameter 'startdate' is missing"))
  }

  if ( is.null(enddate) ) {
    stop(paste0("Required parameter 'enddate' is missing"))
  }

  starttime <- parseDatetime(startdate)
  endtime <- parseDatetime(enddate)

  if ( starttime > endtime ) {
    temptime <- endtime
    endtime <- starttime
    starttime <- temptime
    rm(temptime)
  }

  if ( lubridate::year(starttime) != lubridate::year(endtime) ) {
    stop("Requests covering multiple years are not supported")
  }

  now <- lubridate::now("UTC")
  now_m1 <- now - lubridate::ddays(1)
  now_m10 <- now - lubridate::ddays(10)
  now_m45 <- now - lubridate::ddays(45)

  # Check for very long monitor_join times
  if ( starttime < now_m45 && endtime >= now_m45 && is.null(monitorIDs) ) {
    stop("Requests joining anuual files with 'daily' or 'latest' files can be ",
         "time consuming. Please supply monitorIDs to subset the data.")
  }

  # Load annual data -----------------------------------------------------------

  if ( starttime < now_m45 ) {

    year <- lubridate::year(now)
    annualData <- monitor_loadAnnual(year,
                                     parameter,
                                     baseUrl,
                                     dataDir,
                                     aqsPreference) %>%
      monitor_subset(monitorIDs = monitorIDs, dropMonitors = FALSE)

  }

  # Load daily data ------------------------------------------------------------

  if ( endtime >= now_m45 && starttime < now_m10 ) {

    dailyData <- monitor_loadDaily(parameter,
                                   paste0(baseUrl,'/latest/RData'),
                                   dataDir) %>%
      monitor_subset(monitorIDs = monitorIDs, dropMonitors = FALSE)

  }

  # Load latest data -----------------------------------------------------------

  if ( endtime >= now_m1 ) {

    latestData <- monitor_loadLatest(parameter,
                                     paste0(baseUrl,'/latest/RData'),
                                     dataDir) %>%
      monitor_subset(monitorIDs = monitorIDs, dropMonitors = FALSE)

  }

  # Join ws_monitor objects ----------------------------------------------------

  if ( exists("annualData") ) {
    ws_monitor <- annualData
  }

  if ( exists("dailyData") ) {
    if ( exists("ws_monitor") ) {
      # Hide "No matching monitors found" warning messages
      suppressWarnings({
        ws_monitor <- monitor_join(ws_monitor, dailyData, monitorIDs)
      })
    } else {
      ws_monitor <- dailyData
    }
  }

  if ( exists("latestData") ) {
    if ( exists("ws_monitor") ) {
      # Hide "No matching monitors found" warning messages
      suppressWarnings({
        ws_monitor <- monitor_join(ws_monitor, latestData, monitorIDs)
      })
    } else {
      ws_monitor <- latestData
    }
  }

  # Subset to the requested time range
  ws_monitor <- monitor_subset(ws_monitor, tlim=c(starttime, endtime))

  return(ws_monitor)

}
