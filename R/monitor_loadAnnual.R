#' @keywords AirNow
#' @keywords AIRSIS
#' @keywords WRCC
#' @export
#' @title Load annual PM2.5 monitoring data
#' @param year Desired year (integer or character representing YYYY).
#' @param parameter Parameter of interest.
#' @param baseUrl Base URL for 'daily' AirNow data files.
#' @param dataDir Local directory containing 'daily' data files.
#' @return A \emph{ws_monitor} object with PM2.5 monitoring data.
#' @description Wrapper function to load and combine annual
#' data from AirNow, AIRSIS and WRCC.
#'
#' If \code{dataDir} is defined, data will be loaded from this local
#' dirctory. Otherwise, data will be loaded from the monitoring data repository
#' maintained by PWFSL.
#'
#' The annual files loaded by this function are updated on the 15'th of each
#' month and cover the period from the beginning of the year to the end of the
#' last month.
#'
#' For data during the last 45 days, use \code{monitor_loadDaily()}.
#'
#' For the most recent data, use \code{monitor_loadLatest()}.
#'
#' Currently supported parameters include the following:
#' \enumerate{
# #' \item{BARPR}
# #' \item{BC}
# #' \item{CO}
# #' \item{NO}
# #' \item{NO2}
# #' \item{NO2Y}
# #' \item{NO2X}
# #' \item{NOX}
# #' \item{NOOY}
# #' \item{OC}
# #' \item{OZONE}
# #' \item{PM10}
#' \item{PM2.5}
# #' \item{PRECIP}
# #' \item{RHUM}
# #' \item{SO2}
# #' \item{SRAD}
# #' \item{TEMP}
# #' \item{UV-AETH}
# #' \item{WD}
# #' \item{WS}
#' }
#'
#' Avaialble RData files can be seen at:
#' \href{https://haze.airfire.org/monitoring/}{https://haze.airfire.org/monitoring/}
#' @seealso \code{\link{monitor_loadDaily}}
#' @seealso \code{\link{monitor_loadLatest}}
#' @examples
#' \dontrun{
#' monitor_loadAnnual(2014) %>%
#'   monitor_subset(stateCodes='MT', tlim=c(20140801,20140901)) %>%
#'   monitorMap()
#' }

monitor_loadAnnual <- function(year = NULL,
                               parameter='PM2.5',
                               baseUrl='https://haze.airfire.org/monitoring',
                               dataDir = NULL) {

  # Validate parameters --------------------------------------------------------

  if ( is.null(year) ) {
    stop("Required parameter 'year' is missing.")
  } else if ( year < 1998 ) {
    stop("PWFSL has processed no data prior to 1998.")
  }
  year <- as.numeric(year)

  # Load data ------------------------------------------------------------------

  # Only add to the monitorsList if data are available
  monitorsList <- list()

  # AirNow annual files start in 2016
  if ( as.numeric(year) >= 2016 ) {
    monitorsList$airnow <- airnow_loadAnnual(year, parameter, baseUrl, dataDir)
  }

  # AIRSIS annual files start in 2004
  if ( as.numeric(year >= 2004) ) {
    monitorsList$airsis <- airsis_loadAnnual(year, parameter, baseUrl, dataDir)
  }

  # WRCC annual files start in 2010
  if ( as.numeric(year >= 2010) ) {
    monitorsList$wrcc <- wrcc_loadAnnual(year, parameter, baseUrl, dataDir)
  }

  # EPA annual files
  lastYear <- lubridate::now() %>% lubridate::year() - 1
  if ( year %in% 2008:lastYear) {
    monitorsList$epa_88101 <- epa_loadAnnual(year, "88101", baseUrl, dataDir)
  }
  if ( year %in% 1998:lastYear) {
    monitorsList$epa_88502 <- epa_loadAnnual(year, "88502", baseUrl, dataDir)
  }

  ws_monitor <- monitor_combine(monitorsList)

  return(ws_monitor)

}
