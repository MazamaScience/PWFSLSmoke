#' @keywords AirNow
#' @export
#' @title Load annual AirNow monitoring data
#' @param year Desired year (integer or character representing YYYY).
#' @param parameter Parameter of interest.
#' @param baseUrl Base URL for 'annual' AirNow data files.
#' @param dataDir Local directory containing 'annual' data files.
#' @return A \emph{ws_monitor} object with AirNow data.
#' @description Loads pre-generated .RData files containing annual
#' AirNow data.
#'
#' If \code{dataDir} is defined, data will be loaded from this local
#' dirctory. Otherwise, data will be loaded from the monitoring data repository
#' maintained by PWFSL.
#'
#' The annual files loaded by this function are updated on the 15'th of each
#' month and cover the period from the beginning of the year to the end of the
#' last month.
#'
#' For data during the last 45 days, use \code{airnow_loadDaily()}.
#'
#' For the most recent data, use \code{airnow_loadLatest()}.
#'
#' AirNow parameters include the following:
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
#' Available AirNow RData and associated log files can be seen at:
#' \href{https://haze.airfire.org/monitoring/AirNow/RData}{https://haze.airfire.org/monitoring/AirNow/RData}
#' @seealso \code{\link{airnow_loadDaily}}
#' @seealso \code{\link{airnow_loadLatest}}
#' @examples
#' \dontrun{
#' airnow_loadAnnual(2017) %>%
#'   monitor_subset(stateCodes='MT', tlim=c(20170701,20170930)) %>%
#'   monitor_dailyStatistic() %>%
#'   monitor_timeseriesPlot(style = 'gnats', ylim=c(0,300), xpd=NA)
#'   addAQIStackedBar()
#'   addAQILines()
#'   title("Montana 2017 -- AirNow Daily Average PM2.5")
#' }

airnow_loadAnnual <- function(year = NULL,
                              parameter = 'PM2.5',
                              baseUrl = 'https://haze.airfire.org/monitoring',
                              dataDir = NULL) {

  # Validate parameters --------------------------------------------------------

  if ( is.null(year) ) {
    stop("Required parameter 'year' is missing.")
  }
  year <- as.numeric(year)

  validParams <- c("PM2.5")
  if ( !parameter %in% validParams ) {
    paramsString <- paste(validParams, collapse=", ")
    stop(paste0("'", parameter,
                "' is not a supported parameter. Use 'parameter = ",
                paramsString, "'"), call.=FALSE)
  }

  if ( year < 2016 ) {
    stop("PWFSL has no annual data files for AirNow prior to 2016.")
  }

  # Load the data --------------------------------------------------------------


  # Create URL and filename according to the PWFSLSmoke naming scheme
  baseUrl <- paste0(baseUrl, '/AirNow/RData/', year)
  filename <- paste0("airnow_", parameter, "_", year, ".RData")

  ws_monitor <- MazamaCoreUtils::loadDataFile(filename, baseUrl, dataDir)

  return(ws_monitor)

}
