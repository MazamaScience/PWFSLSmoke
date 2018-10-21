#' @keywords AirNow
#' @export
#' @title Load most recent AirNow monitoring data
#' @param parameter Parameter of interest.
#' @param baseUrl Base URL for 'daily' AirNow data files.
#' @param dataDir Local directory containing 'daily' data files.
#' @return A \emph{ws_monitor} object with AirNow data.
#' @description Loads pre-generated .RData files containing the most recent
#' AirNow data.
#'
#' If \code{dataDir} is defined, data will be loaded from this local
#' dirctory. Otherwise, data will be loaded from the monitoring data repository
#' maintained by PWFSL.
#'
#' The  files loaded by this function are updated multiple times an hour and
#' contain data for the previous 10 days.
#'
#' For daily updates covering the most recent 45 days, use \code{airnow_loadDaily()}.
#'
#' For data extended more than 45 days into the past, use \code{airnow_load()}.
#'
#' AirNow parameters include the following:
#' \enumerate{
# #' \item{BARPR}
# #' \item{BC}
# #' \item{CO}
# #' \item{NO}
# #' \item{NO2}
# #' \item{NO2Y}
# #' \item{NO2X}s
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
#' Avaialble RData and associated log files can be seen at:
#' \href{https://haze.airfire.org/monitoring/AirNow/RData/latest}{https://haze.airfire.org/monitoring/AirNow/RData/latest}
#' @seealso \code{\link{airnow_load}}
#' @seealso \code{\link{airnow_loadDaily}}
#' @examples
#' \dontrun{
#' airnow_loadLatest() %>%
#'   monitor_subset(stateCodes=CONUS) %>%
#'   monitorMap()
#' }

airnow_loadLatest <- function(parameter = 'PM2.5',
                              baseUrl = 'https://haze.airfire.org/monitoring/latest/RData',
                              dataDir = NULL) {

  # Validate parameter
  validParams <- c("PM2.5")
  if ( !parameter %in% validParams ) {
    paramsString <- paste(validParams, collapse=", ")
    stop(paste0("'", parameter,
                "' is not a supported parameter. Use 'parameter = ",
                paramsString, "'"), call.=FALSE)
  }

  # Create filename according to the PWFSLSmoke naming scheme
  filename <- paste0("airnow_", parameter, "_latest10.RData")

  ws_monitor <- loadDataFile(filename, baseUrl, dataDir)

  return(ws_monitor)

}
