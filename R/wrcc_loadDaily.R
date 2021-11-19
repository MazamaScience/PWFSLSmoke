#' @keywords WRCC
#' @export
#' @title Load recent WRCC monitoring data
#' @param parameter Parameter of interest.
#' @param baseUrl Base URL for 'daily' AirNow data files.
#' @param dataDir Local directory containing 'daily' data files.
#' @return A \emph{ws_monitor} object with WRCC data.
#' @description Loads pre-generated .RData files containing recent
#' WRCC data.
#'
#' If \code{dataDir} is defined, data will be loaded from this local
#' dirctory. Otherwise, data will be loaded from the monitoring data repository
#' maintained by PWFSL.
#'
#' The daily files loaded by this function are updated once a day, shortly
#' after midnight and contain data for the previous 45 days.
#'
#' For the most recent data, use \code{wrcc_loadLatest()}.
#'
#' For data extended more than 45 days into the past, use \code{wrcc_loadAnnual()}.
#'
#' WRCC parameters include the following:
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
#' Avaialble WRCC RData and associated log files can be seen at:
#' \href{https://haze.airfire.org/monitoring/WRCC/RData/latest/}{https://haze.airfire.org/monitoring/WRCC/RData/latest/}
#' @seealso \code{\link{wrcc_loadAnnual}}
#' @seealso \code{\link{wrcc_loadLatest}}
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' wrcc_loadDaily() %>%
#'   monitor_subset(stateCodes=CONUS) %>%
#'   monitor_map()
#'
#' }, silent = FALSE)
#' }

wrcc_loadDaily <- function(
  parameter = 'PM2.5',
  baseUrl = 'https://haze.airfire.org/monitoring/latest/RData',
  dataDir = NULL
) {

  # Validate parameter
  validParams <- c("PM2.5")
  if ( !parameter %in% validParams ) {
    paramsString <- paste(validParams, collapse=", ")
    stop(paste0("'", parameter,
                "' is not a supported parameter. Use 'parameter = ",
                paramsString, "'"), call.=FALSE)
  }

  # Create filename according to the PWFSLSmoke naming scheme
  filename <- paste0("wrcc_", parameter, "_latest45.RData")

  ws_monitor <- MazamaCoreUtils::loadDataFile(filename, baseUrl, dataDir)

  return(ws_monitor)

}
