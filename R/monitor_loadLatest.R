#' @keywords AirNow
#' @keywords AIRSIS
#' @keywords WRCC
#' @export
#' @title Load most recent PM2.5 monitoring data
#' @param parameter Parameter of interest.
#' @param baseUrl Base URL for 'daily' AirNow data files.
#' @param dataDir Local directory containing 'daily' data files.
#' @return A \emph{ws_monitor} object with PM2.5 monitoring data.
#' @description Wrapper function to load and combine recent
#' data from AirNow, AIRSIS and WRCC:
#'
#' \preformatted{
#' airnow <- airnow_loadLatest()
#' airsis <- airsis_loadLatest()
#' wrcc <- wrcc_loadLatest()
#' ws_monitor <- monitor_combine(list(airnow, airsis, wrcc))
#' }
#'
#' If \code{dataDir} is defined, data will be loaded from this local
#' dirctory. Otherwise, data will be loaded from the monitoring data repository
#' maintained by PWFSL.
#'
#' The files loaded by this function are updated multiple times an hour and
#' contain data for the previous 10 days.
#'
#' For daily updates covering the most recent 45 days, use \code{monitor_loadDaily()}.
#'
#' For data extended more than 45 days into the past, use \code{monitor_load()}.
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
#' \href{https://haze.airfire.org/monitoring/latest/RData/}{https://haze.airfire.org/monitoring/latest/RData/}
#' @seealso \code{\link{monitor_load}}
#' @seealso \code{\link{monitor_loadAnnual}}
#' @seealso \code{\link{monitor_loadDaily}}
#' @examples
#' \dontrun{
#' monitor_loadLatest() %>%
#'   monitor_subset(stateCodes=CONUS) %>%
#'   monitorMap()
#' }

monitor_loadLatest <- function(parameter='PM2.5',
                               baseUrl='https://haze.airfire.org/monitoring/latest/RData/',
                               dataDir = NULL) {

  airnow <- airnow_loadLatest(parameter, baseUrl, dataDir)
  airsis <- airsis_loadLatest(parameter, baseUrl, dataDir)
  wrcc <- wrcc_loadLatest(parameter, baseUrl, dataDir)
  ws_monitor <- monitor_combine(list(airnow, airsis, wrcc))

  return(ws_monitor)

}
