#' @keywords AirNow
#' @keywords AIRSIS
#' @keywords WRCC
#' @export
#' @title Load recent PM2.5 monitoring data
#' @param parameter Parameter of interest.
#' @param baseUrl Base URL for 'daily' AirNow data files.
#' @param dataDir Local directory containing 'daily' data files.
#' @return A \emph{ws_monitor} object with PM2.5 monitoring data.
#' @description Wrapper function to load and combine recent
#' data from AirNow, AIRSIS and WRCC:
#'
#' \preformatted{
#' airnow <- airnow_loadDaily()
#' airsis <- airsis_loadDaily()
#' wrcc <- wrcc_loadDaily()
#' ws_monitor <- monitor_combine(list(airnow, airsis, wrcc))
#' }
#'
#' If \code{dataDir} is defined, data will be loaded from this local
#' dirctory. Otherwise, data will be loaded from the monitoring data repository
#' maintained by PWFSL.
#'
#' The daily files loaded by this function are updated once a day, shortly
#' after midnight and contain data for the previous 45 days.
#'
#' For the most recent data, use \code{monitor_loadLatest()}.
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
#' @seealso \code{\link{monitor_loadLatest}}
#' @seealso \code{\link{monitor_loadAnnual}}
#' @examples
#' \dontrun{
#' monitor_loadDaily() %>%
#'   monitor_subset(stateCodes=CONUS) %>%
#'   monitor_map()
#' }

monitor_loadDaily <- function(parameter='PM2.5',
                              baseUrl='https://haze.airfire.org/monitoring/latest/RData',
                              dataDir = NULL) {

  airnow <- airnow_loadDaily(parameter, baseUrl, dataDir)
  airsis <- airsis_loadDaily(parameter, baseUrl, dataDir)
  wrcc <- wrcc_loadDaily(parameter, baseUrl, dataDir)
  ws_monitor <- monitor_combine(list(airnow, airsis, wrcc))

  return(ws_monitor)

}
