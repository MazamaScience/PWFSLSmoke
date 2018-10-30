#' @keywords AirNow
#' @keywords AIRSIS
#' @keywords WRCC
#' @export
#' @title Load Recent PM2.5 Monitoring Data
#' @description Wrapper function to load and combine the most recent
#' data from AirNow, AIRSIS and WRCC:
#'
#' \preformatted{
#' airnow <- airnow_loadLatest()
#' airsis <- airsis_loadLatest()
#' wrcc <- wrcc_loadLatest()
#' ws_monitor <- monitor_combine(list(airnow, airsis, wrcc))
#' }
#'
#' Avaialble RData and associated log files can be seen at:
#' \href{https://haze.airfire.org/monitoring/latest/RData/}{https://haze.airfire.org/monitoring/latest/RData/}
#' @seealso \code{\link{airsis_loadDaily}}
#' @return A \emph{ws_monitor} object with PM2.5 monitoring data.
#' @examples
#' \dontrun{
#' ca <- loadLatest() %>% monitor_subset(stateCodes='CA')
#' }

loadLatest <- function() {

  # TODO:  .Deprecate() this function at some point

  # airnow <- airnow_loadLatest()
  # airsis <- airsis_loadLatest()
  # wrcc <- wrcc_loadLatest()
  # ws_monitor <- monitor_combine(list(airnow, airsis, wrcc))

  ws_monitor <- monitor_loadLatest()

  return(ws_monitor)

}
