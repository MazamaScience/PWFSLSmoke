#' @keywords AirNow
#' @keywords AIRSIS
#' @keywords WRCC
#' @export
#' @title Load Recent PM2.5 Monitoring Data
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
#' The daily files are generated once a day, shortly after midnight and contain data for the
#' previous 45 days. 
#' 
#' For the most recent data, use \code{loadLatest()}.
#' 
#' Avaialble RData and associated log files can be seen at:
#' \href{https://haze.airfire.org/monitoring/latest/RData/}{https://haze.airfire.org/monitoring/latest/RData/}
#' @seealso \code{\link{loadLatest}}
#' @return A \emph{ws_monitor} object with PM2.5 monitoring data.
#' @examples
#' \dontrun{
#' ca <- loadDaily() %>% monitor_subset(stateCodes='CA')
#' }

loadDaily <- function() {
  
  airnow <- airnow_loadDaily()
  airsis <- airsis_loadDaily()
  wrcc <- wrcc_loadDaily()
  ws_monitor <- monitor_combine(list(airnow, airsis, wrcc))
  
  return(ws_monitor)
  
}
