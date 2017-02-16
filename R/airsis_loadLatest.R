#' @keywords AIRSIS
#' @export
#' @title Load Recent AIRSIS Monitoring Data
#' @param baseUrl The location of the meta and data files
#' @description The most recent 45 days of AIRSIS data are updated in real time
#' at PWFSL and can be loaded with this function.
#' @return A ws_monitor object with AIRSIS data.
#' @examples
#' \dontrun{
#' airsis <- airsis_loadLatest()
#' }

airsis_loadLatest <- function(baseUrl='https://haze.airfire.org/monitoring/RData/airsis_pm25_latest.RData') {
  
  ws_monitor <- get(load(url(baseUrl)))
  
  return(ws_monitor)
}
