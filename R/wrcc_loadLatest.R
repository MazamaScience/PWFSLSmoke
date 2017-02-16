#' @keywords WRCC
#' @export
#' @title Load Recent WRCC Monitoring Data
#' @param baseUrl The location of the meta and data files
#' @description The most recent 45 days of WRCC data are updated in real time
#' at PWFSL and can be loaded with this function.
#' @return A ws_monitor object with WRCC data.
#' @examples
#' \dontrun{
#' wrcc <- wrcc_loadLatest()
#' }

wrcc_loadLatest <- function(baseUrl='https://haze.airfire.org/monitoring/RData/wrcc_pm25_latest.RData') {
  
  ws_monitor <- get(load(url(baseUrl)))
  
  return(ws_monitor)
}
