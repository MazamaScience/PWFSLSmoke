#' @keywords AirNow
#' @export
#' @title Load Recent AirNow Monitoring Data
#' @param baseUrl location of the AirNow latest data file
#' @param file name of the AirNow latest data file
#' @description The most recent 45 days of AirNow data are updated in real time
#' at PWFSL and can be loaded with this function.
#' @return A ws_monitor object with AirNow data.
#' @examples
#' \dontrun{
#' airnow <- airnow_loadLatest()
#' }

airnow_loadLatest <- function(baseUrl='https://haze.airfire.org/monitoring/RData/',
                              file='airnow_pm25_latest.RData') {
  
  # TODO:  Change to use baseUrl when file starts getting created.
  # ws_monitor <- get(load(url(baseUrl)))
  
  baseUrl <- "https://haze.airfire.org/monitoring/RData/"
  meta <- get(load(url(paste0(baseUrl,'AirNowTech_PM2.5_SitesMetadata.RData'))))
  data <- get(load(url(paste0(baseUrl,'AirNowTech_PM2.5_LATEST.RData'))))
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class=c("ws_monitor", "list"))
  
  return(ws_monitor)
}
