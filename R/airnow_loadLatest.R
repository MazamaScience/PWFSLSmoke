#' @keywords AirNow
#' @export
#' @title Load Most Recent Processed AirNow Monitoring Data
#' @param parameter parameter of interest
#' @param baseUrl base URL for AirNow data
#' @return A \emph{ws_monitor} object with AirNow data.
#' @description Loads pre-generated .RData files containing the most recent AirNow data.
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
#' airnow <- airnow_loadLatest()
#' ca_mean <- monitor_subset(airnow, stateCodes='CA') %>%
#'            monitor_collapse()
#' monitorPlot_timeseries(ca_mean, shadedNight=TRUE)
#' }

airnow_loadLatest <- function(parameter='PM2.5',
                              baseUrl='https://haze.airfire.org/monitoring/AirNow/RData/') {
  
  validParams <- c("PM2.5")
  if ( !parameter %in% validParams ) {
    paramsString <- paste(validParams, collapse=", ")
    stop(paste0("Parameter '", parameter, "' is not supported. Try one of: ", paramsString))
  }
  
  # Create filepath
  filepath <- paste0("latest/airnow_", parameter, "_latest10.RData")
  
  # Define a 'connection' object so we can be sure to close it no matter what happens
  conn <- url(paste0(baseUrl,filepath))
  result <- try( suppressWarnings(ws_monitor <- get(load(conn))),
                 silent=TRUE )
  close(conn)
  
  if ( "try-error" %in% class(result) ) {
    stop(paste0("URL unavailable: ",paste0(baseUrl,filepath)), call.=FALSE)
  }
  
  return(ws_monitor)
  
}
