#' @keywords ws_monitor
#' @export
#' @title Calculate hourly AQI values
#' @param ws_monitor \emph{ws_monitor} object
#' @param aqiParameter parameter type
#' @param nowcastVersion character identity specifying the type of nowcast algorithm to be used
#' @param includeShortTerm calcluate preliminary values starting with the 2nd hour
#' @description Nowcast and AQI algorithms are applied to the data in in the ws_monitor object.
#' @references \url{https://www3.epa.gov/airnow/aqi-technical-assistance-document-may2016.pdf}
#' @references \url{https://archive.epa.gov/ttn/ozone/web/pdf/airqual.pdf}
#' @references \url{https://www.ecfr.gov/cgi-bin/retrieveECFR?n=40y6.0.1.1.6#se40.6.58_150} (see Appendix G)

# NOTE: set up with argument to handle pm25 only; but write code to handle other pollutants if we ever get there

monitor_aqi <- function(ws_monitor, aqiParameter='pm25', nowcastVersion='pm', includeShortTerm=FALSE) {
  
  ws_monitor <- monitor_nowcast(ws_monitor, version=nowcastVersion, includeShortTerm=includeShortTerm)
  
  
  # TODO: implement AQI calculation
  

  
  
  
  return(ws_monitor)
  
}