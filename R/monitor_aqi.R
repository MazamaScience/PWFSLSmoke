#' @keywords ws_monitor
#' @export
#' @title Calculate hourly AQI values
#' @param ws_monitor \emph{ws_monitor} object
#' @param aqiParameter parameter type; used to define reference breakpointsTable
#' @param nowcastVersion character identity specifying the type of nowcast algorithm to be used
#' @param includeShortTerm calcluate preliminary values starting with the 2nd hour
#' @description Nowcast and AQI algorithms are applied to the data in the ws_monitor object.
#' @references \url{https://www3.epa.gov/airnow/aqi-technical-assistance-document-may2016.pdf}
#' @references \url{https://archive.epa.gov/ttn/ozone/web/pdf/airqual.pdf}
#' @references \url{https://www.ecfr.gov/cgi-bin/retrieveECFR?n=40y6.0.1.1.6#se40.6.58_150} (see Appendix G)
#' @examples
#' \dontrun{
#' N_M <- monitor_subset(Northwest_Megafires, tlim=c(20150815,20150831))
#' ws_monitor <- N_M
#' aqi <- monitor_aqi(ws_monitor)
#' monitorPlot_timeseries(aqi, monitorID=aqi$meta$monitorID[1], ylab="PM25 AQI")
#' }

# NOTE: set up with argument to handle pm25 only; but write code to handle other pollutants if we ever get there

monitor_aqi <- function(ws_monitor, aqiParameter='pm25', nowcastVersion='pm', includeShortTerm=FALSE) {
  
  # assign breakpoints
  breakpointsTable <- .assignBreakpointsTable(aqiParameter)
  
  # calculate NowCast values
  ws_monitor <- monitor_nowcast(ws_monitor, version = nowcastVersion, includeShortTerm = includeShortTerm)
  
  # pull out data for AQI calculation
  n <- ncol(ws_monitor$data)
  data <- ws_monitor$data[2:n]
  
  # TODO: include checks to ensure values are appropriately truncated
  data <- round(data, 1)
  
  # for each datapoint find the row index corresponding to the breakpoints that contain the concentration
  rowIndex <- apply(X=data, MARGIN=2, FUN=findInterval, vec=breakpointsTable$rangeHigh, left.open=TRUE)
  rowIndex <- rowIndex + 1
  
  # From 40 CFR 58 Appendix G.12.ii:
  #  If the concentration is larger than the highest breakpoint in Table 2
  #  then you may use the last two breakpoints in Table 2 when you apply Equation 1.
  rowIndex[rowIndex > nrow(breakpointsTable)] <- nrow(breakpointsTable)
  
  # assign breakpoints and corresponding index values
  I_Hi <- breakpointsTable$aqiHigh[rowIndex]
  I_Lo <- breakpointsTable$aqiLow[rowIndex]
  BP_Hi <- breakpointsTable$rangeHigh[rowIndex]
  BP_Lo <- breakpointsTable$rangeLow[rowIndex]
  
  # apply Equation 1 from 40 CFR 58 Appendix G and round to the nearest integer
  I_p <- (I_Hi-I_Lo)/(BP_Hi-BP_Lo)*(data-BP_Lo)+I_Lo
  I_p <- round(I_p, 0)
  
  # For negative concentrations, set AQI = 0
  # NOTE: This functionality is assumed; it is not explicity referenced in 40 CFR 58
  I_p[I_p<0] <- 0
  
  ws_monitor$data[2:n] <- I_p
  
  return(ws_monitor)
  
}

# ----- helper function --------------------

.assignBreakpointsTable <- function(parameter="pm25") {
  
  if ( parameter == "pm25") {
    breakpointsTable <- data.frame(rangeLow=c(0.0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5),
                                   rangeHigh=c(12.0, 35.4, 55.4, 150.4, 250.4, 350.4, 500.4),
                                   aqiLow=c(0, 51, 101, 151, 201, 301, 401),
                                   aqiHigh=c(50, 100, 150, 200, 300, 400, 500))
  } else {
    stop("only pm25 currently supported")
  }
  
  return(breakpointsTable)
  
}
