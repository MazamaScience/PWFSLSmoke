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
  
  
  pmTable <- data.frame(rangeLow=c(0.0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5),
                        rangeHigh=c(12.0, 35.4, 55.4, 150.4, 250.4, 350.4, 500.4),
                        aqiLow=c(0, 51, 101, 151, 201, 301, 401),
                        aqiHigh=c(50, 100, 150, 200, 300, 400, 500))
  

  data <- c(74, 155, 259)
  data <- ws_monitor$data[,2]
  C_p <- 75
  
  getAQI <- function(C_p) {
    
    rowIndex <- which(pmTable$rangeLow < C_p & C_p < pmTable$rangeHigh)
    
    if ( length(rowIndex)==0 ) {
      print("wah wah")
      AQI_val <- NA
    } else {
      BP_Hi <- pmTable$rangeHigh[rowIndex]
      BP_Lo <- pmTable$rangeLow[rowIndex]
      I_Hi <- pmTable$aqiHigh[rowIndex]
      I_Lo <- pmTable$aqiLow[rowIndex]
      
      I_p <- (I_Hi-I_Lo)/(BP_Hi-BP_Lo)*(C_p-BP_Lo)+I_Lo
      
      
    }
  }
  

  

  return(ws_monitor)
  
}

# # 40 CFR 58 Equation 1
# .eqn1_40CFR58_calcAQI <- function(C_p, BP_Hi, BP_Lo, I_Hi, I_Lo) {
#   I_p <- (I_Hi-I_Lo)/(BP_Hi-BP_Lo)*(C_p-BP_Lo)+I_Lo
#   return(I_p)
# }

# .eqn1_40CFR58_calcAQI(C_p, BP_Hi, BP_Lo, I_Hi, I_Lo)
