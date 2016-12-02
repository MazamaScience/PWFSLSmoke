#' @keywords nowcast
#' @export
#' @title Apply Nowcast Algorithm to ws_monitor Object
#' @param ws_monitor ws_monitor object
#' @param version character identify specifying the type of nowcast algorithm to be used
#' @description A Nowcast algorithm is applied to the data in in the ws_monitor object. The 
#' \code{version} argument specifies the minimum weight factor and number of hours to be 
#' considered in the calculation.
#' 
#' Available versions include:
#' \enumerate{
#' \item{pm}{hours=12, weight=0.5}
#' \item{pmAsian}{hours=3, weight=0.1}
#' \item{ozone}{hours=8, weight=NA}
#' }
#' 
#' See the references for details. The default, \code{version='pm'}, is appropriate
#' for typical usage.
#' 
#' @return ws_monitor object with data that have been proccessed by the Nowcast algorithm.
#' @references \url{https://en.wikipedia.org/wiki/Nowcast_(Air_Quality_Index)}
#' @references \url{https://www3.epa.gov/airnow/ani/pm25_aqi_reporting_nowcast_overview.pdf}
#' @references \url{http://aqicn.org/faq/2015-03-15/air-quality-nowcast-a-beginners-guide/}
#' @examples
#' \dontrun{
#' ozone <- monitor_nowcast(ozone, 'ozone')
#' }

# NOTE:  This script is based on the javascript code at: 
# NOET:    https://github.com/chatch/nowcast-aqi/blob/master/nowcast-aqi.js

monitor_nowcast <- function(ws_monitor, version='pm') {
  
  # Set parameters based on version
  if (version =='pm') {
    numHrs <- 12
    weightFactorMin <- 0.5
    digits <- 1
  } else if (version =='pmAsian') {
    numHrs <- 3
    weightFactorMin <- 0.1
    digits <- 1
  } else if (version == 'ozone') {
    numHrs <- 8
    weightFactorMin <- NA
    digits <- 3
  }
  
  # Apply nowcast to each data column in ws_monitor
  n <- ncol(ws_monitor$data)
  ws_monitor$data[,2:n] <- apply(ws_monitor$data[,2:n], 2, function(x) { nowcast(x, numHrs, weightFactorMin) })
  ws_monitor$data[,2:n] <- round(ws_monitor$data[,2:n], digits = digits)
  
  return(ws_monitor)
}

# ----- Helper Functions ------------------------------------------------------

nowcast <- function(x, numHrs, weightFactorMin) {
  for (i in length(x):(numHrs+1)) {
    cByHour <- x[(i-1):(i-numHrs)]
    weightFactor <- weightFactor(cByHour, weightFactorMin)
    sumHourlyByWeightFactor <- sumWeightFactor <- 0
    for (j in 1:numHrs) {
      sumHourlyByWeightFactor <- sumHourlyByWeightFactor + cByHour[j] * weightFactor^(j-1)
      sumWeightFactor <- sumWeightFactor + weightFactor^(j-1)
    }
    x[i] <- sumHourlyByWeightFactor/sumWeightFactor
  }
  return(x)
}

# Calculate the weight factor ('w' in the nowcast formula)
#  cByHour: list of concentrations by hour
#  weightFactorMin (optional): weight factor raised to this min if calculated less then this
weightFactor <- function(cByHour, weightFactorMin) {
  min <- min(cByHour)
  max <- max(cByHour)
  if ( !is.na(weightFactorMin) ) {
    weightFactor <- min/max
    if (is.na(weightFactor)) {
      return(weightFactorMin)
    } else {
      if (weightFactor > weightFactorMin) {
        return(weightFactor)
      } else {
        return(weightFactorMin)
      }
    }
  } else {
    return(min/max)
  }
}

# Example test code
if (FALSE) {
  
  a <- c(1,5,6,5,6,7,6,7,6,5,6,12)
  b <- c(6,5,6,5,6,7,6,7,6,5,6,5)
  c <- c(6,5,6,5,6,NA,6,7,6,5,6,5)
  d <- c(6,5,6,5,6,7,6,7,6,5,NA,5)
  e <- c(6,5,6,5,6,7,6,7,6,5,6,NA)
  
  # More to come...
  
}
