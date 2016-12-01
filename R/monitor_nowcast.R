#' @keywords nowcast
#' @export
#' @title applies nowcast algorithm to ws_monitor objects
#' @param ws_monitor the monitor object that nowcast will be applied to
#' @param version determines the minimum weight factor and number of hours to be calculated
#' @description Applies nowcast algorithm to ws_monitor objects, the version option 
#' will use corresponding minimum weight factor and number of hours for different air-quality data. 
#' This script is based on the javascript: 
#' https://github.com/chatch/nowcast-aqi/blob/master/nowcast-aqi.js
#' 
#' Available versions are: 'pm', 'pmAsian', 'ozone'
#' @return ws_monitor object with data that have been proccessed by nowcast
#' @examples
#' \dontrun{
#' ozone <- monitor_nowcast(ozone, 'ozone')
#' }
#' 

weightFactor <- function(cByHour, weightFactorMin) {
  min <- min(cByHour)
  max <- max(cByHour)
  if(!is.na(weightFactorMin)) {
    weightFactor <- min/max
    if (is.na(weightFactor)) {
      return(weightFactorMin)
    } else if (weightFactor > weightFactorMin) {
      return(weightFactor)
    } else {
      return(weightFactorMin)
    }
  } else {
    return(min/max)
  }
}

nowcast <- function(oneMonitorData, numHrs, weightFactorMin) {
  for(i in length(oneMonitorData):(numHrs+1)){
    cByHour <- oneMonitorData[(i-1):(i-numHrs)]
    weightFactor <- weightFactor(cByHour, weightFactorMin)
    sumHourlyByWeightFactor <- sumWeightFactor <- 0
    for (j in 1:numHrs) {
      sumHourlyByWeightFactor <- sumHourlyByWeightFactor + cByHour[j] * weightFactor^(j-1)
      sumWeightFactor <- sumWeightFactor + weightFactor^(j-1)
    }
    oneMonitorData[i] <- sumHourlyByWeightFactor/sumWeightFactor
  }
  return(oneMonitorData)
}

monitor_nowcast <- function(ws_monitor, version='pm') {
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
  n <- ncol(ws_monitor$data)
  ws_monitor$data[,2:n] <- apply(ws_monitor$data[,2:n], 2, function(x)(nowcast(x, numHrs, weightFactorMin)))
  ws_monitor$data[,2:n] <- round(ws_monitor$data[,2:n], digits = digits)
  return(ws_monitor)
}

