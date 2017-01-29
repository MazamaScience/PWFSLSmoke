#' @keywords nowcast
#' @export
#' @title Apply Nowcast Algorithm to ws_monitor Object
#' @param ws_monitor ws_monitor object
#' @param version character identity specifying the type of nowcast algorithm to be used
#' @return A ws_monitor object with data that have been processed by the Nowcast algorithm.
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
#' @note Calculated Nowcast values are rounded to the nearest .1 for 'pm' and nearest
#' .001 for 'ozone' regardless of the precision of the data in the incoming \code{ws_monitor} object.
#' @references \url{https://en.wikipedia.org/wiki/Nowcast_(Air_Quality_Index)}
#' @references \url{https://www3.epa.gov/airnow/ani/pm25_aqi_reporting_nowcast_overview.pdf}
#' @references \url{http://aqicn.org/faq/2015-03-15/air-quality-nowcast-a-beginners-guide/}
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20160801, 20160815)
#' salinas <- monitor_subset(airnow, monitorIDs='060531003')
#' salinas_nowcast <- monitor_nowcast(salinas)
#' monitorPlot_timeseries(salinas, type='l')
#' monitorPlot_timeseries(salinas_nowcast, add=TRUE, type='l', col='red')
#' }

# NOTE:  This script is based on the javascript code at: 
# NOTE:    https://github.com/chatch/nowcast-aqi/blob/master/nowcast-aqi.js
# NOTE: To compute a valid NowCast, you must have at least two of the most recent 3 hours

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
  # NOTE:  We need as.data.frame for when there is only a single column of data
  n <- ncol(ws_monitor$data)
  ws_monitor$data[,2:n] <- apply(as.data.frame(ws_monitor$data[,2:n]), 2, function(x) { .nowcast(x, numHrs, weightFactorMin) })
  ws_monitor$data[,2:n] <- round(as.data.frame(ws_monitor$data[,2:n]), digits = digits)
  
  return(ws_monitor)
}

# ----- Helper Functions ------------------------------------------------------

.nowcast <- function(x, numHrs, weightFactorMin) {
  
  # Start at the end end of the data (most recent hour) and work backwards
  # The oldest hour for which we can calculate nowcast is numHrs+1
  for ( i in length(x):(numHrs+1) ) {
    
    # Apply nowcast algorithm to numHrs data points in recent-older order
    concByHour <- x[(i-1):(i-numHrs)]
    
    # If two or more of the most recent 3 hours are missing, no valid Nowcast will be reported
    if ( sum( is.na(concByHour[1:3]) ) >= 2 ) {
      x[i] <- NA
      
    } else {
      # Calculate the weight factor according to the type of air quality data
      weightFactor <- .weightFactor(concByHour, weightFactorMin)
      
      weightedConcSum <- weightFactorSum <- 0
      
      # Loop to calculate the denominator and numerator
      for (j in 1:numHrs) {
        weightedConcSum <- sum( weightedConcSum, concByHour[j] * weightFactor^(j-1), na.rm=TRUE)
        weightFactorSum <- sum( weightFactorSum + weightFactor^(j-1), na.rm=TRUE)
      }
      
      x[i] <- weightedConcSum/weightFactorSum
      
    }
  }
  
  # Set missing values when there are not enough preceding hours'
  x[1:numHrs] <- NA
  
  return(x)
}

# Calculate the weight factor ('w' in the nowcast formula)
#  concByHour: vector of hourly concentration values
#  weightFactorMin (optional): wight factor minimum
.weightFactor <- function(concByHour, weightFactorMin) {
  min <- min(concByHour, na.rm=TRUE)
  max <- max(concByHour, na.rm=TRUE)
  
  # NOTE:  The official Nowcast algorithm has no mention of (aphysical) 
  # NOTE:  negative values. But they are not uncommon in actual data.
  # NOTE:  We hande them here.
  
  # Handle zero and negative values
  if ( min < 0 ) { # negative values -- unknowable instrument bias: less weighting
    weightFactor <- weightFactorMin
  } else if ( min == 0 && max == 0 ) { # all zeroes -- consistent: full weighting
    weightFactor = 1
  } else { # all positive: normal calculation
    weightFactor <- min/max
  }
    
  # Nowcast for ozone doesn't use a minimum weight factor
  if ( is.na(weightFactorMin) ) {
    return(weightFactor)
  
  # For pm data, if the min/max ratio is less than min weight factor we use the latter  
  } else if ( weightFactor > weightFactorMin ) {
    return(weightFactor)
    
  } else {
    return(weightFactorMin)
    
  }
}
