#' @keywords ws_monitor
#' @export
#' @title Apply Nowcast Algorithm to ws_monitor Object
#' @param ws_monitor emph{ws_monitor} object
#' @param version character identity specifying the type of nowcast algorithm to be used
#' @param includeShortTerm calcluate preliminary NowCast values for hours 3-11
#' @return A emph{ws_monitor} object with data that have been processed by the Nowcast algorithm.
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
#' .001 for 'ozone' regardless of the precision of the data in the incoming \emph{ws_monitor} object.
#' @references \url{https://en.wikipedia.org/wiki/Nowcast_(Air_Quality_Index)}
#' @references \url{https://www3.epa.gov/airnow/ani/pm25_aqi_reporting_nowcast_overview.pdf}
#' @references \url{https://aqicn.org/faq/2015-03-15/air-quality-nowcast-a-beginners-guide/}
#' @references \url{https://forum.airnowtech.org/t/the-nowcast-for-ozone-and-pm/172}
#' @references \url{https://forum.airnowtech.org/t/the-aqi-equation/169}
#' @examples
#' N_M <- monitor_subset(Northwest_Megafires, tlim=c(20150815,20150831))
#' Omak <- monitor_subset(N_M, monitorIDs='530470013')
#' Omak_nowcast <- monitor_nowcast(Omak, includeShortTerm=TRUE)
#' monitorPlot_timeseries(Omak, type='l', lwd=2)
#' monitorPlot_timeseries(Omak_nowcast, add=TRUE, type='l', col='purple', lwd=2)
#' addAQILines()
#' addAQILegend(lwd=1, pch=NULL)
#' legend("topleft", lwd=2, col=c('black','purple'), legend=c('hourly','nowcast'))
#' title("Omak, Washington Hourly and Nowcast PM2.5 Values in August, 2015")
#' # Zooming in to check on handling of missing values
#' monitorPlot_timeseries(Omak, tlim=c(20150823,20150825))
#' monitorPlot_timeseries(Omak_nowcast, tlim=c(20150823,20150825), pch=16,col='red',type='b', add=TRUE)
#' abline(v=Omak$data[is.na(Omak$data[,2]),1])
#' title("Missing values")


# NOTE:  This script is based on the javascript code at: 
# NOTE:    https://github.com/chatch/nowcast-aqi/blob/master/nowcast-aqi.js
# NOTE: To compute a valid NowCast, you must have at least two of the most recent 3 hours

# TODO:  python-aqi at: https://pypi.python.org/pypi/python-aqi

monitor_nowcast <- function(ws_monitor, version='pm', includeShortTerm=FALSE) {
  
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
  # NOTE:  We truncate, rather than round, as per documentation linked above
  n <- ncol(ws_monitor$data)
  newData <- apply(as.data.frame(ws_monitor$data[,2:n]), 2, function(x) { .nowcast(x, numHrs, weightFactorMin, includeShortTerm) })
  ws_monitor$data[,2:n] <- as.data.frame(trunc(newData*10^digits)/10^digits)
  
  return( structure(ws_monitor, class = c("ws_monitor", "list")) )
  
}

# ----- Helper Functions ------------------------------------------------------

# TODO:  Need special logic for the first 1:numHrs for which the nowcast algorithm can be applied.
# TODO:  as long as we accept na.rm logic while calculating the most recent 12-hr average.
# TODO:  So the first two data points will be NA but the third could be calculated using that
# TODO:  3-hr average as a proxy for the 12-hr average.
.nowcast <- function(x, numHrs, weightFactorMin, includeShortTerm) {
  
  if ( includeShortTerm ) {
    firstHr <- 3
  } else {
    firstHr <- numHrs
  }
  
  # Start at the end end of the data (most recent hour) and work backwards
  # The oldest hour for which we can calculate nowcast is numHrs
  for ( i in length(x):firstHr ) {

    # Apply nowcast algorithm to numHrs data points in order with more recent first
    concByHour <- x[i:max(1, i-numHrs+1)]

    if ( sum( is.na(concByHour[1:3]) ) >= 2 ) {
    
      # If two or more of the most recent 3 hours are missing, no valid Nowcast will be reported
      
      x[i] <- NA
      
    } else {
      
      # Calculate the weight factor according to the type of air quality data
      weightFactor <- .weightFactor(concByHour, weightFactorMin)
      
      # NOTE:  We need to create vectors so that we can sum at the end with na.rm=TRUE

      weightedConcs <- rep(as.numeric(NA),numHrs)
      weightFactors <- rep(as.numeric(NA),numHrs)

      # Loop over hours to get individual elements
      for (j in 1:numHrs) {
        if ( !is.na( concByHour[j]) ) {
          weightedConcs[j] <- concByHour[j] * weightFactor^(j-1)
          weightFactors[j] <- weightFactor^(j-1)
        }
      }
      
      x[i] <- sum(weightedConcs, na.rm=TRUE) / sum(weightFactors, na.rm=TRUE)

    }
  }
  
  # Set missing values when there are not enough preceding hours
  x[1:(firstHr-1)] <- NA
  
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
