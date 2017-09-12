#' @keywords ws_monitor
#' @export
#' @title Apply Nowcast Algorithm to ws_monitor Object
#' @param ws_monitor \emph{ws_monitor} object
#' @param version character identity specifying the type of nowcast algorithm to be used
#' @param includeShortTerm calcluate preliminary NowCast values starting with the 2nd hour
#' @return A \emph{ws_monitor} object with data that have been processed by the Nowcast algorithm.
#' @description A Nowcast algorithm is applied to the data in in the ws_monitor object. The 
#' \code{version} argument specifies the minimum weight factor and number of hours to be 
#' considered in the calculation.
#' 
#' Available versions include:
#' \enumerate{
#' \item{\code{pm}}: {hours=12, weight=0.5}
#' \item{\code{pmAsian}}: {hours=3, weight=0.1}
#' \item{\code{ozone}}: {hours=8, weight=NA}
#' }
#' 
#' The default, \code{version='pm'}, is appropriate for typical usage.
#' 
#' @details 
#' This function calculates the current hour's NowCast value based on the value for the given hour and the previous N-1 hours, where N is the number
#' of hours corresponding to the \code{version} argument (see \strong{Description} above). For example, if \code{version=pm}, then the NowCast value
#' for Hour 12 is based on the data from Hours 1-12.
#' 
#' The function requires valid data for at least two of the three latest hours; NA's are returned for hours where this condition is not met.
#' 
#' By default, the funtion will not return a valid value until the Nth hour. If \code{includeShortTerm=TRUE}, the function will return a valid value
#' after only the 2nd hour (provided, of course, that both hours are valid).
#' 
#' Calculated Nowcast values are truncated to the nearest .1 ug/m3 for 'pm' and nearest
#' .001 ppm for 'ozone' regardless of the precision of the data in the incoming \emph{ws_monitor} object.
#' 
#' @references \url{https://en.wikipedia.org/wiki/Nowcast_(Air_Quality_Index)}
#' @references \url{https://www3.epa.gov/airnow/ani/pm25_aqi_reporting_nowcast_overview.pdf}
#' @references \url{https://aqicn.org/faq/2015-03-15/air-quality-nowcast-a-beginners-guide/}
#' @references \url{https://forum.airnowtech.org/t/the-nowcast-for-ozone-and-pm/172}
#' @references \url{https://forum.airnowtech.org/t/the-aqi-equation/169}
#' @references \url{https://airnow.zendesk.com/hc/en-us/articles/211625598-How-does-AirNow-make-the-Current-PM-Air-Quality-Index-AQI-maps-}
#' @references \url{https://forum.airnowtech.org/t/how-does-airnow-handle-negative-hourly-concentrations/143}
#' 
#' @examples
#' \dontrun{
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
#' }

# NOTE:  This script is based on the javascript code at: 
# NOTE:    https://github.com/chatch/nowcast-aqi/blob/master/nowcast-aqi.js
# NOTE:  To compute a valid NowCast, you must have at least two of the most recent 3 hours

# TODO:  python-aqi at: https://pypi.python.org/pypi/python-aqi

#### ----- NowCast Calculation Overview -----
# 
# The process for calculating the NowCast concentration and AQI for PM2.5 or PM10 is as follows:
# 
# 1. Compute the concentrations range (max-min) over the last 12 hours.
# 2. Divide the range by the maximum concentration in the 12 hour period to obtain the scaled rate of change.
# 3. Compute the weight factor by subtracting the scaled rate from 1. The weight factor must be between .5 and 1.
#    The minimum limit approximates a 3-hour average. If the weight factor is less than .5, then set it equal to .5.
# 4. Multiply each hourly concentration by the weight factor raised to the power of how many hours ago the concentration
#    was measured (for the current hour, the factor is raised to the zero power).
# 5. Compute the NowCast by summing these products and dividing by the sum of the weight factors raised to the power of
#    how many hours ago the concentration was measured.

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
    weightFactorMin <- NA  # negative values adjusted up to 0 in .weightFactor()
    digits <- 3  # NOTE: digits=3 assumes Ozone values given in ppm; update to 0 if values given in ppb
  }
  
  # Apply nowcast to each data column in ws_monitor
  # NOTE:  We need as.data.frame for when there is only a single column of data.
  # NOTE:  We truncate, rather than round, per the following:
  # NOTE:  https://forum.airnowtech.org/t/the-nowcast-for-ozone-and-pm/172
  n <- ncol(ws_monitor$data)
  newData <- apply(as.data.frame(ws_monitor$data[2:n]), 2, function(x) { .nowcast(x, numHrs, weightFactorMin, includeShortTerm) })
  ws_monitor$data[2:n] <- as.data.frame(trunc(newData*10^digits)/10^digits)
  
  return( structure(ws_monitor, class = c("ws_monitor", "list")) )
  
}

# ----- Helper Functions ------------------------------------------------------

.nowcast <- function(x, numHrs, weightFactorMin, includeShortTerm) {
  
  if ( includeShortTerm ) {
    firstHr <- 2
  } else {
    firstHr <- numHrs
  }
  
  # Start at the end end of the data (most recent hour) and work backwards
  # The oldest hour for which we can calculate nowcast is numHrs, unless includeShortTerm=TRUE
  # in which case we can go back to the 2nd hour.
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
# Assumes concByHour has at least one valid value to calculate min & max. In fact, .nowcast won't even call 
# this function if more than one of the three most recent hours is invalid.
.weightFactor <- function(concByHour, weightFactorMin) {
  
  min <- min(concByHour, na.rm=TRUE)
  max <- max(concByHour, na.rm=TRUE)
  
  # Calculate weight factor
  # NOTE:  https://forum.airnowtech.org/t/the-nowcast-for-ozone-and-pm/172 says that there is "no minimum
  # NOTE:    weight factor" for ozone; however, we limit the value to zero since otherwise it would be possible
  # NOTE:    to get negative weights, even as large as -Inf (i.e. if min<0 & max=0).
  # NOTE:  Otherwise, we don't worry about negatives, per the following:
  # NOTE:    https://forum.airnowtech.org/t/how-does-airnow-handle-negative-hourly-concentrations/143
  weightFactor <- 1-(max-min)/max
  weightFactor <- min(weightFactor, 1, na.rm=TRUE)
  weightFactor <- max(weightFactor, weightFactorMin, 0, na.rm=TRUE)
  
  return(weightFactor)
  
}
