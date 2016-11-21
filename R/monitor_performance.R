#' @keywords monitor
#' @export
#' @title Calculate Monitor Prediction Performance
#' @param predicted data list of class \code{ws_monitor}
#' @param observed data list of class \code{ws_monitor}
#' @param t1 value used to classify \code{predicted} measurements
#' @param t2 threshold used to classify \code{observed} measurements
#' @param metric confusion matrix metric to be used
#' @param FPCost cost associated with false positives (type I error)
#' @param FNCost cost associated with false negatives (type II error)
#' @description This function uses "confusion matrix" analysis to calculate
#' different measures of predictive performance for every timeseries found
#' in \code{predicted} with respect to the observed values found in the single timeseries
#' found in \code{observed}.
#' 
#' The requested metric is returned in a dataframe organized with one row per monitor,
#  the same as the \code{ws_monitor$meta} dataframe. If \code{metric=NULL},
#' all available metrics are returned.
#' @return Dataframe of monitors vs named measure of performance.
#' @seealso \link{monitor_performanceMap}
#' @seealso \link{skill_confusionMatrix}

monitor_performance <- function(predicted, observed, t1, t2, metric=NULL, FPCost=1, FNCost=1) {
  
  # Run skill_confusionMatrix on fake data to get a list of all metrics
  metricNames <- names(skill_confusionMatrix(sample(c(TRUE,FALSE),10,replace=TRUE), sample(c(TRUE,FALSE),10,replace=TRUE)))
  # Remove 'table'
  metricNames <- setdiff(metricNames,'table')
  
  # Sanity check:  metric must exist
  if ( !is.null(metric) && !metric %in% metricNames ) {
    stop(paste0('Metric "',metric,'" is not a recognized confusionMatrix metric.'))
  }
  
  # Sanity check:  observed must be a single timeseries
  if ( ncol(observed$data) > 2 ) {
    observedMonitors <- paste0(names(observed$data[-1]), sep=',')
    stop(paste0('"observed" must have a single monitor, ',length(observedMonitors),' found: ',observedMonitors))
  }
  
  # Extract names and data, omitting the first 'datetime' column
  monitorIDs <- names(predicted$data)[-1]
  predictedData <- as.data.frame(predicted$data[,-1])
  observedData = as.numeric(observed$data[,-1])

  # Create empty 'performance dataframe
  performance <- data.frame(monitorID=monitorIDs)
  rownames(performance) <- monitorIDs
  
  # Include columns for the metrics
  if ( !is.null(metric) ) {
    performance[,metric] <- as.numeric(NA)
  } else {
    for (metricName in metricNames) {
      performance[,metricName] <- as.numeric(NA)
    }
  }
  
  # Calculate confusionMatrix metrics comparing each monitor's data with the observed data
  for (monitorID in monitorIDs) {
    
    if ( is.null(metric) ) {
      # all metrics
      cm <- skill_confusionMatrix(predictedData[,monitorID] >= t1, observedData >= t2, FPCost, FNCost)
      for (metricName in metricNames) {
        performance[monitorID,metricName] <- cm[[metricName]]
      }
    } else {
      # a single metric
      cm <- skill_confusionMatrix(predictedData[,monitorID] >= t1, observedData >= t2, FPCost, FNCost)
      performance[monitorID,metric] <- cm[[metric]]
    }
    
  }
  
  return(performance)
  
}
