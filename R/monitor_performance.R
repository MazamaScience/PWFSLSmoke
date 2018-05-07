#' @keywords ws_monitor
#' @export
#' @title Calculate Monitor Prediction Performance
#' @param predicted ws_monitor object with predicted data
#' @param observed ws_monitor object with observed data
#' @param t1 value used to classify \code{predicted} measurements
#' @param t2 threshold used to classify \code{observed} measurements
#' @param metric \emph{confusion matrix} metric to be used
#' @param FPCost cost associated with false positives (type II error)
#' @param FNCost cost associated with false negatives (type I error)
#' @return Dataframe of monitors vs named measure of performance.
#' @description This function uses \emph{confusion matrix} analysis to calculate
#' different measures of predictive performance for every timeseries found
#' in \code{predicted} with respect to the observed values found in the single timeseries
#' found in \code{observed}.
#' 
#' The requested metric is returned in a dataframe organized with one row per monitor,
#  the same as the \code{ws_monitor$meta} dataframe. If \code{metric=NULL},
#' all available metrics are returned.
#' @seealso \link{monitorMap_performance}
#' @seealso \link{skill_confusionMatrix}
#' @examples 
#' \dontrun{
#' # If daily avg data were the prediciton and Spokane were
#' # the observed, which WA State monitors had skill?
#' wa <- airnow_load(2017) %>% monitor_subset(stateCodes='WA')
#' wa_dailyAvg <- monitor_dailyStatistic(wa, mean)
#' Spokane_dailyAvg <- monitor_subset(wa_dailyAvg, monitorIDs='530630021_01')
#' threshold <- AQI$breaks_24[4] # Unhealthy
#' performanceMetrics <- monitor_performance(wa_dailyAvg, 
#'                                           Spokane_dailyAvg,
#'                                           threshold, threshold)
#' monitorIDs <- rownames(performanceMetrics)
#' mask <- performanceMetrics$heidikeSkill &
#'         !is.na(performanceMetrics$heidikeSkill)
#' skillfulIDs <- monitorIDs[mask]
#' skillful <- monitor_subset(wa_dailyAvg, monitorIDs=skillfulIDs)
#' monitorLeaflet(skillful)
#' }

monitor_performance <- function(predicted, observed, t1, t2, metric=NULL, FPCost=1, FNCost=1) {
  
  # Run skill_confusionMatrix on fake data to get a list of all metrics
  metricNames <- names(skill_confusionMatrix(sample(c(TRUE,FALSE),10,replace=TRUE), sample(c(TRUE,FALSE),10,replace=TRUE)))
  # Remove 'table'
  metricNames <- setdiff(metricNames,'table')
  
  # Sanity check:  metric must exist
  if ( !is.null(metric) && !metric %in% metricNames ) {
    stop(paste0('Metric "',metric,'" is not a recognized confusionMatrix metric.'))
  }
  
  # Extract data in different cases
  
  if ( ncol(observed$data) > 2 ) {
  # one to one relation: monitorIDs must be matched exactly in observed and predicted
  ## NOTE: the case when there's only a single matching monitorID is not considered
    
    monitorIDsPred <- names(predicted$data)[-1]
    monitorIDsObs <- names(observed$data)[-1]
    monitorIDs <- intersect(monitorIDsPred, monitorIDsObs)
    diff <- setdiff(monitorIDsPred, monitorIDsObs)
    
    if ( length(diff) > 0 && length(monitorIDs) != 0 ) {
      
      sprintf("%i monitor(s) will be discarded", length(diff))
      predicted <-monitor_subset(predicted, monitorIDs=monitorIDs)
      observed <- monitor_subset(observed, monitorIDs=monitorIDs)
      
    } else {
      stop(paste0("There is no matching monitorIDs at all."))
    }
    
    predictedData <- as.data.frame(predicted$data[,-1])
    observedData <- as.data.frame(observed$data[,-1])
    
  } else {
  # one to many relation: using others monitors with different IDs to predict a single monitor
    
    monitorIDs <- names(predicted$data)[-1]
    predictedData <- as.data.frame(predicted$data[,-1])
    names(predictedData) <- monitorIDs
    observedData = as.numeric(observed$data[,-1])
    
  } 
  
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
    
    if ( ncol(observed$data) > 2) {
      cm <- skill_confusionMatrix(predictedData[,monitorID] >= t1, observedData[,monitorID] >= t2, FPCost, FNCost)
    } else {
      cm <- skill_confusionMatrix(predictedData[,monitorID] >= t1, observedData >= t2, FPCost, FNCost)
    }
    
    if ( is.null(metric) ) {
      # all metrics
      for (metricName in metricNames) {
        performance[monitorID,metricName] <- cm[[metricName]]
      }
    } else {
      # a single metric
      performance[monitorID,metric] <- cm[[metric]]
    }
    
  }
  
  return(performance)
  
}
