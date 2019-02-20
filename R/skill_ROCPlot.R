#' @export
#' @title ROC Plot
#' @param predicted vector of predicted values (or a \emph{ws_monitor} object with a single location)
#' @param observed vector of observed values (or a \emph{ws_monitor} object with a single location)
#' @param t1Range lo and high values used to generate test thresholds for classifying \code{predicted} data
#' @param t2s vector of thresholds used to classify \code{observed} data
#' @param n number of test thresholds in ROC curve
#' @param colors vector of colors used when plotting curves
#' @description This function plots ROC curves for a variety of \code{observed} classification thresholds.
#' @references \href{https://en.wikipedia.org/wiki/Receiver_operating_characteristic}{Receiver Operating Characteristic}
#' @seealso \link{skill_confusionMatrix}
#' @seealso \link{skill_ROC}
#' @examples
#' \dontrun{
#' # Napa Fires -- October, 2017
#' ca <- airnow_loadAnnual(2017) %>%
#'   monitor_subset(tlim=c(20171001,20171101), stateCodes='CA')
#' Vallejo <- monitor_subset(ca, monitorIDs='060950004_01')
#' Napa <- monitor_subset(ca, monitorIDs='060550003_01')
#' skill_ROCPlot(Vallejo, Napa)
#' }

skill_ROCPlot <- function(predicted, observed,
                          t1Range=c(0,100),
                          t2s=seq(10,100,10), n=101,
                          colors=grDevices::rainbow(length(t2s))) {

  # Extract data from ws_monitor objects
  if ( 'ws_monitor' %in% class(predicted) ) {
    if ( ncol(predicted$data) > 2 ) {
      stop(paste0('predicted must represent a single monitoring location'))
    } else {
      predicted <- predicted$data[,2]
    }
  }

  # Extract data from ws_monitor objects
  if ( 'ws_monitor' %in% class(observed) ) {
    if ( ncol(observed$data) > 2 ) {
      stop(paste0('observed must represent a single monitoring location'))
    } else {
      observed <- observed$data[,2]
    }
  }

  # Sanity checks
  if ( length(predicted) != length(observed) ) {
    stop(paste0("predicted and observed vectors are of different lengths"))
  }

  # Set up empty legendText
  legendText <- vector('character', length=length(t2s))

  # Loop through all thresholds
  for (i in 1:length(t2s)) {
    rocList <- skill_ROC(predicted, observed, t1Range, t2s[i], n)
    cm <- skill_confusionMatrix(predicted >= t2s[i], observed >= t2s[i])
    legendText[i] <- paste0('threshold ', t2s[i], ',  AUC = ',sprintf("%.3f",rocList$auc),',  Heidke Skill = ', sprintf("%.3f",cm$heidkeSkill))
    if (i == 1) {
      plot(rocList$roc$TPR ~ rocList$roc$FPR, type='S', lwd=2, col=colors[i],
           xlab='False Positive Rate', ylab='True Positive Rate')
    } else {
      points(rocList$roc$TPR ~ rocList$roc$FPR, type='S', lwd=2, col=colors[i])
    }
  }

  legend('bottomright', legend=legendText, col=colors, lwd=2,
         title='ROC Curves')

}
