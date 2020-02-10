#' @export
#' @title ROC Curve
#' @param predicted vector of predicted values (or a \emph{ws_monitor} object with a single location)
#' @param observed vector of observed values (or a \emph{ws_monitor} object with a single location)
#' @param t1Range lo and high values used to generate test thresholds for classifying \code{predicted} data
#' @param t2 used to classify \code{observed} data
#' @param n number of test thresholds in ROC curve
#' @description This function calculates an ROC dataframe of TPR, FPR, and Cost for a range of
#' thresholds as well as the area under the ROC curve.
#' @return List containing an \code{roc} matrix and the \code{auc} area under the ROC curve.
#' @references \href{https://en.wikipedia.org/wiki/Receiver_operating_characteristic}{Receiver Operating Characteristic}
#' @seealso \link{skill_confusionMatrix}
#' @seealso \link{skill_ROCPlot}
#' @examples
#' \dontrun{
#' # Napa Fires -- October, 2017
#' ca <- airnow_loadAnnual(2017) %>%
#'   monitor_subset(tlim = c(20171001,20171101), stateCodes = 'CA')
#' Vallejo <- monitor_subset(ca, monitorIDs = '060950004_01')
#' Napa <- monitor_subset(ca, monitorIDs = '060550003_01')
#' t2 <- AQI$breaks_24[4] # 'Unhealthy'
#' rocList <- skill_ROC(Vallejo, Napa, t1Range = c(0,100), t2 = t2)
#' roc <- rocList$roc
#' auc <- rocList$auc
#' plot(roc$TPR ~ roc$FPR, type = 'S')
#' title(paste0('Area Under Curve = ', format(auc,digits = 3)))
#' }

skill_ROC <- function(
  predicted,
  observed,
  t1Range = NULL,
  t2 = NULL,
  n = 101
) {

  # ----- Validate parameters --------------------------------------------------

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

  if ( is.null(t2) ) {
    stop(paste0("t2 must be specified"))
  }

  if ( class(predicted) != "numeric" ) {
    stop(paste0("predicted must be a number vector"))
  }

  if ( class(observed) != "numeric" ) {
    stop(paste0("observed must be a number vector"))
  }

  # ----- Prepare variables ----------------------------------------------------

  # Remove any elements where either predicted or observed has NA
  m <- matrix(c(predicted, observed), ncol = 2)
  badRows <- apply(m, 1, function(x) { any(is.na(x)) })
  predicted <- m[!badRows,1]
  observed <- m[!badRows, 2]

  # If no range is given, derive the range from the data
  if ( is.null(t1Range) ) {
    t1_lo <- max(min(predicted), min(observed))
    t1_hi <- min(max(predicted), max(observed))
    t1Range <- c(t1_lo, t1_hi)
  }

  # Create test thresholds
  testThresholds <- seq(t1Range[1], t1Range[2], length.out = n)

  # Calculate ROC and cost
  roc <- data.frame(threshold = testThresholds, FPR = NA, TPR = NA, cost = NA)
  for ( i in seq_along(testThresholds) ) {
    cm <- skill_confusionMatrix(predicted >= testThresholds[i], observed >= t2)
    roc$FPR[i] <- cm$FPRate
    roc$TPR[i] <- cm$TPRate
    roc$cost[i] <- cm$cost
  }

  # Calculate Area Under Curve ----------------------------
  # TODO:  Is there an AUC function in a standard package we should use, perhaps DescTools::AUC?
  roc <- roc[with(roc, order(FPR, TPR)),]
  tpr_diff <- diff(roc$TPR)
  fpr_diff <- diff(roc$FPR)
  triangles <- 0.5 * tpr_diff * fpr_diff
  long_parts <- fpr_diff * roc$TPR[-length(roc$TPR)]
  auc <- sum(long_parts) + sum(triangles)

  # Reorder ROC by threshold
  roc <- roc[with(roc, order(threshold)),]
  return(list(roc = roc, auc = auc))

  if ( FALSE ) {
    ca <- airnow_loadAnnual(2017) %>%
      monitor_subset(tlim = c(20171001,20171101), stateCodes = 'CA')
    Vallejo <- predicted <- monitor_subset(ca, monitorIDs = '060950004_01')
    Napa <- observed <- monitor_subset(ca, monitorIDs = '060550003_01')
    t2 <- AQI$breaks_24[4] # 'Unhealthy'
    t1Range <-  c(55.5,55.5)
    skill_ROC(Vallejo, Napa,t2 = t2)
    skill_ROCPlot(Vallejo, Napa, t2 = t2)
  }

}

