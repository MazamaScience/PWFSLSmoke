#' @export
#' @title Confusion Matrix Statistics
#' @param predicted logical vector of predicted values
#' @param observed logical vector of observed values
#' @param FPCost cost associated with false positives (type I error)
#' @param FNCost cost associated with false negatives (type II error)
#' @param lightweight flag specifying creation of a return list without derived metrics
#' @return List containing a table of confusion matrix values and a suite of derived metrics.
#' @description Measurements of categorical forecast accuracy have a long history
#' in weather forecasting. The standard approach involves making binary classifications
#' (detected/not-detected) of predicted and observed data and combining them in a
#' binary contingency table known as a *confusion matrix*.
#' 
#' This function creates a confusion matrix from predicted and observed values and calculates
#' a wide range of common statistics including: 
#' \itemize{
#'   \item{TP}{ (true postive)}
#'   \item{FP}{ (false postive) (type I error)}
#'   \item{FN}{ (false negative) (type II error)}
#'   \item{TN}{ (true negative) }
#'   \item{TPRate}{ (true positive rate) = sensitivity = recall = TP / (TP + FN)}
#'   \item{FPRate}{ (false positive rate) = FP / (FP + TN)}
#'   \item{FNRate}{ (false negative rate) = FN / (TP + FN)}
#'   \item{TNRate}{ (true negative rate) = specificity = TN / (FP + TN)}
#'   \item{accuracy}{ = proportionCorrect = (TP + TN) / total}
#'   \item{errorRate}{ = 1 - accuracy = (FP + FN) / total}
#'   \item{falseAlarmRatio}{ = PPV (positive predictive value) = precision = TP / (TP + FP)}
#'   \item{FDR}{ (false discovery rate) = FP / (TP + FP) }
#'   \item{NPV}{ (negative predictive value) = TN / (TN + FN)}
#'   \item{FOR}{ (false omission rate) = FN / (TN + FN)}
#'   \item{f1_score}{ = (2 * TP) / (2 * TP + FP + FN)}
#'   \item{detectionRate}{ = TP / total}
#'   \item{baseRate}{ = detectionPrevalence = (TP + FN) / total}
#'   \item{probForecastOccurance}{ = prevalence = (TP + FP) / total}
#'   \item{balancedAccuracy}{ = (TPRate + TNRate) / 2}
#'   \item{expectedAccuracy}{ = (((TP + FP) * (TP + FN) / total) + ((FP + TN) * sum(FN + TN) / total )) / total}
#'   \item{heidikeSkill}{ = kappa = (accuracy - expectedAccuracy) / (1 - expectedAccuracy)}
#'   \item{bias}{ = (TP + FP) / (TP + FN)}
#'   \item{hitRate}{ = TP / (TP + FN)}
#'   \item{falseAlarmRate}{ = FP / (FP + TN)}
#'   \item{pierceSkill}{ = ((TP * TN) - (FP * FN)) / ((FP + TN) * (TP + FN))}
#'   \item{criticalSuccess}{ = TP / (TP + FP + FN)}
#'   \item{oddsRatioSkill}{ = yulesQ = ((TP * TN) - (FP * FN)) / ((TP * TN) + (FP * FN))}
#' }
#' 
#' @references \href{http://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/}{Simple Guide to Confusion Matrix Terminology}
#' @seealso \link{skill_ROC}
#' @seealso \link{skill_ROCPlot}
#' @examples 
#' predicted <- sample(c(TRUE,FALSE), 1000, replace=TRUE, prob=c(0.3,0.7))
#' observed <- sample(c(TRUE,FALSE), 1000, replace=TRUE, prob=c(0.3,0.7))
#' cm <- skill_confusionMatrix(predicted, observed)
#' print(cm)
#' 
skill_confusionMatrix <- function(predicted, observed,
                                  FPCost=1, FNCost=1,
                                  lightweight=FALSE) {
  
  # Sanity check
  if ( length(predicted) != length(observed) ) {
    stop(paste0("The predicted and observed vectors are of different lengths."))
  }
  
  # Sanity check
  if ( !is.logical(predicted) | !is.logical(observed) ) {
    stop(paste0("predicted and observed must be logical vectors."))
  }
  
  # Remove any elements where either predicted or observed has NA
  m <- matrix(c(predicted, observed), ncol=2)
  badRows <- apply(m, 1, function(x) { any(is.na(x)) })
  predicted <- m[!badRows, 1]
  observed <- m[!badRows, 2]

  # data size
  total = length(observed)
  
  # ----- Positivity Metrics -----
  # from http://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/
  # OR from https://en.wikipedia.org/wiki/Confusion_matrix#Table_of_confusion
  
  # Here is the structure of a "binary contingency table":
  #
  #             Predicted F Predicted T
  #    Actual F    TN          FP
  #    Actual T    FN          TP
  
  # true positive
  TP <- sum(predicted & observed)
  TPRate <- TP / sum(observed)
  # false positive
  FP <- sum(predicted & !observed)
  FPRate <- FP / sum(!observed)
  # true negative
  TN <- sum(!predicted & !observed)
  TNRate <- TN / sum(!observed)
  # false negative
  FN <- sum(!predicted & observed)
  FNRate <- FN / sum(observed)
  
  # Create the matrix
  dimnames <- list(Predicted=c('FALSE','TRUE'),Actual=c('FALSE','TRUE'))
  table <- as.table(matrix(c(TN,FP,FN,TP), nrow=2, dimnames=dimnames))
  
  # ----- Cost Function -----
  cost <- (FP / total * FPCost) + (FN / total * FNCost)
  
  if ( lightweight ) {

    # Calculation of ROC curves requires only a minimal list of metrics
    returnList <- list(table, TPRate, FPRate, cost)
    names(returnList) <- c('Table', 'TPRate', 'FPRate', 'cost')
    
  } else {
    
    # ----- Rates -----
    # from http://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/
    # OR from https://en.wikipedia.org/wiki/Confusion_matrix#Table_of_confusion
    accuracy <- (TP+TN) / total
    errorRate <- 1 - accuracy            # also = (FP+FN)/total
    sensitivity <- recall <- TPRate
    specificity <- TNRate
    PPV <- precision <- TP / (TP + FP)   # also = TP / sum(predicted)
    FDR <- FP / (TP + FP)                # also = FP / sum(predicted)
    NPV <- TN / (TN + FN)                # also = TN / sum(!predicted)
    FOR <- FN / (TN + FN)                # also = FN / sum(!predicted)
    
    prevalence <- sum(observed) / total
    
    # from https://en.wikipedia.org/wiki/Confusion_matrix#Table_of_confusion
    f1_score <- (2*TP) / (2*TP + FP + FN)
    
    # additional metrics from ?caret::confusionmatrix
    detectionRate <- TP / total
    detectionPrevalence <- (TP + FN) / total
    balancedAccuracy <- (sensitivity + specificity) / 2
    
    # ----- Cohen's Kappa -----
    # from https://en.wikipedia.org/wiki/Cohen%27s_kappa
    expectedAccuracy <- (( sum(predicted) * sum(observed) / total ) + 
      ( sum(!predicted) * sum(!observed) / total )) / total
    kappa <- (accuracy - expectedAccuracy) / (1 - expectedAccuracy)
    
    # descriptive measures from forecast verification book
    baseRate <- detectionPrevalence
    probForecastOccurance <- prevalence
    bias <- (TP + FP) / (TP + FN)
      
    # performance measures from forecast verification book
    hitRate <- TP / (TP + FN)
    falseAlarmRate <- FP / (FP + TN)
    falseAlarmRatio <- PPV
    proportionCorrect <- accuracy
    heidikeSkill <- kappa
    pierceSkill <- ((TP * TN) - (FP * FN)) / ((FP + TN) * (TP + FN))
    criticalSuccess <- TP / (TP + FP + FN)
    oddsRatioSkill <- yulesQ <- ((TP * TN) - (FP * FN)) / ((TP * TN) + (FP * FN))
    
    # Create a list with the named metrics
    returnList <- list(table, TPRate, FPRate, TNRate, FNRate,
                    PPV, FDR, NPV, FOR,
                    accuracy, errorRate, sensitivity, recall, specificity,
                    precision, prevalence, f1_score,
                    detectionRate, detectionPrevalence, balancedAccuracy,
                    expectedAccuracy, kappa, cost,
                    hitRate, falseAlarmRate, falseAlarmRatio, proportionCorrect,
                    oddsRatioSkill, heidikeSkill, pierceSkill, criticalSuccess, yulesQ)
    names(returnList) <- c('table', 'TPRate', 'FPRate', 'TNRate', 'FNRate', 
                        'PPV', 'FDR', 'NPV', 'FOR',
                        'accuracy', 'errorRate', 'sensitivity', 'recall', 'specificity',
                        'precision', 'prevalence', 'f1_score',
                        'detectionRate', 'detectionPrevalence', 'balancedAccuracy',
                        'expectedAccuracy', 'kappa', 'cost',
                        'hitRate', 'falseAlarmRate', 'falseAlarmRatio', 'proportionCorrect',
                        'oddsRatioSkill', 'heidikeSkill', 'pierceSkill', 'criticalSuccess', 'yulesQ')
    
  }
  
  return(returnList)
  
}
