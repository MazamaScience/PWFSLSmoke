#' @keywords AIRSIS
#' @export
#' @import MazamaCoreUtils
#'
#' @title Apply Quality Control to raw AIRSIS dataframe
#'
#' @param tbl single site tibble created by airsis_downloadData()
#' @param ... additional parameters are passed to type-specific QC functions
#' @description Various QC steps are taken to clean up the incoming raw tibble including:
#'
#' \enumerate{
#' \item{Ensure GPS location data are included in each measurement record.}
#' \item{Remove GPS location records.}
#' \item{Remove measurement records with values outside of valid ranges.}
#' }
#'
#' See the individual \code{airsis_~QualityControl()} functions for details.
#'
#' QC parameters that can be passed in the \code{\dots} include the following
#' valid data ranges as taken from \code{airsis_EBAMQualityControl()}:
#'
#' \itemize{
#' \item{\code{valid_Longitude=c(-180,180)}}
#' \item{\code{valid_Latitude=c(-90,90)}}
#' \item{\code{remove_Lon_zero = TRUE}}
#' \item{\code{remove_Lat_zero = TRUE}}
#' \item{\code{valid_Flow = c(16.7*0.95,16.7*1.05)}}
#' \item{\code{valid_AT = c(-Inf,45)}}
#' \item{\code{valid_RHi = c(-Inf,45)}}
#' \item{\code{valid_Conc = c(-Inf,5.000)}}
#' }
#'
#' Note that appropriate values for QC thresholds will depend on the type of monitor.
#'
#' @return Cleaned up tibble of AIRSIS monitor data.
#' @seealso \code{\link{airsis_EBAMQualityControl}}
#' @seealso \code{\link{airsis_ESAMQualityControl}}

airsis_qualityControl <- function(
  tbl,
  ...
) {

  logger.debug(" ----- airsis_qualityControl() ----- ")

  # Sanity check -- row count
  if ( nrow(tbl) == 0 ) {
    logger.error("Unable to perform QC: tibble empty")
    stop(paste0("Unable to perform QC: tibble empty"))
  }

  # Sanity check -- tbl must have a monitorType
  if ( !'monitorType' %in% names(tbl) ) {
    logger.error("No 'monitorType' column found in 'tbl' tibble with columns: %s", paste0(names(tbl), collapse=", "))
    stop(paste0("No 'monitorType' column found in 'tbl' tibble."))
  }

  monitorType <- unique(tbl$monitorType)

  # Sanity check -- tbl must have only one monitorType
  if ( length(monitorType) > 1 ) {
    logger.error("Multilpe monitor types found in 'tbl' tibble: %s", paste0(monitorType, collapse=", "))
    stop(paste0("Multiple monitor types found in 'tbl' tibble."))
  }

  monitorType <- monitorType[1]

  logger.trace("Applying %s QC rules", monitorType)

  if ( monitorType == 'BAM1020' ) {

    tbl <- airsis_BAM1020QualityControl(tbl, ...)

  } else if ( monitorType == 'EBAM' ) {

    tbl <- airsis_EBAMQualityControl(tbl, ...)

  } else if ( monitorType == 'ESAM' ) {

    tbl <- airsis_ESAMQualityControl(tbl, ...)

  } else {

    logger.warn("Dataframe contains %s data -- no QC available, original tibble being returned", monitorType)

  }

  return(tbl)

}
