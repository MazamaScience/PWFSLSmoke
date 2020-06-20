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

  # ----- Validate parameters --------------------------------------------------

  # Sanity check -- row count
  if ( nrow(tbl) == 0 ) {
    logger.warn("No valid PM2.5 data") # This is more of a warning than some error in the data.
    stop("No valid PM2.5 data")
  }

  # Sanity check -- tbl must have a monitorType
  if ( !'monitorType' %in% names(tbl) ) {
    logger.error("No 'monitorType' column found in 'tbl' tibble with columns: %s", paste0(names(tbl), collapse = ", "))
    stop(paste0("No 'monitorType' column found in 'tbl' tibble."))
  }

  monitorType <- unique(tbl$monitorType)

  # Sanity check -- tbl must have only one monitorType
  if ( length(monitorType) > 1 ) {
    logger.error("Multilpe monitor types found in 'tbl' tibble: %s", paste0(monitorType, collapse = ", "))
    stop(paste0("Multiple monitor types found in 'tbl' tibble."))
  }

  monitorType <- monitorType[1]

  monitorSubtype <- unique(tbl$monitorSubtype)

  # Sanity check -- tbl must have only one monitorSubtype
  if ( length(monitorSubtype) > 1 ) {
    logger.error("Multilpe monitor sybtypes found in 'tbl' tibble: %s", paste0(monitorSubtype, collapse = ", "))
    stop(paste0("Multiple monitor subtypes found in 'tbl' tibble."))
  }

  monitorSubtype <- monitorSubtype[1]

  # ----- Apply type-specific QC -----------------------------------------------

  logger.trace("Applying %s %s QC rules", monitorType, monitorSubtype)

  if ( monitorType == 'BAM1020' ) {

    tbl <- airsis_BAM1020QualityControl(tbl, ...)

  } else if ( monitorType == 'EBAM' ) {

    if ( monitorSubtype == "MULTI2" ) {
      tbl <- airsis_EBAM_MULTI2QualityControl(tbl, ...)
    } else if ( monitorSubtype == "PLUS_MULTI" ) {
      tbl <- airsis_EBAM_PLUS_MULTIQualityControl(tbl, ...)
    } else {
      tbl <- airsis_EBAMQualityControl(tbl, ...)
    }

  } else if ( monitorType == 'ESAM' ) {

    if ( monitorSubtype == "MULTI" ) {
      tbl <- airsis_ESAM_MULTIQualityControl(tbl, ...)
    } else {
      tbl <- airsis_ESAMQualityControl(tbl, ...)
    }

  } else {

    logger.warn("Dataframe contains %s data -- no QC available, original tibble being returned", monitorType)

  }

  return(tbl)

}
