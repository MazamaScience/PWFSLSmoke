#' @keywords WRCC
#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Apply Quality Control to raw WRCC tibble
#' @param tbl single site tibble created by \code{wrcc_downloadData()}
#' @param ... additional parameters are passed to type-specific QC functions
#' @description Various QC steps are taken to clean up the incoming raw tibble including:
#'
#' \enumerate{
#' \item{Convert numeric missing value flags to \code{NA}.}
#' \item{Remove measurement records with values outside of valid ranges.}
#' }
#'
#' See the individual \code{wrcc_~QualityControl()} functions for details.
#' @return Cleaned up tibble of WRCC monitor data.
#' @seealso \code{\link{wrcc_EBAMQualityControl}}
#' @seealso \code{\link{wrcc_ESAMQualityControl}}

wrcc_qualityControl <- function(
  tbl,
  ...
) {

  logger.debug(" ----- wrcc_qualityControl() ----- ")

  # ----- Validate parameters --------------------------------------------------

  # Sanity check -- row count
  if ( nrow(tbl) == 0 ) {
    logger.warn("No valid PM2.5 data") # This is more of a warning than some error in the data.
    stop("No valid PM2.5 data")
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

  logger.trace('Applying %s QC rules', monitorType)

  if ( monitorType == 'BAM1020' ) {

    logger.warn("Tibble contains %s data -- no QC available, original tibble being returned", monitorType)

  } else if ( monitorType == 'EBAM' ) {

    tbl <- wrcc_EBAMQualityControl(tbl, ...)

  } else if ( monitorType == 'ESAM' ) {

    tbl <- wrcc_ESAMQualityControl(tbl, ...)

  } else {

    logger.warn("Tibble contains %s data -- no QC available, original tibble being returned", monitorType)

  }

  return(tbl)

}
