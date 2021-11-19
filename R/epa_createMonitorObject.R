#' @keywords EPA
#' @export
#' @import MazamaCoreUtils
#'
#' @title Download and convert hourly EPA air quality data
#'
#' @param zipFile absolute path to monitoring data .zip file
#' @param zeroMinimum logical specifying whether to convert negative values to zero
#' @param addGoogleMeta logicial specifying wheter to use Google elevation and reverse geocoding services
#' @description Convert EPA data into a \emph{ws_monitor} object, ready for use with all \code{monitor_~} functions.
#' @note Before running this function you must first enable spatial data capabilities as in the example.
#' @return A \emph{ws_monitor} object with EPA data.
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_format_3}{file format description}
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' initializeMazamaSpatialUtils()
#' zipFile <- epa_downloadData(2016, "88101", downloadDir = '~/Data/EPA')
#' mon <- epa_createMonitorObject(zipFile, addGoogleMeta = FALSE)
#'
#' }, silent = FALSE)
#' }

epa_createMonitorObject <- function(
  zipFile = NULL,
  zeroMinimum = TRUE,
  addGoogleMeta = TRUE
) {

  logger.debug(" ----- epa_createMonitorObject() ----- ")

  # Sanity checks
  if ( is.null(zipFile) ) {
    logger.error("Required parameter 'zipFile' is missing")
    stop(paste0("Required parameter 'zipFile' is missing"))
  }

  tbl <- epa_parseData(zipFile)

  # Create 'meta' dataframe
  logger.trace("Creating 'meta' dataframe ...")
  meta <- epa_createMetaDataframe(tbl,
                                  pwfslDataIngestSource = paste0('EPA_',basename(zipFile)),
                                  addGoogleMeta = addGoogleMeta)

  # Create 'data' dataframe
  logger.trace("Creating 'data' dataframe ...")
  data <- epa_createDataDataframe(tbl)

  # Create the 'ws_monitor' data list
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))

  # Reset all negative values that made it through QC to zero
  if ( zeroMinimum ) {
    logger.trace("Reset negative values to zero ...")
    ws_monitor <- monitor_replaceData(ws_monitor, data < 0, 0)
  }

  logger.trace(paste0('Finished creating ws_monitor object'))

  return(ws_monitor)

}

