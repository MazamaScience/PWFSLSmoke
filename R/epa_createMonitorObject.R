#' @keywords EPA
#' @export
#' @title Download and Convert Hourly EPA Air Quality Data
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
#' initializeMazamaSpatialUtils()
#' zipFile <- epa_downloadData(2016, "88101", downloadDir='~/Data/EPA')
#' mon <- epa_createMonitorObject(zipFile, addGoogleMeta=FALSE)
#' }

epa_createMonitorObject <- function(zipFile=NULL,
                                    zeroMinimum=TRUE,
                                    addGoogleMeta=TRUE) {

  # Sanity checks
  if ( is.null(zipFile) ) {
    logger.error("Required parameter 'zipFile' is missing")
    stop(paste0("Required parameter 'zipFile' is missing"))
  }
  
  tbl <- epa_parseData(zipFile)
      
  # Create 'meta' dataframe
  logger.debug("Creating 'meta' dataframe ...")
  meta <- epa_createMetaDataframe(tbl,
                                  pwfslDataIngestSource = paste0('EPA_',basename(zipFile)),
                                  addGoogleMeta = addGoogleMeta)
  
  # Create 'data' dataframe
  logger.debug("Creating 'data' dataframe ...")
  data <- epa_createDataDataframe(tbl)
  
  # Create the 'ws_monitor' data list
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))

  # Reset all negative values that made it through QC to zero
  if ( zeroMinimum ) {
    logger.debug("Reset negative values to zero ...")
    ws_monitor <- monitor_replaceData(ws_monitor, data < 0, 0)
  }
  
  logger.debug(paste0('Finished creating ws_monitor object'))
  
  return(ws_monitor)
  
}

