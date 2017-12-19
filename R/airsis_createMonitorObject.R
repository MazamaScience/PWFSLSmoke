#' @keywords AIRSIS
#' @export
#' @title Obain AIRSIS Data and Create ws_monitor Object
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param provider identifier used to modify baseURL \code{['APCD'|'USFS']}
#' @param unitID character or numeric AIRSIS unit identifier
#' @param clusterDiameter diameter in meters used to determine the number of clusters (see \code{addClustering})
#' @param baseUrl base URL for data queries
#' @param saveFile optional filename where raw CSV will be written
#' @return A \emph{ws_monitor} object with AIRSIS data.
#' @description Obtains monitor data from an AIRSIS webservice and converts
#' it into a quality controlled, metadata enhanced \emph{ws_monitor} object
#' ready for use with all \code{monitor_~} functions.
#' 
#' Steps involved include:
#' 
#' \enumerate{
#'  \item{download CSV text}
#'  \item{parse CSV text}
#'  \item{apply quality control}
#'  \item{apply clustering to determine unique deployments}
#'  \item{enhance metadata to include: elevation, timezone, state, country, site name}
#'  \item{reshape AIRSIS data into deployment-by-property \code{meta} and and time-by-deployment \code{data} dataframes}
#' }
#' 
#' @note The downloaded CSV may be saved to a local file by providing an argument to the \code{saveFile} parameter.
#' @seealso \code{\link{airsis_downloadData}}
#' @seealso \code{\link{airsis_parseData}}
#' @seealso \code{\link{airsis_qualityControl}}
#' @seealso \code{\link{addClustering}}
#' @seealso \code{\link{airsis_createMetaDataframe}}
#' @seealso \code{\link{airsis_createDataDataframe}}
#' @examples
#' \dontrun{
#' usfs_1013 <- airsis_createMonitorObject(20150301, 20150831, 'USFS', unitID='1013')
#' monitorLeaflet(usfs_1013)
#' }

airsis_createMonitorObject <- function(startdate=20100101,
                                       enddate=strftime(lubridate::now(),"%Y%m%d",tz="GMT"),
                                       provider=NULL,
                                       unitID=NULL,
                                       clusterDiameter=1000,
                                       baseUrl="http://xxxx.airsis.com/vision/common/CSVExport.aspx?",
                                       saveFile=NULL) {

  # Sanity checks
  if ( is.null(provider) ) {
    logger.error("Required parameter 'provider' is missing")
    stop(paste0("Required parameter 'provider' is missing"))
  }

  if ( is.null(unitID) ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }

  startdateCount <- stringr::str_count(as.character(startdate))
  if ( !startdateCount %in% c(8,10,12) ) {
    logger.error("Cannot parse 'startdate' with %d characters", startdateCount)
    stop(paste0("Cannot parse 'startdate' with ",startdateCount," characters"))
  }
  
  enddateCount <- stringr::str_count(as.character(enddate))
  if ( !enddateCount %in% c(8,10,12) ) {
    logger.error("Cannot parse 'enddate' with %d characters", enddateCount)
    stop(paste0("Cannot parse 'enddate' with ",enddateCount," characters"))
  }
  
  # Read in AIRSIS .csv data
  logger.info("Downloading AIRSIS data ...")
  fileString <- airsis_downloadData(startdate, enddate, provider, unitID, baseUrl)
  
  # Optionally save as a raw .csv file
  if ( !is.null(saveFile) ) {
    result <- try( cat(fileString, file=saveFile),
                   silent=TRUE )
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      logger.warn("Unable to save data to local file %s: %s", saveFile, err_msg)
    }
    # NOTE:  Processing continues even if we fail to write the local file
  }
  
  # Read csv raw data into a dataframe
  logger.debug("Parsing data ...")
  tbl <- airsis_parseData(fileString)
  
  # Apply monitor-appropriate QC to the dataframe
  logger.debug("Applying QC logic ...")
  tbl <- airsis_qualityControl(tbl)
  
  # See if anything gets through QC
  if ( nrow(tbl) == 0 ) {
    logger.warn("No data remaining after QC")
    stop("No data remaining after QC")
  }
  
  # Add clustering information to identify unique deployments
  logger.debug("Clustering ...")
  tbl <- addClustering(tbl, lonVar='Longitude', latVar='Latitude', clusterDiameter=clusterDiameter)
  
  # Create 'meta' dataframe of site properties organized as monitorID-by-property
  # NOTE:  This step will create a uniformly named set of properties and will
  # NOTE:  add site-specific information like timezone, elevation, address, etc.
  logger.debug("Creating 'meta' dataframe ...")
  meta <- airsis_createMetaDataframe(tbl, provider, unitID, 'AIRSIS')
  
  # Create 'data' dataframe of PM2.5 values organized as time-by-monitorID
  logger.debug("Creating 'data' dataframe ...")
  data <- airsis_createDataDataframe(tbl, meta)
  
  # Create the 'ws_monitor' object
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  # Reset all negative values that made it through QC to zero
  logger.debug("Reset negative valus to zero ...")
  ws_monitor <- monitor_replaceData(ws_monitor, data < 0, 0)
  
  return(ws_monitor)
  
}
