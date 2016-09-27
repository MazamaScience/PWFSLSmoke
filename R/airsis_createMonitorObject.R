#' @keywords AIRSIS
#' @export
#' @title Obtain AIRSIS Data and Create ws_monitor Object
#' @param provider identifier used to modify baseURL \code{['APCD'|'USFS']}
#' @param unitID character or numeric AIRSIS unit identifier
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param clusterDiameter diameter in meters used to determine the number of clusters (see \code{addClustering})
#' @param baseUrl base URL for data queries
#' @param saveFile optional filename where raw CSV will be written
#' @description Obtains monitor data from an AIRSIS webservice and converts
#' it into a quality controlled, metadata enhanced \code{ws_monitor} object
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
#'  \item{reshape data into deployment-by-property 'meta' and and time-by-deployment 'data' dataframes}
#' }
#' 
#' @note The downloaded CSV may be saved to a local file by providing an argument to the \code{saveFile} parameter.
#' @return ws_monitor object with a unique `monitorID` for each unique deployment.
#' @seealso \code{\link{airsis_downloadData}}
#' @seealso \code{\link{airsis_parseData}}
#' @seealso \code{\link{airsis_qualityControl}}
#' @seealso \code{\link{addClustering}}
#' @seealso \code{\link{airsis_createMetaDataframe}}
#' @seealso \code{\link{airsis_createDataDataframe}}

airsis_createMonitorObject <- function(provider='USFS', unitID=NULL,
                                       startdate=20020101,
                                       enddate=strftime(lubridate::now(),"%Y%m%d",tz="GMT"),
                                       clusterDiameter=1000,
                                       baseUrl="http://xxxx.airsis.com/vision/common/CSVExport.aspx?",
                                       saveFile=NULL,
                                       returnQCDataframe=FALSE) {

  # Sanity checks
  if ( is.null(unitID) ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing."))
  }

  # Read in AIRSIS .csv data
  logger.info('Downloading data...')
  fileString <- airsis_downloadData(provider, unitID=unitID, startdate, enddate, baseUrl)
  
  # Optionally save as a raw .csv file
  if ( !is.null(saveFile) ) {
    result <- try( cat(fileString, file=saveFile),
                   silent=TRUE )
    if ( class(result)[1] == "try-error" ) {
      err_msg <- geterrmessage()
      logger.warn('Unable to save data to local file %s: %s', saveFile, err_msg)
    }
    # NOTE:  Processing continues even if we fail to write the local file
  }
  
  # Read csv raw data into a dataframe
  logger.info('Parsing data...')
  df <- airsis_parseData(fileString)
  
  # Apply monitor-appropriate QC to the dataframe
  logger.info('Applying QC logic...')
  df <- airsis_qualityControl(df)
  
  # Add clustering information to identify unique deployments
  logger.info('Clustering...')
  df <- addClustering(df, lonVar='Longitude', latVar='Latitude', clusterDiameter=1000)
  
  # Create 'meta' dataframe of site properties organized as monitorID-by-property
  # NOTE:  This step will create a uniformly named set of properties and will
  # NOTE:  add site-specific information like timezone, elevation, address, etc.
  logger.info('Creating \'meta\' dataframe...')
  meta <- airsis_createMetaDataframe(df)
  
  # Create 'data' dataframe of PM2.5 values organized as hour-by-monitorID
  logger.info('Creating \'data\' dataframe...')
  data <- airsis_createDataDataframe(df, meta)
  
  # Create the 'ws_monitor' object
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return(ws_monitor)
  
}
