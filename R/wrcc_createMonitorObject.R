#' @keywords WRCC
#' @export
#' @title Obtain WRCC Data and Create ws_monitor Object
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param stationID station identifier (will be upcased)
#' @param clusterDiameter diameter in meters used to determine the number of clusters (see \code{addClustering})
#' @param baseUrl base URL for data queries
#' @param saveFile optional filename where raw CSV will be written
#' @return A ws_monitor object with a WRCC data.
#' @description Obtains monitor data from an WRCC webservice and converts
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
#' @seealso \code{\link{wrcc_downloadData}}
#' @seealso \code{\link{wrcc_parseData}}
#' @seealso \code{\link{wrcc_qualityControl}}
#' @seealso \code{\link{addClustering}}
#' @seealso \code{\link{wrcc_createMetaDataframe}}
#' @seealso \code{\link{wrcc_createDataDataframe}}

wrcc_createMonitorObject <- function(startdate=20020101,
                                     enddate=strftime(lubridate::now(),"%Y%m%d",tz="GMT"),
                                     stationID=NULL,
                                     clusterDiameter=1000,
                                     baseUrl="http://www.wrcc.dri.edu/cgi-bin/wea_list2.pl",
                                     saveFile=NULL) {
  
  # Sanity checks
  if ( is.null(stationID) ) {
    logger.error("Required parameter 'stationID' is missing")
    stop(paste0("Required parameter 'stationID' is missing"))
  }
  
  # Read in WRCC .csv data
  logger.info("Downloading WRCC data ...")
  fileString <- wrcc_downloadData(startdate, enddate, stationID, baseUrl)
  
  # Optionally save as a raw .csv file
  if ( !is.null(saveFile) ) {
    result <- try( cat(fileString, file=saveFile),
                   silent=TRUE )
    if ( class(result)[1] == "try-error" ) {
      err_msg <- geterrmessage()
      logger.warn("Unable to save data to local file %s: %s", saveFile, err_msg)
    }
    # NOTE:  Processing continues even if we fail to write the local file
  }
  
  # Read csv raw data into a dataframe
  logger.info("Parsing data ...")
  df <- wrcc_parseData(fileString)
  
  # Apply monitor-appropriate QC to the dataframe
  logger.info("Applying QC logic ...")
  df <- wrcc_qualityControl(df)
  
  # See if anything gets through QC
  if ( nrow(df) == 0 ) {
    logger.warn("No data remaining after QC")
    stop("No data remaining after QC")
  }
  
  # Add clustering information to identify unique deployments
  logger.info("Clustering ...")
  df <- addClustering(df, lonVar='GPSLon', latVar='GPSLat', clusterDiameter=1000)
  
  # Create 'meta' dataframe of site properties organized as monitorID-by-property
  # NOTE:  This step will create a uniformly named set of properties and will
  # NOTE:  add site-specific information like timezone, elevation, address, etc.
  logger.info("Creating 'meta' dataframe ...")
  meta <- wrcc_createMetaDataframe(df)
  
  # Create 'data' dataframe of PM2.5 values organized as hour-by-monitorID
  logger.info("Creating 'data' dataframe ...")
  data <- wrcc_createDataDataframe(df, meta)
  
  # Create the 'ws_monitor' object
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return(ws_monitor)
  
}
