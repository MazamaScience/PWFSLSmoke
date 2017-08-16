#' @keywords internal
#' @export
#' @title Ingest WRCC Dump File and Create ws_monitor Object
#' @param filepath absolute path of the WRCC dump file
#' @param clusterDiameter diameter in meters used to determine the number of clusters (see \code{addClustering})
#' @param existingMeta existing 'meta' dataframe from which to obtain metadata for known monitor deployments
#' @return A emph{ws_monitor} object with WRCC data.
#' @description Ingests a WRCC dump file and converts
#' it into a quality controlled, metadata enhanced \emph{ws_monitor} object
#' ready for use with all \code{monitor_~} functions.
#' 
#' Steps involved include:
#' 
#' \enumerate{
#'  \item{ingest CSV text}
#'  \item{parse CSV text}
#'  \item{apply quality control}
#'  \item{apply clustering to determine unique deployments}
#'  \item{enhance metadata to include: elevation, timezone, state, country, site name}
#'  \item{reshape data into deployment-by-property \code{meta} and and time-by-deployment \code{data} dataframes}
#' }
#' 
#' @note Each dump file must contain data for only one type of monitor, e.g. EBAM or E-Sampler.
#' @seealso \code{\link{wrccDump_parseData}}
#' @seealso \code{\link{wrcc_qualityControl}}
#' @seealso \code{\link{addClustering}}
#' @seealso \code{\link{wrcc_createMetaDataframe}}
#' @seealso \code{\link{wrcc_createDataDataframe}}

wrccDump_createMonitorObject <- function(filepath, clusterDiameter=1000, existingMeta=NULL) {

  logger.debug("Reading data ...")
  fileString <- readr::read_file(filepath)
  
  # Special parsing for dump files as the format is different from the WRCC CSV webservice
  logger.debug("Parsing data ...")
  dfList <- wrccDump_parseData(fileString)
  
  # empty list for ws_monitor objects
  monitorList <- list()
  
  # Loop over monitor dataframe list (mostly verbatim from wrcc_createMonitorObject)
  for ( name in names(dfList) ) {
  
    logger.info("Processing data for %s ...", name)
    
    df <- dfList[[name]]
    
    # Apply monitor-appropriate QC to the dataframe
    logger.debug("Applying QC logic ...")
    result <- try( df <- wrcc_qualityControl(df),
                   silent=TRUE ) # don't show errors
    
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      logger.warn(err_msg)
      next
    }
    
    # See if anything gets through QC
    if ( nrow(df) == 0 ) {
      logger.warn("No data remaining after QC")
      next
    }
    
    # Add clustering information to identify unique deployments
    logger.debug("Clustering ...")
    df <- addClustering(df, lonVar='GPSLon', latVar='GPSLat', clusterDiameter=clusterDiameter)
    
    # Create 'meta' dataframe of site properties organized as monitorID-by-property
    # NOTE:  This step will create a uniformly named set of properties and will
    # NOTE:  add site-specific information like timezone, elevation, address, etc.
    logger.debug("Creating 'meta' dataframe ...")
    meta <- wrcc_createMetaDataframe(df, existingMeta)
    
    # Create 'data' dataframe of PM2.5 values organized as time-by-monitorID
    logger.debug("Creating 'data' dataframe ...")
    data <- wrcc_createDataDataframe(df, meta)
    
    # Create the 'ws_monitor' object
    ws_monitor <- list(meta=meta, data=data)
    ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))

    # Reset all negative values that made it through QC to zero
    logger.debug("Reset negative valus to zero ...")
    ws_monitor <- monitor_replaceData(ws_monitor, data < 0, 0)
    
    monitorList[[name]] <- ws_monitor
    
  }
  
  wrcc <- monitor_combine(monitorList)
  
  return(wrcc)
  
}
