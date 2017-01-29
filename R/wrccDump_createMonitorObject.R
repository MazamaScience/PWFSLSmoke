#' @keywords WRCC
#' @export
#' @title Ingest WRCC Dump File and Create ws_monitor Object
#' @param filepath absolute path of the WRCC dump file
#' @return A ws_monitor object with WRCC data.
#' @description Ingests an  WRCC dump file and converts
#' it into a quality controlled, metadata enhanced \code{ws_monitor} object
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
#'  \item{reshape data into deployment-by-property 'meta' and and time-by-deployment 'data' dataframes}
#' }
#' 
#' @note Each dump file must contain data for only one type of monitor, e.g. EBAM or E-Sampler.
#' @seealso \code{\link{wrccDump_parseData}}
#' @seealso \code{\link{wrcc_qualityControl}}
#' @seealso \code{\link{addClustering}}
#' @seealso \code{\link{wrcc_createMetaDataframe}}
#' @seealso \code{\link{wrcc_createDataDataframe}}

wrccDump_createMonitorObject <- function(filepath) {

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
    logger.info("Applying QC logic ...")
    df <- wrcc_qualityControl(df)
    
    # See if anything gets through QC
    if ( nrow(df) == 0 ) {
      logger.warn("No data remaining after QC")
      next
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
    
    monitorList[[name]] <- ws_monitor
    
  }
  
  wrcc <- monitor_combine(monitorList)
  
  return(wrcc)
  
}
