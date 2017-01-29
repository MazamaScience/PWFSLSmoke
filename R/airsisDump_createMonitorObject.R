#' @keywords AIRSIS
#' @export
#' @title Ingest AIRSIS Dump File and Create ws_monitor Object
#' @param filepath absolute path of the AIRSIS dump file
#' @return A ws_monitor object with AIRSIS data.
#' @description Ingests an  AIRSIS dump file and converts
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
#' @seealso \code{\link{airsisDump_parseData}}
#' @seealso \code{\link{airsis_qualityControl}}
#' @seealso \code{\link{addClustering}}
#' @seealso \code{\link{airsis_createMetaDataframe}}
#' @seealso \code{\link{airsis_createDataDataframe}}

airsisDump_createMonitorObject <- function(filepath) {

  logger.debug("Reading data ...")
  fileString <- readr::read_file(filepath)
  
  # Special parsing for dump files as the format is different from the AIRSIS CSV webservice
  logger.debug("Parsing data ...")
  df <- airsisDump_parseData(fileString)
  
  # At this point df has multiple monitors in it.
  
  # Standard quality control still works
  logger.debug("Applying QC logic ...")
  if ( df$monitorType[1] == 'ESAM' ) {
    # NOTE:  Conversation with Sim and Lee on 2015-07-09. Accept all values of RHi for E-Samplers
    df <- airsis_ESAMQualityControl(df, valid_RHi=c(-Inf,Inf))
  } else {
    df <- airsis_qualityControl(df)
  }
  
  # Change the name here so that we can continue to use 'df' in the loop to match code in airsis_createMonitorObject.R.
  dfCombined <- df
  
  # Loop through df$Alias and run each chunk through further data processing
  metaList <- list()
  dataList <- list()
  for (alias in unique(dfCombined$Alias)) {
    logger.info("Processing data for %s ...", alias)
    df <- dplyr::filter(dfCombined, dfCombined$Alias == alias)
    
    # Add clustering information to identify unique deployments
    logger.debug("Clustering ...")
    df <- addClustering(df, lonVar='Longitude', latVar='Latitude', clusterDiameter=1000)
    
    # Create 'meta' dataframe of site properties organized as monitorID-by-property
    # NOTE:  This step will create a uniformly named set of properties and will
    # NOTE:  add site-specific information like timezone, elevation, address, etc.
    logger.debug("Creating 'meta' dataframe ...")
    meta <- airsis_createMetaDataframe(df)
    
    # Create 'data' dataframe of PM2.5 values organized as hour-by-monitorID
    logger.debug("Creating 'data' dataframe ...")
    data <- airsis_createDataDataframe(df, meta)
    
    metaList[[alias]] <- meta
    dataList[[alias]] <- data
  }
  
  # NOTE:  Could have done this inside the loop but leaving metaList and dataList in tact
  # NOTE:  for debugging purposes.
  
  # Create combined 'meta'
  logger.debug("Combining 'meta' dataframes ...")
  meta <- dplyr::bind_rows(metaList)
  
  # Create combined 'data'
  logger.debug("Combining 'data' dataframes ...")
  alias <- names(dataList[1])
  data <- dataList[[alias]]
  for (alias in names(dataList)[-1]) {
    data <- dplyr::full_join(data,dataList[[alias]],by="datetime")
  }
  
  # Create the 'ws_monitor' object
  ws_monitor <- list(meta=as.data.frame(meta), data=as.data.frame(data))
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return(ws_monitor)
  
}
