#' @keywords AIRSIS
#' @export
#' @title Ingest AIRSIS Dump File and Create ws_monitor Object
#' @param filepath absolute path of the AIRSIS dump file
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
#' @return ws_monitor object with a unique `monitorID` for each unique deployment.
#' @seealso \code{\link{airsisDump_parseData}}
#' @seealso \code{\link{airsis_qualityControl}}
#' @seealso \code{\link{addClustering}}
#' @seealso \code{\link{airsis_createMetaDataframe}}
#' @seealso \code{\link{airsis_createDataDataframe}}

airsisDump_createMonitorObject <- function(filepath) {

  logger.debug('Reading data...')
  fileString <- readr::read_file(filepath)
  
  # Special parsing for dump files as the format is different from the AIRSIS CSV webservice
  logger.debug('Parsing data...')
  df <- airsisDump_parseData(fileString)
  
  # At this point df has multiple monitors in it.
  
  # Standard quality control still works
  logger.debug('Applying QC logic...')
  df <- airsis_qualityControl(df)
  
  # Change the name here so that we can continue to use 'df' in the loop to match code in airsis_createMonitorObject.R.
  dfCombined <- df
  
  # Loop through df$Alias and run each chunk through further data processing
  metaList <- list()
  dataList <- list()
  for (singleAlias in unique(dfCombined$Alias)) {
    # NOTE:  You cannot assign the alias name to "Alias" in each loop as the dplyr expression
    # NOTE:  "Alias == Alias" will always evaluate to TRUE.
    logger.info('Processing data for %s...', singleAlias)
    df <- dplyr::filter(dfCombined, Alias == singleAlias)
    
    # Add clustering information to identify unique deployments
    logger.debug('Clustering...')
    df <- addClustering(df, lonVar='Longitude', latVar='Latitude', clusterDiameter=1000)
    
    # Create 'meta' dataframe of site properties organized as monitorID-by-property
    # NOTE:  This step will create a uniformly named set of properties and will
    # NOTE:  add site-specific information like timezone, elevation, address, etc.
    logger.debug('Creating \'meta\' dataframe...')
    meta <- airsis_createMetaDataframe(df)
    
    # Create 'data' dataframe of PM2.5 values organized as hour-by-monitorID
    logger.debug('Creating \'data\' dataframe...')
    data <- airsis_createDataDataframe(df, meta)
    
    metaList[[singleAlias]] <- meta
    dataList[[singleAlias]] <- data
  }
  
  # NOTE:  Could have done this inside the loop but leaving metaList and dataList in tact
  # NOTE:  for debugging purposes.
  
  # Create combined 'meta'
  logger.debug('Combining \'meta\' dataframes...')
  meta <- dplyr::bind_rows(metaList)
  
  # Create combined 'data'
  logger.debug('Combining \'data\' dataframes ...')
  singleAlias <- names(dataList[1])
  data <- dataList[[singleAlias]]
  for (singleAlias in names(dataList)[-1]) {
    data <- dplyr::full_join(data,dataList[[singleAlias]],by="datetime")
  }
  
  # Create the 'ws_monitor' object
  ws_monitor <- list(meta=as.data.frame(meta), data=as.data.frame(data))
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  return(ws_monitor)
  
}
