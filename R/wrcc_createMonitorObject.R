#' @keywords WRCC
#' @export
#' @title Convert WRCC Data into a ws_monitor Object
#' @param df single site dataframe created by wrcc_readData()
#' @param clusterDiameter diameter in meters used to determine the number of clusters (see description)
#' @param verbose logical flag to generate verbose output
#' @description Convert a WRCC dataframe into a ws_monitor object, ready for use
#' with all monitor_~ functions. Steps involved include:
#' 
#' \enumerate{
#'  \item{quality control}
#'  \item{clustering to determine unique deployments}
#'  \item{metadata enhancement including: elevation, timezone, state, country, site name}
#'  \item{reshaping data into a time-by-deployment dataframe}
#' }
#' 
#' @return ws_monitor object with a unique `monitorID` for each unique location

wrcc_createMonitorObject <- function(df, clusterDiameter=1000, verbose=FALSE) {
  
  # Apply quality control
  df <- wrcc_qualityControl(df, verbose=verbose)
  
  # Apply clustering
  df <- wrcc_clustering(df, clusterDiameter, verbose=verbose)
  
  # Create 'meta' dataframe
  meta <- wrcc_createMetaDataframe(df, verbose=verbose)
  
  # Create 'data' dataframe
  data <- wrcc_createDataDataframe(df, meta, verbose=verbose)
  
  # Create the 'ws_monitor' data list
  ws_monitor <- list(meta=meta,
                     data=data)
 
  return(structure(ws_monitor, class = c("ws_monitor", "list")))
  
}
