#' @keywords AIRSIS
#' @export
#' @title Combine Individual Monitor Files
#' @param outputDir directory where data files are located
#' @param inputFiles vector of file names to be ingested
#' @param outputFile name of combined file
#' @description Load and combine .RData files containing \code{ws_monitor} objects.
#' 
#' When \code{inputFiles=NULL} (the default), all .RData files are used.
#' @return Name of combined file.
#' @seealso \link{airsis_buildLibrary}
#' @references \href{http://usfs.airsis.com}{Interagency Real Time Smoke Monitoring}
#' @examples
#' \dontrun{
#' files <- airsis_buildLibrary('APCD', years=2010:2015, outputDir='~/Data/AIRSIS')
#' monitor_combineLibraryFiles('~/Data/AIRSIS', files, 'AIRSIS_APCD.RData')
#' apcd_CA <- <- airsis_load(stateCodes='CA', url='~/Data/AIRSIS/AIRSIS_APCD.RData')
#' monitor_leaflet(apcd_CA)
#' }

monitor_combineLibraryFiles <- function(outputDir=getwd(), inputFiles=NULL, outputFile=NULL) {
  
  # Create a vector of intput paths
  if ( is.null(inputFiles) ) {
    inputFiles <- list.files(outputDir, pattern='.*\\.RData')
  }
  inputFiles <- file.path(outputDir, inputFiles)
  
  # Load all ws_monitor objects into a list
  monitorList <- vector('list',length(inputFiles))
  for ( i in seq(length(inputFiles)) ) {
    monitorList[[i]] <- get(load(inputFiles[i]))
  }
  
  # meta dataframe for all monitors
  metaList <- lapply(monitorList, '[[', 'meta')
  meta <- dplyr::bind_rows(metaList)
  
  # Find the overall starttime and endtime
  starttime <- min( sapply(monitorList, function(x) { min(x$data$datetime) }) )
  starttime <- as.POSIXct(starttime, tz="UTC", origin=lubridate::origin)
  endtime <- max( sapply(monitorList, function(x) { max(x$data$datetime) }) )
  endtime <- as.POSIXct(endtime, tz="UTC", origin=lubridate::origin)
  
  # Create a empty dataframe with just the datetime
  datetime <- seq(starttime,endtime,by="hours")
  data <- data.frame(datetime=datetime)
  
  # Loop through all data dataframes, merging the contents
  for (monitor in monitorList) {
    data <- dplyr::left_join(data, monitor$data, by='datetime')
  }
  
  # Create the ws_monitor object
  ws_monitor <- list(meta=as.data.frame(meta), data=as.data.frame(data))
  ws_monitor <- structure(ws_monitor, class=c("ws_monitor","list"))

  # Save the combined file
  save(ws_monitor, file=file.path(outputDir, outputFile))
  
}

