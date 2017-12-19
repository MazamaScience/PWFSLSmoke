#' @keywords EPA
#' @export
#' @title Download and Convert Hourly EPA Air Quality Data
#' @param year year
#' @param parameterName pollutant name
#' @param parameterCode pollutant code
#' @param baseUrl base URL for archived daily data
#' @param downloadDir directory where files are downloaded and unzipped
#' @param addGoogleMeta logicial specifying wheter to use Google elevation and reverse geocoding services
#' @description Convert EPA data into a \emph{ws_monitor} object, ready for use with all \code{monitor_~} functions.
#' @note Before running this function you must first enable spatial data capabilities as in the example.
#' @return A \emph{ws_monitor} object with EPA data.
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_format_3}{file format description}
#' @examples
#' \dontrun{
#' library(PWFSLSmoke)
#' library(MazamaSpatialUtils)
#' setSpatialDataDir('~/Data/Spatial')
#' loadSpatialData('NaturalEarthAdm1') # stateCodes dataset
#' mon <- epa_createMonitorObject(2015, "PM2.5", "88101")
#' }

epa_createMonitorObject <- function(year=NULL,
                                    parameterName="PM2.5",
                                    parameterCode="88101",
                                    baseUrl='https://aqs.epa.gov/aqsweb/airdata/',
                                    downloadDir=tempdir(),
                                    addGoogleMeta=TRUE) {

  tbl <- epa_downloadData(year, parameterName, parameterCode, baseUrl, downloadDir)
      
  # Create a column with the unique monitorID using fields 1:3 in the "file format description" above
  tbl$monitorID <- paste0(tbl$`State Code`,tbl$`County Code`,tbl$`Site Num`)

  # Create a column with the datetime
  tbl$datetime <- lubridate::ymd_hms(paste0(tbl$`Date GMT`,' ',tbl$`Time GMT`,':00'))
  
  # check if MazamaSpatialUtils data are loaded 
  if ( !exists('NaturalEarthAdm1') ) {
    # NOTE:  Timezone, countryCode and stateCode information is mandatory for ws_monitor objects.
    logger.error("MazamaSpatialUtils package was not properly initialized -- no Mazama metadata added")
    stop(paste0("MazamaSpatialUtils package was not properly initialized.\n",
                "Please run:  library(MazamaSpatialUtils); setSpatialDataDir('~/Data/Spatial'); loadSpatialData('NaturalEarthAdm1')"))
  }
  
  # Create 'meta' dataframe
  logger.info("Creating 'meta' dataframe ...")
  meta <- epa_createMetaDataframe(tbl, addGoogleMeta = addGoogleMeta)
  
  # Create 'data' dataframe
  logger.info("Creating 'data' dataframe ...")
  data <- epa_createDataDataframe(tbl)
  
  # Create the 'ws_monitor' data list
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  logger.debug(paste0('   Finished creating ws_monitor object\n'))
  
  return(ws_monitor)
  
}

