#' @keywords EPA
#' @export
#' @title Download and Convert Hourly EPA Air Quality Data
#' @param year year
#' @param parameterName pollutant name
#' @param parameterCode pollutant code
#' @param baseUrl base URL for archived daily data
#' @param downloadDir directory where files are downloaded and unzipped
#' @description Convert EPA data into a ws_monitor object, ready for use with all monitor_~ functions.
#' @note Before running this function you must first enable spatial data capabilities as in the example.
#' @return A ws_monitor object with EPA data.
#' @references \href{https://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
#' @references \href{https://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/FileFormats.html#_format_3}{file format description}
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
                                    baseUrl='https://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/',
                                    downloadDir=tempdir()) {

  df <- epa_downloadData(year, parameterName, parameterCode, baseUrl, downloadDir)
      
  dataSource <- 'EPA'
  
  # Create a column with the unique monitorID using the method described above
  df$monitorID <- paste(df$`State Code`,df$`County Code`,df$`Site Num`,df$`Parameter Code`,df$POC,sep='_')
  
  # Create a column with the datetime
  df$datetime <- lubridate::ymd_hms(paste0(df$`Date GMT`,' ',df$`Time GMT`,':00'))
  
  # check if MazamaSpatialUtils data are loaded 
  if ( !exists('NaturalEarthAdm1') ) {
    # NOTE:  Timezone, countryCode and stateCode information is mandatory for ws_monitor objects.
    logger.error("MazamaSpatialUtils package was not properly initialized -- no Mazama metadata added")
    stop(paste0("MazamaSpatialUtils package was not properly initialized.\n",
                "Please run:  library(MazamaSpatialUtils); setSpatialDataDir('~/Data/Spatial'); loadSpatialData('NaturalEarthAdm1')"))
  }
  
  # Create 'meta' dataframe
  logger.info("Creating 'meta' dataframe ...")
  meta <- epa_createMetaDataframe(df)
  
  # Create 'data' dataframe
  logger.info("Creating 'data' dataframe ...")
  data <- epa_createDataDataframe(df)
  
  # Create the 'ws_monitor' data list
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  logger.debug(paste0('   Finished creating ws_monitor object\n'))
  
  # # Create appropriate data object and file name and write the data to disk
  # dfName <- paste(dataSource, parameterName, parameterCode, year,sep='_')  
  # assign(dfName, ws_monitor)
  # fileName <- paste0(getSmokeDataDir(), '/', dfName, '.RData')
  # save(list=dfName, file=fileName)
  # 
  # # Return a 'prefix' for use with monitor_load()
  # return(fileName)
  
  return(ws_monitor)
  
}

