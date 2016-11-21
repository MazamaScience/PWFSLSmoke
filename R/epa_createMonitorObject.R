#' @keywords ws_monitor 
#' @export
#' @importFrom utils download.file
#' @title Download and Convert Hourly EPA Air Quality Data
#' @param parameterName name of parameterName.
#' @param parameterCode specific parameter code (e.g. PM2.5 could be 88101 or 88502)
#' @param year year
#' @param baseUrl base URL for archived hourly data
#' @description Convert EPA data into a ws_monitor object, ready for use with all monitor_~ functions.
#' @note Before running this function you must first enable spatial data capabilities as in the example.
#' @return Character string name of the generated .RData file.
#' @references \href{http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html}{EPA data files}
#' @references \href{http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/FileFormats.html#_format_3}{file format description}
#' @examples
#' \dontrun{
#' library(PWFSLSmoke)
#' setSmokeDataDir('~/Data/Smoke')
#' library(MazamaSpatialUtils)
#' setSpatialDataDir('~/Data/Spatial')
#' loadSpatialData('NaturalEarthAdm1') # stateCodes dataset
#' file <- epa_createMonitorObject("PM2.5", 88101, 2015)
#' mon <- get(load(file))
#' }

# if (false){
#   parameterName="PM2.5"
#   parameterCode=88101
#   year=NULL
#   baseUrl='http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/'
# }

epa_createMonitorObject <- function(parameterName=NULL, parameterCode=NULL, year=NULL,
                                    baseUrl='https://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/') {

  df <- epa_downloadData(parameterName, parameterCode, year, baseUrl)
      
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
  meta <- epa_createMetaDataframe(df)
  
  #Create 'data' dataframe
  data <- epa_createDataDataframe(df)
  
  # Create the 'ws_monitor' data list
  ws_monitor <- list(meta=meta,
                     data=data)
  
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  # Create appropriate data object and file name and write the data to disk
  dfName <- paste(dataSource, parameterName, parameterCode, year,sep='_')  
  assign(dfName, ws_monitor)
  fileName <- paste0(getSmokeDataDir(), '/', dfName, '.RData')
  save(list=dfName, file=fileName)
  
  logger.debug(paste0('   Finished creating ws_monitor object\n'))
  
  # Return a 'prefix' for use with monitor_load()
  return(fileName)
  
}

