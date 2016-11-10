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

epa_createMonitorObject <- function(parameterName="PM2.5", parameterCode=88101, year=NULL,
                                    baseUrl='http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/') {
  
  dataSource <- 'EPA'
  
  # Sanity check: year is supplied
  if(is.null(year)){
    logger.error("Reuiqred parameter 'year' is missing")
    stop(paste0("Reuiqred parameter 'year' is missing"))
  }
  
  # Sanity check
  if ( requireNamespace('MazamaSpatialUtils', quietly=TRUE) ) {
    dummy <- getSpatialDataDir()
  }
  
  # Set up file names and paths
  fileBase <- paste("hourly",parameterCode,year,sep="_")
  url <- paste0(baseUrl,fileBase,".zip")
  zipFile <- paste0(getSmokeDataDir(),'/',fileBase,".zip")
  csvFile <- paste0(getSmokeDataDir(),'/',fileBase,".csv")
  
  utils::download.file(url,zipFile)
  
  logger.debug(paste0('   Uncompressing ',fileBase,'.zip ...\n'))
  utils::unzip(zipFile,exdir=getSmokeDataDir())
  logger.debug(paste0('   Finished uncompressing\n'))
  
  
  # Here are the column names from an EPA hourly dataset:
  
  #   [1] "State Code"          "County Code"         "Site Num"            "Parameter Code"      "POC"                
  #   [6] "Latitude"            "Longitude"           "Datum"               "Parameter Name"      "Date Local"         
   #   [11] "Time Local"          "Date GMT"            "Time GMT"            "Sample Measurement"  "Units of Measure"   
  #   [16] "MDL"                 "Uncertainty"         "Qualifier"           "Method Type"         "Method Code"        
  #   [21] "Method Name"         "State Name"          "County Name"         "Date of Last Change"
  
  # Assign appropriate data types
  col_types <- paste0("ccccc","ddccc","cccdc","ddccc","cccc")
  
  # Read in the data
  logger.debug(paste0('Reading in ',csvFile,'\n'))
  df <- readr::read_csv(csvFile, col_types=col_types)
  logger.debug(paste0('Finished reading in ',csvFile,'\n'))
  
  # NOTE:  Unique MonitorID values are described at: http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/FileFormats.html#_format_3
  # NOTE:  
  # NOTE:  For the purposes of AQS, a monitor does not refer to a specific piece of equipment. Instead, it reflects 
  # NOTE:  that a given parameterName (or other parameter) is being measured at a given site.
  # NOTE:  
  # NOTE:  Identified by:
  # NOTE:  The site (state + county + site number) where the monitor is located AND
  # NOTE:  
  # NOTE:  The parameterName code AND
  # NOTE:  
  # NOTE:  POC – Parameter Occurrence Code. Used to uniquely identify a monitor if there is more than one device 
  # NOTE:        measuring the same parameterName at the same site.
  # NOTE:  
  # NOTE:  For example monitor IDs are usually written in the following way:
  # NOTE:  
  # NOTE:  SS-CCC-NNNN-PPPPP-Q
  # NOTE:  
  # NOTE:  where SS is the State FIPS code, CCC is the County FIPS code, and NNNN is the Site Number within the county 
  # NOTE:  (leading zeroes are always included for these fields), PPPPP is the AQS 5-digit parameter code, and Q is the POC. For example:
  # NOTE:  
  # NOTE:  01-089-0014-44201-2
  # NOTE:  
  # NOTE:  is Alabama, Madison County, Site Number 14, ozone monitor, POC 2.
  
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
  
  # Cleanup
  file.remove(zipFile, csvFile)
  
  # Return a 'prefix' for use with monitor_load()
  return(fileName)
  
}

