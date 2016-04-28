#' @keywords ws_monitor 
#' @export
#' @title Download and Convert Hourly EPA Air Quality Data
#' @param parameterName name of parameterName.
#' @param parameterCode specific parameter code (e.g. PM2.5 could be 88101 or 88502)
#' @param year year
#' @param verbose logical flag to generate verbose output
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
#' file <- epa_createMonitorObject("PM2.5", 88101, 2015, verbose=TRUE)
#' mon <- get(load(file))
#' }

epa_createMonitorObject <- function(parameterName="PM2.5", parameterCode=88101, year=2013,
                                    verbose=TRUE,
                                    baseUrl='http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/') {
  
  dataSource <- 'EPA'
  
  # Sanity check
  if ( requireNamespace('MazamaSpatialUtils', quietly=TRUE) ) {
    dummy <- getSpatialDataDir()
  }
    
  # Set up file names and paths
  fileBase <- paste("hourly",parameterCode,year,sep="_")
  url <- paste0(baseUrl,fileBase,".zip")
  zipFile <- paste0(getSmokeDataDir(),'/',fileBase,".zip")
  csvFile <- paste0(getSmokeDataDir(),'/',fileBase,".csv")
  
  download.file(url,zipFile,quiet=!verbose)
  
  if (verbose) cat(paste0('   Uncompressing ',fileBase,'.zip ...\n'))
  unzip(zipFile,exdir=getSmokeDataDir())
  if (verbose) cat(paste0('   Finished uncompressing\n'))
  
  
  # Here are the column names from an EPA hourly dataset:
  
  #   [1] "State Code"          "County Code"         "Site Num"            "Parameter Code"      "POC"                
  #   [6] "Latitude"            "Longitude"           "Datum"               "Parameter Name"      "Date Local"         
  #   [11] "Time Local"          "Date GMT"            "Time GMT"            "Sample Measurement"  "Units of Measure"   
  #   [16] "MDL"                 "Uncertainty"         "Qualifier"           "Method Type"         "Method Code"        
  #   [21] "Method Name"         "State Name"          "County Name"         "Date of Last Change"
  
  # Assign appropriate data types
  col_types <- paste0("ccccc","ddccc","cccdc","ddccc","cccc")
  
  # Read in the data
  if (verbose) cat(paste0('Reading in ',csvFile,'\n'))
  df <- readr::read_csv(csvFile, col_types=col_types, progress=verbose)
  if (verbose) cat(paste0('Finished reading in ',csvFile,'\n'))
  
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
  
  
  # Create meta dataframe -----------------------------------------------------
  
  if (verbose) cat(paste0('   Creating meta dataframe ...\n'))
  
  # The metadata file will strictly involve the data that doesn't involve the parameterNames.
  
  # Create a vector of column names. Create vector from df by inputing default x values and columns as the y values.
  columns <- c('Site Num','Parameter Code','POC','Latitude','Longitude','Units of Measure','MDL',
               'Method Type','Method Name','State Name','County Name','monitorID')
  dfSub <- df[,columns]
  
  # Create a new dataframe containing a single row for each monitorID
  # Create a mask that limits multiple readings from a single instruments with the !dupplicated() command.
  uniqueMonitorIDMask <- !duplicated(dfSub$monitorID)
  meta <- dfSub[uniqueMonitorIDMask,]
  
  # Add common metadata columns share among all PM2.5 datasets
  # latitude, longitude, elevation, monitorID, timezone, has_pm25
  meta$latitude <- meta$Latitude
  meta$longitude <- meta$Longitude

  # TODO:  code to get elevation from a location
  meta$elevation <- as.numeric(NA)
  meta$timezone <- as.character(NA)
  meta$stateCode <- as.character(NA)
  meta$siteName <- as.character(NA)

  # Add timezones only if the MazamaSpatialUtils package exists. This way we can compile and load this
  # package even if the MazamaSpatialUtils package is not present.
  
  if ( requireNamespace('MazamaSpatialUtils', quietly = TRUE) ) {
    
    dummy <- getSpatialDataDir()
    if (verbose) cat(paste0('   Determining timezones and stateCodes...\n')) 
    
    meta$timezone <- MazamaSpatialUtils::getTimezone(meta$longitude, meta$latitude, useBuffering=TRUE)

    meta$stateCode <- MazamaSpatialUtils::getStateCode(meta$longitude, meta$latitude, countryCodes=c('CA','US','MX'), useBuffering=TRUE)

  }
  
  
  # Create a vector of column names to be included in meta in the order of their inclusion
  columns <- c('monitorID','siteName','latitude','longitude','elevation','timezone','stateCode','Site Num','Parameter Code',
               'POC','Units of Measure','MDL','Method Type','Method Name','State Name','County Name')
  meta <- meta[,columns]
  rownames(meta) <- meta$monitorID
  
  # Make end users lives easier by using normal R names for the columns. (No more "meta$`Parameter Code`")
  names(meta) <- make.names(names(meta))
  
  # Remove the "tbl_df" class
  meta <- as.data.frame(meta)
  
  # Create data dataframe -----------------------------------------------------
  
  if (verbose) cat(paste0('   Creating data dataframe ...\n'))
  
  # The data file will refer to any of the original instrument's readings, the bulk of the information.
  
  # "melt" the data frame into long-format data
  # The "melt" function will turn the column names into their own column and the rest of the data into a second.
  molten <- reshape2::melt(data=df, id.vars = c("datetime","monitorID"), measure.vars="Sample Measurement")
  
  # "cast" the data frame into wide-format data with country code column names
  # The "cast" function will take the melted version of the data and and move desired columns out of their 
  # Original column and into their own columns.
  data <- reshape2::dcast(molten, datetime ~ monitorID)
  
  # Add rownames
  # TODO:  Figure out what is up with non-unique %Y%m%d%H
  # TODO:  rownames(data) <- strftime(data$datetime,"%Y%m%d%H") ### ERROR:  non-unique row names 2013110301 (daylight savings issue?)
  
  # Create the 'ws_monitor' data list
  ws_monitor <- list(meta=meta,
                     data=data)
  
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))
  
  # Create appropriate data object and file name and write the data to disk
  dfName <- paste(dataSource,parameterName,parameterCode,year,sep='_')  
  assign(dfName,ws_monitor)
  fileName <- paste0(getSmokeDataDir(),'/',dfName,'.RData')
  save(list=dfName,file=fileName)
  
  if (verbose) cat(paste0('   Finished creating ws_monitor object\n'))
  
  # Cleanup
  file.remove(zipFile, csvFile)
  
  # Return a 'prefix' for use with monitor_load()
  return(fileName)
  
}

