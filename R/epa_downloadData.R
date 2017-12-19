#' @keywords EPA
#' @export
#' @title Download Data from EPA
#' @param year year
#' @param parameterName pollutant name
#' @param parameterCode pollutant code
#' @param baseUrl base URL for archived daily data
#' @param downloadDir directory where files are downloaded and unzipped
#' @description This function downloads air quality data from the EPA and
#' converts it into a dataframe.
#' 
#' Available parameters include:
#' \enumerate{
#' \item{Ozone}
#' \item{SO2}
#' \item{CO}
#' \item{NO2}
#' \item{PM2.5}
#' \item{PM10}
#' \item{Wind}
#' \item{Temperatue}
#' \item{Barometric_Pressure}
#' \item{RH_and_Dewpoint}
#' \item{HAPs}
#' \item{VOCs}
#' \item{NONOxNOy}
#' }
#' 
#' Available parameter codes include:
#' \enumerate{
#' \item{44201}{ --Ozone}
#' \item{42401}{ --SO2}
#' \item{42101}{ --CO}
#' \item{42602}{ --NO2}
#' \item{88101}{ --PM2.5}
#' \item{88502}{ --PM2.5}
#' \item{81102}{ --PM10}
#' \item{SPEC}{ --PM2.5}
#' \item{WIND}{ --Wind}
#' \item{TEMP}{ --Temperature}
#' \item{PRESS}{ --Barometric Pressure}
#' \item{RH_DP}{ --RH and dewpoint}
#' \item{HAPS}{ --HAPs}
#' \item{VOCS}{ --VOCs}
#' \item{NONOxNOy}
#' }
#'
#' @note Unzipped files can be several hundred megabytes so downloadDir may need to be carefully chosen.
#' Downloaded and unzipped files are removed after data are read into a dataframe.
#' @return Tibble of EPA data.
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
#' @examples
#' \dontrun{
#' tbl <- epa_downloadData(2016, "PM2.5", "88101")
#' }


# if (false){
#   parameterName="PM2.5"
#   parameterCode=88101
#   year=2016
#   baseUrl='http://aqs.epa.gov/aqsweb/airdata/'
# }

epa_downloadData <- function(year=NULL,
                             parameterName="PM2.5",
                             parameterCode="88101",
                             baseUrl='https://aqs.epa.gov/aqsweb/airdata/',
                             downloadDir=tempdir()) {

  # Sanity Check -- validate parameter name
  validParameters <- c("Ozone", "SO2", "CO", "NO2", "PM2.5", "PM10", "Wind", "Temperature", "Barometric_Pressure",
                       "RH_and_Dewpoint", "HAPs", "VOCs", "NONOxNOy")
  
  if ( is.null(parameterName) ) {
    logger.error("Required parameter 'parameterName' is missing")
    stop("Required parameter 'parameterName' is missing")
  } else {
    if ( !parameterName %in% validParameters ) {
      logger.error("parameterName '%s' is not in: %s", parameterName, paste0(validParameters, collapse=", "))
      stop(paste0("parameterName '",parameterName,"' is not in: ", paste0(validParameters, collapse=", ")))
    }
  }
  
  # Sanity Check -- validate parameter code
  validParameterCodes <- c("44201", "42401", "42101", "42602", "88101", "88502", "81102", "SPEC",
                           "WIND", "TEMP", "PRESS", "RH_DP", "HAPS", "VOCS", "NONOxNOy")
  
  if ( is.null(parameterCode) ) {
    logger.error("Required parameter 'parameterCode' is missing")
    stop("Required parameter 'parameterCode' is missing")
  } else {
    parameterCode <- as.character(parameterCode)
    if ( !parameterCode %in% validParameterCodes ) {
      logger.error("parameterCode '%s' is not in: %s", parameterCode, paste0(validParameterCodes, collapse=", "))
      stop(paste0("parameterCode '",parameterCode,"' is not in: ", paste0(validParameterCodes, collapse=", ")))
    }
  }
  
  # Sanity check: year is supplied and valid
  if ( is.null(year) ) {
    logger.error("Required parameter 'year' is missing")
    stop(paste0("Required parameter 'year' is missing"))
  } else if ( year < 1990 ) {
    logger.error("No data available before 1990")
    stop(paste0("No data available before 1990"))
  } else if ( (parameterCode=="88101" && year<2008) ||
              (parameterCode=="88502" && year<1998) || 
              (parameterCode=="SPEC" && year<2001) ||
              (parameterCode=="HAPS" && year<1993) ) {
    logger.error("No data available for parameter code %s in year %i", parameterCode, year)  
    stop(sprintf("No data available for parameter code %s in year %i", parameterCode, year))
  }
  
  # Set up file names and paths
  fileBase <- paste("hourly",parameterCode,year,sep="_")
  url <- paste0(baseUrl,fileBase,".zip")
  zipFile <- paste0(downloadDir,'/',fileBase,".zip")
  csvFile <- paste0(downloadDir,'/',fileBase,".csv")
  
  # TODO:  Change to use httr and test for success
  
  utils::download.file(url,zipFile)
  
  logger.debug(paste0('Uncompressing ',fileBase,'.zip ...'))
  utils::unzip(zipFile, exdir=downloadDir)
  logger.debug(paste0('Finished uncompressing'))
  
  
  # Here are the column names from an EPA hourly dataset:
  
  #   [1] "State Code"          "County Code"         "Site Num"            "Parameter Code"      "POC"                
  #   [6] "Latitude"            "Longitude"           "Datum"               "Parameter Name"      "Date Local"         
  #   [11] "Time Local"          "Date GMT"            "Time GMT"            "Sample Measurement"  "Units of Measure"   
  #   [16] "MDL"                 "Uncertainty"         "Qualifier"           "Method Type"         "Method Code"        
  #   [21] "Method Name"         "State Name"          "County Name"         "Date of Last Change"
  
  # Assign appropriate data types
  col_types <- paste0("ccccc","ddccc","cccdc","ddccc","cccc")
  
  # Read in the data
  logger.debug(paste0('Reading in ',csvFile,' ...'))
  tbl <- readr::read_csv(csvFile, col_types=col_types)
  logger.debug(paste0('Finished reading in ',csvFile))
  
  # Cleanup
  file.remove(zipFile, csvFile)
  
  logger.info('Downloaded and parsed %d rows of EPA data', nrow(tbl))
  
  return(tbl)
}
