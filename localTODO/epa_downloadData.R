#' @keywords EPA
#' @export
#' @title Download hourly air-qiality data from a given year
#' @param parameterName pollutant name
#' @param parameterCode a character string of pollutant code
#' @param year a numeric year value
#' @param baseUrl base URL for archived daily data
#' @description This function downloads data from the EPA 
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
#' @return Dataframe of EPA data.
#' @examples
#' \dontrun{
#' df <- epa_downloadData(parameterName="PM2.5", parameterCode="88101", year=2016)
#' }


# if (false){
#   parameterName="PM2.5"
#   parameterCode=88101
#   year=2016
#   baseUrl='http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/'
# }

epa_downloadData <- function(parameterName=NULL, parameterCode=NULL, year=NULL,
                                    baseUrl='https://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/') {

  # Sanity Check -- validate parameter name
  validParameters <- c("Ozone", "SO2", "CO", "NO2", "PM2.5", "PM10", "Wind", "Temperature", "Barometric_Pressure",
                       "RH_and_Dewpoint", "HAPs", "VOCs", "NONOxNOy")
  if ( !is.null(parameterName) ) {
    if ( !parameterName %in% validParameters ) {
      logger.error("parameter '%s' is not in: %s", parameterName, paste0(validParameters, collapse=", "))
      stop(paste0("parameter '",parameterName,"' is not in: ", paste0(validParameters, collapse=", ")))
    }
  }
  
  # Sanity Check -- validate parameter code
  validParameterCodes <- c("44201", "42401", "42101", "42602", "88101", "88502", "81102", "SPEC",
                           "WIND", "TEMP", "PRESS", "RH_DP", "HAPS", "VOCS", "NONOxNOy")
  if ( !is.null(parameterCode) ) {
    if ( !parameterCode %in% validParameterCodes ) {
      logger.error("parameter code '%s' is not in: %s", parameterCode, paste0(validParameterCodes, collapse=", "))
      stop(paste0("parameter code '",parameterCode,"' is not in: ", paste0(validParameterCodes, collapse=", ")))
    }
  }
  
  # Sanity check: year is supplied and valid
  if(is.null(year)){
    logger.error("Reuiqred parameter 'year' is missing")
    stop(paste0("Reuiqred parameter 'year' is missing"))
  } else if (! (year >= 1990 && year <= 2016)) {
    logger.error("Valid year is from 1990 to 2016")  ## NOTE: might need to change
    stop(paste0("Valid year is from 1990 to 2016"))
  } else if ( (parameterCode=="88101" && year<2008) || (parameterCode=="88502" && year<1998) || 
              (parameterCode=="SPEC" && year<2001) || (parameterCode=="HAPS" && year<1993) ) {
    logger.error("Data for parameter code %s in year %i are missing", parameterCode, year)  
    stop(sprintf("Data for parameter code %s in year %i are missing", parameterCode, year))
  }
  
  # Sanity check
  if ( requireNamespace('MazamaSpatialUtils', quietly=TRUE) ) {
    dummy <- getSpatialDataDir()
  }
  
  if ( requireNamespace('PWFSLSmoke', quietly=TRUE) ) {
    dummy <- getSmokeDataDir()
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
  
  # Cleanup
  file.remove(zipFile, csvFile)
  
  logger.info('Downloaded %d rows of OpenAQ data', nrow(df))
  
  return(df)
}
  