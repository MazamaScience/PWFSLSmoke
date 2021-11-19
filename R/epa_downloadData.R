#' @keywords EPA
#' @export
#' @import MazamaCoreUtils
#'
#' @title Download EPA air quality data
#'
#' @param year year
#' @param parameterCode pollutant code
#' @param downloadDir directoroy where monitoring data .zip file will be saved
#' @param baseUrl base URL for archived daily data
#' @description This function downloads air quality data from the EPA and
#' saves it to a directory.
#'
#' Available parameter codes include:
#' \enumerate{
#' \item{44201}{ -- Ozone}
#' \item{42401}{ -- SO2}
#' \item{42101}{ -- CO}
#' \item{42602}{ -- NO2}
#' \item{88101}{ -- PM2.5}
#' \item{88502}{ -- PM2.5}
#' \item{81102}{ -- PM10}
#' \item{SPEC}{ -- PM2.5}
#' \item{WIND}{ -- Wind}
#' \item{TEMP}{ -- Temperature}
#' \item{PRESS}{ -- Barometric Pressure}
#' \item{RH_DP}{ -- RH and dewpoint}
#' \item{HAPS}{ -- HAPs}
#' \item{VOCS}{ -- VOCs}
#' \item{NONOxNOy}
#' }
#'
#' @note Unzipped CSV files are almost 100X larger than the compressed .zip files.
#' @return Filepath of the downloaded zip file.
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' zipFile <- epa_downloadData(2016, "88101", '~/Data/EPA')
#' tbl <- epa_parseData(zipFile, "PM2.5")
#'
#' }, silent = FALSE)
#' }

epa_downloadData <- function(
  year = NULL,
  parameterCode = "88101",
  downloadDir = tempdir(),
  baseUrl = 'https://aqs.epa.gov/aqsweb/airdata/'
) {

  logger.debug(" ----- epa_downloadData() ----- ")

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
  zipFile <- path.expand( paste0(downloadDir,'/',fileBase,".zip") )

  # TODO:  Change to use httr and test for success

  logger.trace(paste0('Downloading ',fileBase,'.zip ...'))
  utils::download.file(url, zipFile, quiet=TRUE)
  logger.trace(paste0('Finished downloading.'))

  return(zipFile)

}
