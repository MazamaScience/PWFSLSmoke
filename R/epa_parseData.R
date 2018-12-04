#' @keywords EPA
#' @export
#' @title Parse Data from EPA
#' @param zipFile absolute path to monitoring data .zip file
#' @description This function uncompress previously downloaded air quality .zip files from the EPA and
#' reads it into a tibble.
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
#' \item{Temperature}
#' \item{Barometric_Pressure}
#' \item{RH_and_Dewpoint}
#' \item{HAPs}
#' \item{VOCs}
#' \item{NONOxNOy}
#' }
#'
#' Associated parameter codes include:
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
#' CSV files are removed after data are read into a dataframe.
#' @return Tibble of EPA data.
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_format_3}{file format description}
#' @examples
#' \dontrun{
#' zipFile <- epa_downloadData(2016, "88101", '~/Data/EPA')
#' tbl <- epa_parseData(zipFile, "PM2.5")
#' }


# if (false){
#   parameterName="PM2.5"
#   parameterCode=88101
#   year=2016
#   baseUrl='http://aqs.epa.gov/aqsweb/airdata/'
# }

epa_parseData <- function(zipFile=NULL) {

  # Sanity checks
  if ( is.null(zipFile) ) {
    logger.error("Required parameter 'zipFile' is missing")
    stop(paste0("Required parameter 'zipFile' is missing"))
  }

  csvFile <- stringr::str_replace(zipFile,"\\.zip","\\.csv")

  # Uncompress
  logger.debug(paste0('Uncompressing ',zipFile,' ...'))
  utils::unzip(zipFile, exdir=dirname(zipFile))
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
  file.remove(csvFile)

  logger.debug('Downloaded and parsed %d rows of EPA data', nrow(tbl))

  return(tbl)
}
