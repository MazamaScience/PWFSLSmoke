#' @keywords AirNow
#' @export
#' @importFrom utils read.table
#' @title Download Hourly Data from AirNow
#' @param datestamp integer or character representing YYYYMMDDHH
#' @param baseUrl base URL for archived hourly data
#' @return Dataframe of AirNow hourly data.
#' @description The \url{http://airnowtech.org} site provides both air pollution
#' monitoring data as well as monitoring site location metadata. The \code{airnow_downloadHourlyData()}
#' function retrieves a single, hourly data file and returns it as a dataframe.
#' 
#' @note:  As of 2016-12-27, it appears that hourly data are available only for 2016 and
#' not for earlier years.
#' @seealso \link{airnow_createDataDataframes}
#' @seealso \link{airnow_downloadData}
#' @examples
#' \dontrun{
#' df <- airnow_downloadHourlyData(2016070112)
#' }

airnow_downloadHourlyData <- function(datestamp='',
                                      baseUrl='https://files.airnowtech.org/airnow') {
  
  # Strip off any final '/'
  baseUrl <- stringr::str_replace(baseUrl,'/$','')

  # Create URL
  datestamp <- as.character(datestamp)
  year <- stringr::str_sub(datestamp,1,4)
  ymd <- stringr::str_sub(datestamp,1,8)
  url <- paste0(baseUrl,'/',year,'/',ymd,'/HourlyData_',datestamp,'.dat')
  
  col_names <- c('ValidDate', 'ValidTime', 'AQSID', 'SiteName', 'GMTOffset',
                 'ParameterName', 'ReportingUnits', 'Value', 'DataSource')
  
  col_types = 'cccciccdc'
  
  # NOTE:  Even after the move to Amazon web services, they still have some enoding issues
  # NOTE:  e.g. 12/26/16|00:00|000051501|Z<82>phirin|-5|OZONE|PPB|39|Meteorological Service of Canada
  locale <- readr::locale(encoding="CP437")
  
  # Read in text as a dataframe
  df <- readr::read_delim(url, delim='|', col_names=col_names, col_types=col_types, locale=locale)
  
  return(df) 
}
