#' @keywords AirNow
#' @export
#' @importFrom utils read.table
#' @importFrom rlang .data
#'
#' @title Download hourly data from AirNow
#'
#' @param datestamp Integer or character representing YYYYMMDDHH.
#' @param baseUrl Base URL for archived hourly data.
#' @return Dataframe of AirNow hourly data.
#'
#' @description The \url{https://airnowtech.org} site provides both air
#' pollution monitoring data as well as monitoring site location metadata. This
#' function retrieves a single, hourly data file and returns it as a dataframe
#' which includes a monitor's site name and parameters monitored.
#'
#' @note As of 2016-12-27, it appears that hourly data are available only for
#' 2016 and not for earlier years.
#'
#' @note Data from locations whose timezones have a fractional offset from UTC
#' are removed as the PWFSLSmoke data model only supports data reported on hour
#' boundaries. As of 2019-06-26, this only applies to US Department of State
#' monitors in Myanmar, Sri Lanka, India and Nepal.
#'
#' @seealso \link{airnow_createDataDataframes}
#' @seealso \link{airnow_downloadParseData}
#'
#' @examples
#' \dontrun{
#' df <- airnow_downloadHourlyData(2018070112)
#' }

airnow_downloadHourlyData <- function(
  datestamp = strftime(lubridate::now(tzone = "UTC"), "%Y%m%d00", tz = "UTC"),
  baseUrl = 'https://files.airnowtech.org/airnow'
) {

  # Strip off any final '/'
  baseUrl <- stringr::str_replace(baseUrl,'/$','')

  # Create URL
  datestamp <- as.character(datestamp)
  year <- stringr::str_sub(datestamp,1,4)
  ymd <- stringr::str_sub(datestamp,1,8)
  url <- paste0(baseUrl,'/',year,'/',ymd,'/HourlyData_',datestamp,'.dat')

  col_names <- c('ValidDate', 'ValidTime', 'AQSID', 'SiteName', 'GMTOffset',
                 'ParameterName', 'ReportingUnits', 'Value', 'DataSource')

  col_types <- 'ccccdccdc'

  # NOTE:  Even after the move to Amazon web services, they still have some
  # NOTE:  enoding issues, e.g.:
  # NOTE:
  # NOTE:  12/26/16|00:00|000051501|Z<82>phirin|-5|OZONE|PPB|39|Meteorological Service of Canada
  locale <- readr::locale(encoding="CP437")

  # Read in text as a dataframe
  tbl <-
    readr::read_delim(url,
                      delim = '|',
                      col_names = col_names,
                      col_types = col_types,
                      locale=locale) %>%
    # Remove records with fractional hours (see @note above)
    dplyr::filter(stringr::str_detect(.data$ValidTime, ":00"))

  return(tbl)

}
