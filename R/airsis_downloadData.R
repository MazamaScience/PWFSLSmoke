#' @keywords AIRSIS
#' @export
#' @import MazamaCoreUtils
#'
#' @title Download AIRSIS data
#'
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param provider identifier used to modify baseURL \code{['APCD'|'USFS']}
#' @param unitID unit identifier
#' @param baseUrl base URL for data queries
#'
#' @description Request data from a particular station for the desired time period.
#' Data are returned as a single character string containing the AIRIS output.
#' @return String containing AIRSIS output.
#' @references \href{http://usfs.airsis.com}{Interagency Real Time Smoke Monitoring}
#' @examples
#' \dontrun{
#' fileString <- airsis_downloadData( 20150701, 20151231, provider='USFS', unitID='1026')
#' df <- airsis_parseData(fileString)
#' }

airsis_downloadData <- function(
  startdate = strftime(lubridate::now(tzone = "UTC"), "%Y0101", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"), "%Y%m%d", tz = "UTC"),
  provider = 'USFS',
  unitID = NULL,
  baseUrl = "http://xxxx.airsis.com/vision/common/CSVExport.aspx?"
) {

  logger.debug(" ----- airsis_downloadData() ----- ")

  # Sanity check
  if ( is.null(unitID) ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }

  # Get UTC times
  starttime <- MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC")
  endtime <- MazamaCoreUtils::parseDatetime(enddate, timezone = "UTC")

  # Example URL:
  #   http://usfs.airsis.com/vision/common/CSVExport.aspx?uid=1026&StartDate=2016-02-03&EndDate=2016-02-03

  # Create a valid baseUrl
  baseUrl <- stringr::str_replace(baseUrl, 'xxxx', provider)

  # Create URL
  url <- paste0(baseUrl, 'uid=', unitID,
                '&StartDate=', strftime(starttime, "%Y-%m-%d", tz="UTC"),
                '&EndDate=', strftime(endtime, "%Y-%m-%d", tz="UTC"))

  logger.trace("Downloading data from: %s", url)

  # Read the url output into a string
  fileString <- readr::read_file(url)

  # NOTE:  Data downloaded directly from AIRSIS is well formatted:
  # NOTE:    single header line, unicode
  # NOTE:
  # NOTE:  No further processing is needed.

  return(fileString)

}
