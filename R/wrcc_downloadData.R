#' @keywords WRCC
#' @export
#' @import MazamaCoreUtils
#'
#' @title Download WRCC data
#'
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param unitID station identifier (will be upcased)
#' @param baseUrl base URL for data queries
#' @description Request data from a particular station for the desired time period.
#' Data are returned as a single character string containing the WRCC output.
#'
#' Monitor unitIDs can be found at https://wrcc.dri.edu/cgi-bin/smoke.pl.
#' @return String containing WRCC output.
#' @references \href{https://wrcc.dri.edu/cgi-bin/smoke.pl}{Fire Cache Smoke Monitoring Archive}
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' fileString <- wrcc_downloadData(20150701, 20150930, unitID = 'SM16')
#' df <- wrcc_parseData(fileString)
#'
#' }, silent = FALSE)
#' }

wrcc_downloadData <- function(
  startdate = strftime(lubridate::now(tzone = "UTC"),"%Y010101",tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"),"%Y%m%d23",tz = "UTC"),
  unitID = NULL,
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl"
) {

  logger.debug(" ----- wrcc_downloadData() ----- ")

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(unitID) ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }

  # Get UTC times
  starttime <- MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC")
  endtime <- MazamaCoreUtils::parseDatetime(enddate, timezone = "UTC")

  # Create CGI parameters
  .params <- list(stn = toupper(unitID),
                  smon = strftime(starttime,"%m",tz = "UTC"),
                  sday = strftime(starttime,"%d",tz = "UTC"),
                  syea = strftime(starttime,"%y",tz = "UTC"),
                  emon = strftime(endtime,"%m",tz = "UTC"),
                  eday = strftime(endtime,"%d",tz = "UTC"),
                  eyea = strftime(endtime,"%y",tz = "UTC"),
                  'Submit Info' = 'Submit Info',
                  dfor = '04',
                  src = 'W',
                  miss = '08',
                  flag = 'N',
                  Dfmt = '01',
                  Tfmt = '01',
                  Head = '01',
                  Deli = '01',
                  unit = 'M',
                  WsMon = '01',
                  WsDay = '01',
                  WeMon = '12',
                  WeDay = '12',
                  WsHou = '00',
                  WeHou = '24',
                  .cgifields = c('unit','flag','srce'))

  # ----- Download data --------------------------------------------------------

  logger.trace("Downloading WRCC data for unitID %s", unitID)

  suppressWarnings({
    r <- httr::POST(baseUrl, body = .params)
  })

  if ( httr::http_error(r) ) {
    logger.error("WRCC data service failed for unitID: %s", unitID)
    logger.error("WRCC data service failed with: %s", httr::content(r))
    return("")
  }

  fileString <- httr::content(r, 'text', encoding = 'UTF-8')

  return(fileString)

}
