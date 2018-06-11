#' @keywords PurpleAir
#' @export
#' @title Download Data from PurpleAir
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param provider identifier used to modify baseURL \code{['APCD'|'USFS']}
#' @param unitID unit identifier
#' @param baseUrl base URL for data queries
#' @description Request data from a particular station for the desired time period.
#' Data are returned as a single character string containing the AIRIS output.
#' @return String containing AIRSIS output.
#' @references \href{http://usfs.airsis.com}{Interagency Real Time Smoke Monitoring}
#' @examples
#' \dontrun{
#' fileString <- airsis_downloadData( 20150701, 20151231, provider='USFS', unitID='1026')
#' df <- airsis_parseData(fileString)
#' }
 

if ( FALSE ) {
  
  startdate = strftime(lubridate::now(),"%Y-%m-01 00:00:00",tz="UTC")
  enddate = strftime(lubridate::now(),"%Y-%m-%d %H:%M:%S",tz="UTC")
  apikey = "49V50OO5MTDR7UF6"
  offset = 0
  average = NULL
  round = 2
  baseUrl = "https://thingspeak.com/channels/195168/feed.csv?"  
  
}

purpleair_downloadData <- function(startdate = strftime(lubridate::now(),"%Y-%m-%d 00:00:00",tz="UTC"),
                                   enddate = strftime(lubridate::now(),"%Y-%m-%d %H:%M:%S",tz="UTC"),
                                   apikey = NULL,
                                   offset = 0,
                                   average = NULL,
                                   round = 2,
                                   baseUrl = "https://thingspeak.com/channels/195168/feed.csv?") {
  
  # Sanity check
  if ( is.null(apikey) ) {
    logger.error("Required parameter 'apikey' is missing")
    stop(paste0("Required parameter 'apikey' is missing"))
  }
  
  # Get UTC times
  starttime <- lubridate::ymd_hms(startdate)
  endtime <- lubridate::ymd_hms(enddate)
  
  # Example URL:
  #   https://thingspeak.com/channels/195168/feed.csv?api_key=49V50OO5MTDR7UF6&offset=0&average=&round=2&start=2018-06-01%2000:00:00&end=2018-06-02%2000:00:00
  
  # Create URL
  url <- paste0(baseUrl,
                '&apikey=', apikey,
                '&offset=', offset,
                '&average=', average,
                '&round=', round,
                '&start=', strftime(starttime, "%Y-%m-%d %H:%M:%S", tz="UTC"),
                '&end=', strftime(endtime, "%Y-%m-%d %H:%M:%S", tz="UTC"))
  
  logger.debug("Downloading data from: %s", url)
  
  # Read the url output into a string
  fileString <- readr::read_file(URLencode(url))
  
  # NOTE:  Data downloaded directly from ThingSpeak is well formatted:
  # NOTE:    single header line, unicode
  # NOTE:
  # NOTE:  No further processing is needed.
  
  return(fileString)
  
}
