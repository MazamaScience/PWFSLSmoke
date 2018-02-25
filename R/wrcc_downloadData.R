#' @keywords WRCC
#' @export
#' @title Download Data from WRCC
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param unitID station identifier (will be upcased)
#' @param baseUrl base URL for data queries
#' @description Request data from a particular station for the desired time period.
#' Data are returned as a single character string containing the WRCC output. 
#' 
#' Monitor unitIDs can be found at http://www.wrcc.dri.edu/cgi-bin/smoke.pl.
#' @return String containing WRCC output.
#' @references \href{http://www.wrcc.dri.edu/cgi-bin/smoke.pl}{Fire Cache Smoke Monitoring Archive}
#' @examples
#' \dontrun{
#' fileString <- wrcc_downloadData(20150701, 20150930, unitID='SM16')
#' df <- wrcc_parseData(fileString)
#' }

# Monitor IDs include:
# # http://www.wrcc.dri.edu/cgi-bin/smoke.pl
# CacheMonitors <- c('SM11','SM13','SM15','SM16','SM16','SM17','SM19',
#                    'SM20','SM21','SM22','SM23','SM24',
#                    'S265','SM66','SM67','SM68','SM69',
#                    'S284','S215','S216','S217',
#                    'E231','E840','E866','E925')
# USFSRegionalMonitors <- c()
# MiscellaneousMonitors <- c()

wrcc_downloadData <- function(startdate=strftime(lubridate::now(),"%Y010101",tz="UTC"),
                              enddate=strftime(lubridate::now(),"%Y%m%d23",tz="UTC"),
                              unitID=NULL, 
                              baseUrl="https://wrcc.dri.edu/cgi-bin/wea_list2.pl") {
  
  # Sanity check
  if ( is.null(unitID) ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }
  
  # Get UTC times
  starttime <- parseDatetime(startdate)
  endtime <- parseDatetime(enddate)
  
  # Create CGI parameters
  .params <- list(stn=toupper(unitID),
                  smon=strftime(starttime,"%m",tz="UTC"),
                  sday=strftime(starttime,"%d",tz="UTC"),
                  syea=strftime(starttime,"%y",tz="UTC"),
                  emon=strftime(endtime,"%m",tz="UTC"),
                  eday=strftime(endtime,"%d",tz="UTC"),
                  eyea=strftime(endtime,"%y",tz="UTC"),
                  'Submit Info'='Submit Info',
                  dfor='04',
                  src='W',
                  miss='08',
                  flag='N',
                  Dfmt='01',
                  Tfmt='01',
                  Head='01',
                  Deli='01',
                  unit='M',
                  WsMon='01',
                  WsDay='01',
                  WeMon='12',
                  WeDay='12',
                  WsHou='00',
                  WeHou='24',
                  .cgifields=c('unit','flag','srce'))
  
  logger.debug("Downloading WRCC data for unitID %s", unitID)
  
  suppressWarnings({
    r <- httr::POST(baseUrl, body=.params)
  })

  if ( httr::http_error(r) ) {
    logger.error("WRCC data service failed for unitID: %s", unitID)
    logger.error("WRCC data service failed with: %s", httr::content(r))
    return("")
  }
  
  fileString <- httr::content(r, 'text', encoding='UTF-8')

  # NOTE:  Data downloaded directly from WRCC is well formatted:
  # NOTE:    single header line, unicode
  # NOTE:
  # NOTE:  No further processing is needed.
  
  return(fileString)
  
}
