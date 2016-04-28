#' @keywords internal AirNow
#' @export
#' @title Download Hourly Data from AirNow
#' @param user username
#' @param pass password
#' @param datestamp integer or character representing YYYYMMDDHH
#' @param tries number of download attempts in the face of timeouts
#' @param verbose logical requesting verbose output from libcurl
#' @param baseUrl base URL for archived hourly data
#' @param curl previously initialized CURL context/handle (see RCurl::getURL())
#' @description With the given username and password, airnow_downloadHourlyData() uses the \code{datesteamp}
#' argument to create a URL into the Airnow archive at \url{'ftp.airnowapi.org/HourlyData/Archive}.
#' The associated ASCII hourly data file is downloaded and converted into a dataframe.
#' @return Data frame of the AirNow hourly data.
# @seealso \link{airnow_aggregateHourlyData}
# @seealso \link{airnow_getMonthlyData}
#' @examples
#' \dontrun{
#' df <- airnow_downloadHourlyData(USER, PASS, 2015070112)
#' }

airnow_downloadHourlyData <- function(user='', pass='', datestamp='', tries=6, verbose=FALSE,
                                      baseUrl='ftp.airnowapi.org/HourlyData/Archive/',
                                      curl=NULL) {
  
  # Create URLs
  datestamp <- as.character(datestamp)  
  yearstamp <- stringr::str_sub(datestamp,1,4)
  url=sprintf(baseUrl,'%s/%s.dat', yearstamp, datestamp)
  ftp_url <- paste0('ftp://',user,':',pass,'@',url)
  
  # Define the column names and their classes that will be used for the converted data 
  colNames <- c('ValidDate', 'ValidTime', 'AQSID', 'SiteName', 'GMTOffset', 'ParameterName', 'ReportingUnits',
                'Value', 'DataSource')
  
  colClasses <- c('character','character','character','character','numeric','character','character',
                  'numeric','character')
  
  # NOTE:  Typically, this function is called repeatedly within a loop with the curl handle
  # NOTE:  created outside the loop and reused.  We only create one here if none was passed in.
  
  if ( is.null(curl) ) {
    # Create a curl handle with appriprate options
    # NOTE:  Specific set of curl options to avoid "421 Maximum login connections"
    .opts <- list(maxconnects=2,
                  ftp.use.epsv=FALSE,
                  verbose=verbose)
    curl=RCurl::getCurlHandle(.opts=.opts)
  }
    
  # NOTE:  The monitoring_site_locations.dat file has an encoding of CP437 "Non-ISO extended-ASCII".
  # NOTE:  In order to propery convert to UTF-8 we must first lie to getURL() and say that it is UTF-8.
  # NOTE:  Then we use iconv to really convert it.
  # NOTE:  
  # NOTE:  Presumably, the hourly data files have the same encoding and require the same treatment.
  
  result <- try( fileText <- iconv( retryURL(ftp_url, curl=curl, .encoding="UTF-8", tries=tries),
                                    from="CP437", to="UTF-8") )
  
  # Print out the error message if there is an error
  if ( class(result) == "try-error" ) {
    
    err_msg <- paste('ERROR getting: ',ftp_url,'\n',geterrmessage())
    
    # NOTE:  If we fail with only "Timeout" errors, create a fake fileText with a single record of all missing
    if (stringr::str_detect(err_msg,'retryURL() failed after')) {
      warning('Unable to download ',ftp_url,'\tafter ',tries,' tries.')
      fileText <- '||||||||'
    } else {
      stop(err_msg)
    }
    
  }
  
  # Read in text as a dataframe, utilizing various arguments to handle the format.
  df <- read.table(textConnection(fileText),
                   col.names=colNames, colClasses=colClasses,
                   sep='|', quote="", comment.char="", na.strings="N/A", strip.white=TRUE)
  
  
  return(df) 
}
