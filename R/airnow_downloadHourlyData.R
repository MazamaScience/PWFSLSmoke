#' @keywords internal AirNow
#' @export
#' @importFrom utils read.table
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
#' @seealso \link{airnow_createDataDataframes}
#' @seealso \link{airnow_downloadData}
#' @examples
#' \dontrun{
#' df <- airnow_downloadHourlyData(USER, PASS, 2015070112)
#' }

airnow_downloadHourlyData <- function(user='', pass='', datestamp='', tries=6, verbose=FALSE,
                                      baseUrl='ftp.airnowapi.org/HourlyData/Archive',
                                      curl=NULL) {
  
  datestamp <- as.character(datestamp)  

  # Create URLs
  if ( stringr::str_detect(baseUrl,'Archive') ) {
    # If the baseUrl contains 'Archive' we need to include a YEAR subdirectory
    yearstamp <- stringr::str_sub(datestamp,1,4)
    url <- sprintf('%s/%s/%s.dat', baseUrl, yearstamp, datestamp)
  } else {
    # otherwise just find it in the HourlyData archive
    url <- sprintf('%s/%s.dat', baseUrl, datestamp)
  }
  # Clean up any '//' in case the incoming baseUrl ended with '/'
  stringr::str_replace_all(url,'//','')
  ftp_url <- paste0('ftp://',user,':',pass,'@',url)
  
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
  
  logger.debug('Downloading data from %s', url)
  
  # NOTE:  The monitoring_site_locations.dat file has an encoding of CP437 "Non-ISO extended-ASCII".
  # NOTE:  In order to propery convert to UTF-8 we must first lie to getURL() and say that it is UTF-8.
  # NOTE:  Then we use iconv to really convert it.
  # NOTE:  
  # NOTE:  Presumably, the hourly data files have the same encoding and require the same treatment.
  
  result <- try( fileText <- iconv( retryURL(ftp_url, curl=curl, .encoding="UTF-8", tries=tries),
                                    from="CP437", to="UTF-8") )
  
  # Print out the error message if there is an error
  if ( class(result)[1] == "try-error" ) {
    
    err_msg <- paste('ERROR getting: ',url,'\n',geterrmessage())
    
    # NOTE:  If we fail with only "Timeout" errors, create a fake fileText with a single record of all missing
    # NOTE:  readr::read_delim() requires at least one newline for fileText to be interpreted as literal data.
    
    if (stringr::str_detect(err_msg,'retryURL() failed after')) {
      logger.warn("Unable to download %s after %d tries", url, tries)
      fileText <- '||||||||\n'
    } else if (stringr::str_detect(err_msg,'Connection refused')) {
      logger.fatal("Connection refused from AirNow for ", url)
      stop("Connection refused from AirNow.  Too many requests in a short period of time might look like a DOS attack.")
    } else {
      logger.debug(err_msg)
      logger.warn("Unable to download %s after %d tries", url, tries)
      fileText <- '||||||||\n'
    }
    
  }
  
  # Read in text as a dataframe
  col_names <- c('ValidDate', 'ValidTime', 'AQSID', 'SiteName', 'GMTOffset',
                 'ParameterName', 'ReportingUnits', 'Value', 'DataSource')
  df <- readr::read_delim(fileText, delim='|', col_names=col_names, col_types='cccciccdc')
  
  return(df) 
}
