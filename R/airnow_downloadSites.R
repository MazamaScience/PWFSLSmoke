#' @keywords AirNow
#' @export
#' @title Download AirNow Site Location Metadata
#' @param user user name
#' @param pass password
#' @param tries number of download attempts in the face of timeouts
#' @param verbose logical flag to generate verbose output
#' @param url location of the monitoring_site_locations.data file
#' @description The \url{http://airnowtech.org} site provides both air pollution
#' monitoring data as well as monitoring site location metadata. The airnow_downloadSites()
#' function retrieves the most recent version of the site location metadata file and returns it as a dataframe.
#' The \code{monitoring_site_locations.dat} file is only available to registered users so the \code{user}
#' and \code{pass} arguments are required.
#' 
#' A description of the data format is publicly available at the
#' \href{ttp://www.airnowapi.org/docs/MonitoringSiteFactSheet.pdf}{Monitoring Site Fact Sheet}.
#' @note As of January, 2016, the \code{monitoring_site_locations.dat} file has an encoding of 
#' "CP437" (aka "Non-ISO extended-ASCII" or "IBMPC 437") and will be converted to "UTF-8"
#' so that French and Spanish language place names are properly encoded in the returned dataframe.
#' @return Dataframe of site location metadata.
#' @seealso \link{airnow_createMetaDataframe}
#' @examples
#' \dontrun{
#' sites <- airnow_downloadSites('USER','PASS')
#' meta <- airnow_createMetaDataframe(sites, parameter='PM2.5')
#' }

airnow_downloadSites <- function(user='', pass='', tries=6, verbose=FALSE,
                                 url='ftp.airnowapi.org/Locations/monitoring_site_locations.dat') {

  # Location of monitoring site metadata file at AirNow
  ftp_url <- paste0('ftp://',user,':',pass,'@',url)
  
  if (verbose) cat(paste0("Downloading site location metadata from:\n\t",ftp_url,"\n\n"))

  # NOTE:  Information on the strucutre of this file come from the Monitoring Site Factsheet.
  # NOTE:    http://www.airnowapi.org/docs/MonitoringSiteFactSheet.pdf
  
  colNames <- c('AQSID','parameterName','siteCode','siteName','status','agencyID','agencyName',
                'EPARegion','latitude','longitude','elevation','GMTOffsetHours','countryCode',
                'FIPSCMSACode','CMSAName','FIPSMSACode','MSAName','FIPSStateCode','stateCode',
                'GNISCountyCode','countyName','GNISCityCode','cityName')
  
  colClasses <- c('character','factor','character','character','factor','factor','factor',
                  'factor','numeric','numeric','numeric','numeric','factor',
                  'factor','factor','factor','factor','factor','factor',
                  'factor','factor','factor','factor')
  
  # Create a curl handle with appropriate options
  .opts <- list(ftp.use.epsv=FALSE,
                verbose=verbose)
  curl <- RCurl::getCurlHandle(.opts=.opts)
  
  # NOTE:  The monitoring_site_locations.dat file has an encoding of CP437 "Non-ISO extended-ASCII".
  # NOTE:  In order to propery convert to UTF-8 we must first lie to getURL() and say that it is UTF-8.
  # NOTE:  Then we use iconv to really convert it.
  
  result <- try( fileText <- iconv( retryURL(ftp_url, curl=curl, .encoding="UTF-8", tries=tries),
                                    from="CP437", to="UTF-8") )
  
  if ( class(result) == "try-error" ) {
    err_msg <- paste('ERROR getting: ',ftp_url,'\n',geterrmessage())
    stop(err_msg)
  }
  
  # Read in text as a dataframe, utilizing various arguments to handle the format.
  df <- read.table(textConnection(fileText),
                   col.names=colNames, colClasses=colClasses,
                   sep='|', quote="", comment.char="", na.strings="N/A", strip.white=TRUE)
  
  return(df)
}

