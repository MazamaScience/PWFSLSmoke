#' @keywords AirNow
#' @export
#' @title Download AirNow Site Location Metadata
#' @param baseUrl location of the AirNow monitoring site locations file
#' @param file name of the AirNow monitoring site locations file
#' @description The \url{https://airnowtech.org} site provides both air pollution
#' monitoring data as well as monitoring site location metadata. This 
#' function retrieves the most recent version of the site location metadata file and returns it as a dataframe.
#' 
#' A description of the data format is publicly available at the
#' \href{https://www.airnowapi.org/docs/MonitoringSiteFactSheet.pdf}{Monitoring Site Fact Sheet}.
#' @note As of December, 2016, the \code{monitoring_site_locations.dat} file has an encoding of 
#' "CP437" (aka "Non-ISO extended-ASCII" or "IBMPC 437") and will be converted to "UTF-8"
#' so that French and Spanish language place names are properly encoded in the returned dataframe.
#' @return Tibble of site location metadata.
#' @seealso \link{airnow_createMetaDataframes}
#' @examples
#' \dontrun{
#' sites <- airnow_downloadSites()
#' }

airnow_downloadSites <- function(baseUrl='https://files.airnowtech.org/airnow/today/',
                                 file='monitoring_site_locations.dat') {
  
  url <- paste0(baseUrl, file)
  
  # NOTE:  Information on the strucutre of this file come from the Monitoring Site Factsheet.
  # NOTE:    https://www.airnowapi.org/docs/MonitoringSiteFactSheet.pdf
  
  col_names <- c('AQSID','parameterName','siteCode','siteName','status','agencyID','agencyName',
                 'EPARegion','latitude','longitude','elevation','GMTOffsetHours','countryCode',
                 'FIPSCMSACode','CMSAName','FIPSMSACode','MSAName','FIPSStateCode','stateCode',
                 'GNISCountyCode','countyName','GNISCityCode','cityName')
  
  col_types <- c('ccccccccdddcccccccccccc')
  
  # NOTE:  Even after the move to Amazon web services, they still have some enoding issues
  # NOTE:  e.g. 800090033         PM2.5     0033                             Coyoac\xa0n
  locale <- readr::locale(encoding="CP437")
  
  # Read in text as a dataframe
  tbl <- readr::read_delim(url, delim='|', col_names=col_names, col_types=col_types, locale=locale)
  
  return(tbl)
}

