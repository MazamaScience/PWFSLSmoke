#' @keywords OpenAQ
#' @export
#' @title Download and Aggregate Multiple Daily Data Files from OpenAQ
#' @param parameter pollutant name
#' @param startdate desired staring date (integer or character representing YYYYMMDD)
#' @param days desired number of days of data to assemble
#' @param countryCode two-character country code (ISO 3166-1 alpha-2)
#' @param baseUrl base URL for archived daily data
#' @description This function downloads data from the OpenAQ Data Backups page
#' hosted on Amazon S3. All data obtained are then combined into a single dataframe
#' and returned.
#' 
#' Available parameters are limited to:
#' PM2.5, PM10, ozone, sulfur dioxide, nitrogen dioxide, carbon monoxide, and black carbon
#' \enumerate{
#' \item{pm25}{ -- PM2.5}
#' \item{pm10}{ -- PM10}
#' \item{o3}{ -- ozone }
#' \item{so2}{ -- sulfer dioxide }
#' \item{no2}{ -- nitrogen dioxide}
#' \item{co}{ -- carbon monoxide}
#' \item{bc}{ -- black carbon}
#' }
#' 
#' @return Dataframe of aggregated OpenAQ data.
#' @examples
#' \dontrun{
#' df <- openaq_downloadData(20160901, days=7)
#' }

openaq_downloadData <- function(parameter='pm25',
                                startdate='', days=1, countryCode='US',
                                baseUrl="http://openaq-data.s3.amazonaws.com/") {
  
  # OpenAQ Standard headers
  col_names <- c("location", "city", "country", "utc", "local", "parameter", "value", "unit", "latitude", "longitude", "attribution")
  col_types <- 'ccccccdcddc'
  
  # Format the startdate integer using lubridate
  startdate <- parseDatetime(startdate)

  # Pre-allocate an empty list of the appropriate length (basic R performance idiom)
  dfList <- vector(mode="list", length=days)
  
  # Loop through days and store each datafame in the list
  for (i in 1:days) {
    datetime <- startdate + lubridate::days(i-1)
    datestamp <- strftime(datetime,'%Y-%m-%d', tz='GMT')
    # TODO: logger!
    logger.debug("Working on datastamp %s", datestamp)
    url <- paste0(baseUrl,datestamp,'.csv')
    df <- suppressMessages( readr::read_csv(url, skip=1, col_names=col_names, col_types=col_types, progress=FALSE) )
    # Subset dataframe here so we don't grow too big if someone asks for many days
    if ( !is.null(countryCode) ) df <- df[df$country == countryCode,]
    if ( !is.null(parameter) ) df <- df[df$parameter == parameter,]
    dfList[[i]] <- df
  }
  # TODO: logger!
  
  # Combine all dataframes
  df <- dplyr::bind_rows(dfList)
  
  # Only return distinct rows (no duplicates)
  return(dplyr::distinct(df))
  
}