#' @keywords OpenAQ
#' @export
#' @title Download and Aggregate Multiple Daily Data Files from OpenAQ
#' @param parameter pollutant name
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param days desired number of days of data to assemble
#' @param countryCode two-character country code (ISO 3166-1 alpha-2)
#' @param baseUrl base URL for data queries
#' @description This function downloads data from the OpenAQ Data Backups page
#' hosted on Amazon S3. All data obtained are then combined into a single dataframe
#' and returned.
#' 
#' Available parameters include:
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
#' df <- openaq_downloadData(startdate=20160901, days=2)
#' }

openaq_downloadData <- function(parameter=NULL,
                                startdate='', days=1, countryCode=NULL,
                                baseUrl="http://openaq-data.s3.amazonaws.com/") {
  
  # Sanity Check -- validate parameter
  validParameters <- c('pm25','pm10','o3','so2','no2','co','bc')
  if ( !is.null(parameter) ) {
    if ( !parameter %in% validParameters ) {
      logger.error("Requested parameter '%s' is not in: %s", parameter, paste0(validParameters, collapse=", "))
      stop(paste0("Requested parameter '",parameter,"' is not in: ", paste0(validParameters, collapse=", ")))
    }
  }
  
  # TODO:  validate countrycode -- this requires MazamaSpatialUtils to provide a vector of countryCodes
  
  # OpenAQ Standard headers
  col_names <- c("location", "city", "country", "utc", "local", "parameter", "value", "unit", "latitude", "longitude", "attribution")
  col_types <- 'ccccccdcddc'
  
  # Format the startdate integer using lubridate
  startdate <- parseDatetime(startdate)

  # Pre-allocate an empty list of the appropriate length (basic R performance idiom)
  dfList <- vector(mode="list", length=days)
  
  logger.info("Downloading %d daily data files from OpenAQ ...",days)
  
  # Loop through days and store each datafame in the list
  for (i in 1:days) {
    datetime <- startdate + lubridate::days(i-1)
    datestamp <- strftime(datetime,'%Y-%m-%d', tz='GMT')
    logger.debug("Downloading data for %s", datestamp)
    url <- paste0(baseUrl,datestamp,'.csv')
    df <- suppressMessages( readr::read_csv(url, skip=1, col_names=col_names, col_types=col_types, progress=FALSE) )
    
    # Subset dataframe here so we don't grow too big if someone asks for many days
    if ( !is.null(countryCode) ) df <- df[df$country == countryCode,]
    if ( !is.null(parameter) ) df <- df[df$parameter == parameter,]
    
    # Subset based on time because we sometimes see times that don't match the requested file
    badIndexes <- which( as.POSIXct(df$utc, tz="UTC") != datetime )
    if ( length(badIndexes) > 0 ) {
      df <- df[- badIndexes,]
    }

    dfList[[i]] <- df
  }

  # Combine all dataframes
  df <- dplyr::bind_rows(dfList)
  
  # Remove any duplicate rows
  df <- dplyr::distinct(df)
  
  logger.info("Downloaded %d rows of OpenAQ data", nrow(df))
  
  return(df)
  
}
