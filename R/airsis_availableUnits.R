#' @keywords AIRSIS
#' @export
#' @title Get Available Unit Identifiers from AIRSIS
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param provider identifier used to modify baseURL \code{['APCD'|'USFS']}
#' @param unitTypes vector of unit types
#' @param baseUrl base URL for data queries
#' @description Returns a list of unitIDs with data during a particular time period. 
#' @return Vector of AIRSIS unitIDs.
#' @references \href{http://usfs.airsis.com}{Interagency Real Time Smoke Monitoring}
#' @examples
#' \dontrun{
#' unitIDs <- airsis_availableUnits(20150701, 20151231,
#'                                  provider='USFS', 
#'                                  unitType=c('EBAM','ESAM'))
#' }

airsis_availableUnits <- function(startdate=NULL,
                                  enddate=NULL,
                                  provider='USFS', unitTypes=c('BAM1020','EBAM','ESAM'),
                                  baseUrl="http://xxxx.airsis.com/vision/common/CSVExport.aspx?") {
  
  # Sanity checks
  if ( is.null(startdate) ) {
    logger.error("Required parameter 'startdate' is missing")
    stop(paste0("Required parameter 'startdate' is missing"))
  }
  
  if ( is.null(startdate) ) {
    logger.error("Required parameter 'enddate' is missing")
    stop(paste0("Required parameter 'enddate' is missing"))
  }
  
  if ( is.null(unitType) ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }
  
  unitTypes <- toupper(unitTypes)
  
  # Sanity Check
  if ( !unitType %in% names(AIRSIS$unitTypes) ) {
    logger.error("Parameter 'unitType=%s' is not recognized", unitType)
    stop(paste0("Parameter 'unitType=", unitType, "' is not recognized"))
  }
    
  # Get UTC times
  starttime <- parseDatetime(startdate)
  endtime <- parseDatetime(enddate)
  
  # Example URL:
  #   http://usfs.airsis.com/vision/common/CSVExport.aspx?utid=38&StartDate=2016-02-03&EndDate=2016-02-03
  
  # Create a valid baseUrl
  baseUrl <- stringr::str_replace(baseUrl, 'xxxx', provider)
  
  unitIDs <- c()
  for ( unitType in unitTypes ) {
    
    # Create URL
    url <- paste0(baseUrl, 'utid=', AIRSIS$unitTypes[[unitType]],
                  '&StartDate=', strftime(starttime, "%F", tz="GMT"),
                  '&EndDate=', strftime(endtime, "%F", tz="GMT"))
    
    logger.debug("Downloading AIRSIS data from %s", url)
    
    # Read the url output into a string
    fileString <- readr::read_file(url)
    
    # Output has the following header:
    #   "MasterTable_ID,UnitID,Alias,Latitude,Longitude,TimeStamp,PDate"
    
    # We just want the unitIDs
    # NOTE:  Normally, we avoid append() (see R Inferno) but we only have a few iterations and a few IDs so it's OK
    df <- readr::read_csv(fileString)
    
    logger.debug("Parsing %d lines of csv", nrow(df))
    
    unitIDs <- append(unitIDs, unique(df$UnitID))
    
  }
  
  # Sanity check
  if ( any(duplicated(unitIDs)) ) {
    duplicates <- unitIDs[which(duplicated(unitIDs))]
    duplicatesString <- paste0(duplicates, colllapse=', ')
    logger.error("Duplicate unitIDs found: %s", duplicatesString)
    stop(paste0("Duplicate unitIDs found: ", duplicatesString))
  }

  return(as.character(sort(unitIDs)))
  
}
