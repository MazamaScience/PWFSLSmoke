#' @keywords AIRSIS
#' @export
#' @import MazamaCoreUtils
#'
#' @title Get AIRSIS available unit identifiers
#'
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
#'                                  provider = 'USFS',
#'                                  unitTypes = c('EBAM','ESAM'))
#' }

airsis_availableUnits <- function(
  startdate = strftime(lubridate::now("UTC"), "%Y010100", tz = "UTC"),
  enddate = strftime(lubridate::now("UTC"), "%Y%m%d23", tz = "UTC"),
  provider = 'USFS',
  unitTypes = c('BAM1020', 'EBAM', 'ESAM'),
  baseUrl = "http://xxxx.airsis.com/vision/common/CSVExport.aspx?"
) {

  logger.debug(" ----- airsis_availableUnits() ----- ")

  unitTypes <- toupper(unitTypes)

  # Sanity Check
  badUnitTypes <- setdiff(unitTypes, names(AIRSIS$unitTypes))
  goodUnitTypes <- intersect(unitTypes, names(AIRSIS$unitTypes))
  if ( length(badUnitTypes) > 0 ) {
    badUnitTypesString <- paste0(badUnitTypes, collapse = ", ")
    logger.warn("Unrecognized AIRSIS unitType(s): '%s'", badUnitTypesString)
  }
  if ( length(goodUnitTypes) == 0 ) {
    err_msg <- paste0("No valid AIRSIS unitType(s): ", paste0(unitTypes, collapse = ", "))
    logger.error(err_msg)
    stop(err_msg, call. = FALSE)
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
                  '&StartDate=', strftime(starttime, "%Y-%m-%d", tz="UTC"),
                  '&EndDate=', strftime(endtime, "%Y-%m-%d", tz="UTC"))

    logger.trace("Downloading AIRSIS data from %s", url)

    # Read the url output into a string
    fileString <- readr::read_file(url)

    # Output has the following header:
    #   "MasterTable_ID,UnitID,Alias,Latitude,Longitude,TimeStamp,PDate"

    # We just want the unitIDs
    # NOTE:  Normally, we avoid append() (see R Inferno) but we only have a few iterations and a few IDs so it's OK
    df <- readr::read_csv(fileString)

    logger.trace("Parsing %d lines of csv", nrow(df))

    unitIDs <- append(unitIDs, unique(df$UnitID))

  }

  # Sanity check
  if ( any(duplicated(unitIDs)) ) {
    duplicates <- unitIDs[which(duplicated(unitIDs))]
    duplicatesString <- paste0(duplicates, colllapse = ', ')
    logger.error("Duplicate unitIDs found: %s", duplicatesString)
    stop(paste0("Duplicate unitIDs found: ", duplicatesString))
  }

  return(as.character(sort(unitIDs)))

}
