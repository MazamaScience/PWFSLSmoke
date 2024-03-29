#' @export
#' @title Return Day Stamps for Values Above a Threshold
#' @param df dataframe with \code{datetime} column in UTC
#' @param dataVar variable to be evaluated
#' @param tzone timezone where data were collected
#' @param highlightRange range of values of to be highlighted
#' @description Returna list of dates in YYYYMMDD format where
#' the dataVar is within \code{highlightRange}.
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' raw <- airsis_createRawDataframe(startdate = 20160901, provider = 'USFS',unitID = '1033')
#' raw <- raw_enhance(raw)
#' highlightRange <- c(50,Inf)
#' dataVar <- 'pm25'
#' tzone <- "America/Los_Angeles"
#' highlightDates <- raw_getHighlightDates(raw,dataVar,tzone,highlightRange)
#' rawPlot_timeOfDaySpaghetti(df=raw,highlightDates = highlightDates)
#'
#' }, silent = FALSE)
#' }

raw_getHighlightDates <- function(
  df,
  dataVar,
  tzone = NULL,
  highlightRange = c(1e12, Inf)
) {

  # Sanity check -- 'datetime' must exist
  if ( !'datetime' %in% names(df) ) {
    stop(paste0("Dataframe 'df' has no 'datetime' column."))
  }

  # Data Preparation ----------------------------------------------------------

  df$localTime <- lubridate::with_tz(df$datetime, tzone = tzone)
  df$date <- format(df$localTime,"%Y%m%d")

  # Create a subset dataframe for local use
  df <- df[,c('localTime',dataVar,'date')]
  names(df) <- c('localTime','data','date')

  subDF <- dplyr::filter(df, df$data >= highlightRange[1] & df$data <= highlightRange[2])

  highlightDates <- unique(subDF$date)

  return(highlightDates)
}

