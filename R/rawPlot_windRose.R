#' @keywords raw
#' @export
#' @import graphics
#' @title Create Wind Rose Plot from a Raw Dataframe
#' @param df enhanced, raw dataframe as created by the \code{raw_enhance()} function
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param localTime logical specifying whether \code{tlim} is in local time or UTC
#' @param ... additional arguments to pass on to \code{openair::windRose()}
#' @description Create wind rose plot from raw_enhance object. Based on openair::windRose().
#' @note If more than one timezone is found, \code{localTime} is ignored and UTC is used.
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' raw <- airsis_createRawDataframe(20160901, 20161015, provider='USFS', unitID=1012)
#' raw <- raw_enhance(raw)
#' rawPlot_windRose(raw)
#'
#' }, silent = FALSE)
#' }

rawPlot_windRose <- function(
  df,
  tlim = NULL,
  localTime = TRUE,
  ...
) {

  # ----- Data Preparation -----------------

  # Identify timezone(s)
  timezone <- unique(df$timezone)

  # Force timezone to UTC if >1 timezone in metadata for monitorIDs
  if ( length(timezone) > 1 ) { # note that we will only enter this condition if localTime==TRUE
    if ( localTime ) {
      warning(">1 timezone in data: Timezone (including tlim, if specified) forced to UTC")
      timezone <- "UTC"
    }
  }

  # Set timezone to UTC if localTime==FALSE
  if ( !localTime ) {
    timezone <- "UTC"
  }

  # Set time axis data
  df$datetime <- lubridate::with_tz(df$datetime, tzone=timezone)

  # Time limit application
  # TODO: add logic to check for tlim format
  # TODO: warn if tlim is outside range of datetime data
  if ( !is.null(tlim) ) {
    # When tlim is specified in whole days we add hours to get the requsted full days
    tlimStrings <- as.character(tlim)
    if ( stringr::str_length(tlimStrings)[1] == 8 ) {
      tlim[1] <- paste0(tlim[1],'00')
    }
    if ( stringr::str_length(tlimStrings)[2] == 8 ) {
      tlim[2] <- paste0(tlim[2],'23')
    }
    tlim <- MazamaCoreUtils::parseDatetime(tlim, timezone = timezone)

    # Create time mask and subset data
    timeMask <- df$datetime >= tlim[1] & df$datetime <= tlim[2]
    if ( sum(timeMask)==0 ) {
      stop("No data contained within specified time limits, please try again.")
    }
    df <- df[timeMask,]
  }

  # Populate arguments for openair::windRose
  argsList <- list(...)
  argsList$mydata <- df
  if ( !('angle' %in% names(argsList)) ) {
    argsList$angle <- 45
  }
  if ( !('ws' %in% names(argsList)) ) {
    argsList$ws <- "windSpeed"
  }
  if ( !('wd' %in% names(argsList)) ) {
    argsList$wd <- "windDir"
  }
  if ( !('paddle' %in% names(argsList)) ) {
    argsList$paddle <- FALSE
  }

  # ----- Plotting -------------------------

  # Create Plot
  do.call(openair::windRose,argsList)

}
