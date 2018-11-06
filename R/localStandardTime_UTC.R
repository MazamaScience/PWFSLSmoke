#'
#' @title Create UTC version of Local Standard Time
#'
#' @description
#' Creates a "Local Standard Time" version of a timeseries with monotonically
#' increasing hours that avoids labeling issues during the change between
#' standard time and daylight savings.
#'
#' While the \pkg{lubridate} package makes it easy to display time labels in
#' "Local Time", this causes labeling problems for timeseries that cross the
#' boundary between daylight savings time and standard time.
#'
#' This function provides a solution for this labeling problem by returning
#' a UTC version of the timeseries that has been shifted by `UTC_offset` hours
#' as found in the \code{@data} slot of the
#' \code{MazamaSpatialUtils::SimpleTimezones} spatial dataset.
#'
#' @note The returned timeseries represents different physical times than the
#' incoming \code{datetime} vector. \strong{It should only be used for labeling.}
#'
#' @param datetime vector of character or integer datetimes in Ymd[HMS] format
#'   (or POSIXct).
#' @param timezone Olson timezone at the location of interest.
#'
#' @return Vector of POSIXct datetimes.
#'
#' @importFrom rlang .data
#' @importFrom dplyr filter pull
#' @export
#'
#' @examples
#' \dontrun{
#' Thompson_Falls <- monitor_load(20181031, 20181111,
#'                                monitorIDs = "300890007_01")
#' datetime <- Thompson_Falls$data$datetime
#' timezone <- Thompson_Falls$meta$timezone
#' LST_UTC <- localStandardTime_UTC(datetime, timezone)
#' lubridate::with_tz(datetime, "America/Denver")[103:106]
#' lubridate::with_tz(LST_UTC, "UTC")[103:106]
#' strftime(datetime[103:106], "%Y-%m-%d %H:%M", tz = timezone, usetz = TRUE)
#' strftime(LST_UTC[103:106], "%Y-%m-%d %H:%M MST", tz = "UTC")
#' }

localStandardTime_UTC <- function(datetime, timezone = NULL) {

  # Validate args ----------------------------------------------------------------

  if ( !lubridate::is.POSIXct(datetime) ) {
    stop("Required parameter 'datetime' is not of class POSIXct.")
  }

  # error handling ----------------------------------------------------------

  if ( is.null(timezone) ) {
    timezone <- lubridate::tz(datetime)
  }

  # Change the name so it doesn't conflict with "timezone" inside the dataframe
  tz <- timezone

  # Calculate the Local Standard Time offset
  lst_offset <- MazamaSpatialUtils::SimpleTimezones@data %>%
    filter(.data$timezone == tz) %>%
    dplyr::pull(.data$UTC_offset)

  # 'datetime' should be UTC but set it just in case
  LST_UTC <- lubridate::with_tz(datetime, "UTC") +
    lst_offset * lubridate::dhours(1)

  return(LST_UTC)

}
