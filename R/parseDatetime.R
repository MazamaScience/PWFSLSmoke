#' @export
#' @title Parse Datetime Strings
#' @param datetime character or integer datetimes in YYYYMMDD[HHMMSS] format
#' @param timezone Olson timezone at the location of interest
#' @description 8-, 10-, 12- and 14-digit formats are understood, e.g: 20150721 to 20150721000000.
#' Integers will be converted to character before parsing.
#' @return POSIXct datetimes.
#' @examples
#' starttime <- parseDatetime(2015080718)

parseDatetime <- function(datetime, timezone='UTC') {
  
  datetime <- as.character(datetime)
  length <- stringr::str_length(datetime)[1]
  
  if (length == 8) {
    datetime <- lubridate::ymd(datetime, tz=timezone)
  } else if (length == 10) {
    datetime <- lubridate::ymd_h(datetime, tz=timezone)
  } else if (length == 12) {
    datetime <- lubridate::ymd_hm(datetime, tz=timezone)
  } else if (length == 14) {
    datetime <- lubridate::ymd_hms(datetime, tz=timezone)
  } else {
    stop(paste0('Datetime "',datetime,'" cannot be parsed.'))
  }
  
  # guarantee that we return a POSIXct time
  return(as.POSIXct(datetime))
}
