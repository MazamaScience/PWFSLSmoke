#' @export
#'
#' @title Parse Datetime Strings
#'
#' @description
#' Transforms numeric and string representations of YYYYMMDD[HHMMSS] datetimes
#' to \code{POSIXct} format.
#'
#' Ymd, YmdH, YmdHM, and YmdHMS formats are understood, where:
#'
#'   * Y - four digit year
#'   * m - decimal number (1-12, 01-12) or english name month (October, oct.)
#'   * d - decimal number day of the month (0-31 or 01-31)
#'   * H - decimal number hours (0-24 or 00-24)
#'   * M - decimal number minutes (0-59 or 00-59)
#'   * S - decimal number seconds (0-61 or 00-61)
#'
#' This allows for mixed inputs. For example, 20181012130900,
#' "2018-10-12-13-09-00", and "2018 Oct. 12th 13:09:00" will all be converted to
#' the same \code{POSIXct} datetime. The incoming datetime vector does not need
#' to have a homogeneous format either -- "20181012" and "2018-10-12 13:09" can
#' exist in the same vector without issue. All incoming datetimes will be
#' interpreted in the specified timezone.
#'
#' If \code{datetime} is a \code{POSIXct} it will be returned unmodified, and
#' formats not recognized will be returned as \code{NA}.
#'
#' @section PWFSLSmoke Conventions:
#' Within the PWFSLSmoke package, datetimes not in \code{POSIXct} format are
#' represented as decimal values with no separation (ex: 20181012,
#' 20181012130900), either as numerics or strings.
#'
#' @section Implementation:
#' \code{parseDatetime} is essentially a wrapper around
#' \code{\link[packagename]{functioname}}, handling which formats we want to
#' account for.
#'
#' @param datetime vector of character or integer datetimes in YYYYMMDD[HHMMSS]
#'   format (or POSIXct).
#' @param timezone Olson timezone at the location of interest (default "UTC").
#' @param expectAll Logical value determining if the function should fail if
#'   any elements fail to parse (default \code{FALSE}).
#'
#' @return POSIXct datetimes.
#'
#' @seealso \code{\link[lubridate]{parse_date_time}} for implementation details.
#'
#' @examples
#' starttime <- parseDatetime(2015080718, timezone = "America/Los_Angeles")

parseDatetime <- function(datetime, timezone= "UTC", expectAll = FALSE) {

  orders <- c("Ymd", "YmdH", "YmdHM", "YmdHMS")
  parsedDatetime <- lubridate::parse_date_time(datetime, orders, tz = timezone)

  if (all(is.na(datetime))) {
    stop(paste0("No datetimes could be parsed."))
  }

  # If there already exist NAs in datetime, we don't want to accidently fail
  # if all non-NA values were parsed
  if (expectAll && which(is.na(datetime)) != which(is.na(parsedDatetime))) {

    stop(paste0(
      sum(is.na(datetime)), " datetimes could not be parsed",
      " (at indices: ", which(is.na(datetime)), ")."
    ))

  }

  return(datetime)
}
