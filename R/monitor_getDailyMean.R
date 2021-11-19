#' @title Calculate daily means for a ws_monitor object
#'
#' @description Calculates and returns daily means for a monitor. If either
#' \code{startdate} or \code{enddate} is \code{NULL}, a single value is
#' returned for that date.
#'
#' @param ws_monitor \emph{ws_monitor} object
#' @param monitorID monitor ID of interest
#' @param startdate desired start date (integer or character in Ymd format
#'        or \code{POSIXct})
#' @param enddate desired end date (integer or character in Ymd format
#'        or \code{POSIXct})
#'
#' @return A dataframe of daily means.
#'
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#' library(PWFSLSmoke)
#'
#' monitor_getDailyMean(
#'   PWFSLSmoke::Carmel_Valley,
#'   startdate = "2016-08-01",
#'   enddate = "2016-08-08"
#' )

monitor_getDailyMean <- function(
  ws_monitor,
  monitorID = NULL,
  startdate = NULL,
  enddate = NULL
) {

  # Validate arguments ---------------------------------------------------------

  if ( !monitor_isMonitor(ws_monitor) )
    stop("Required parameter 'ws_monitor' is not a valid ws_monitor object.")

  if ( monitor_isEmpty(ws_monitor) )
    stop("Required parameter 'ws_monitor' is empty.")

  if ( is.null(startdate) && is.null(enddate) )
    stop("Either 'startdate' or 'enddate' must be specified.")

  if ( nrow(ws_monitor$meta) == 1 ) {
    monitorID <- ws_monitor$meta$monitorID[1]
  } else {
    if ( !is.null(monitorID) ) {
      if ( !monitorID %in% ws_monitor$meta$monitorID ) {
        stop(paste0("Monitor ", monitorID, " is not found in 'ws_monitor'"))
      }
      ws_monitor <- monitor_subset(ws_monitor, monitorIDs = monitorID)
    }
  }

  # Time limits ----------------------------------------------------------------

  timezone <- ws_monitor$meta$timezone[1]

  # startdate
  if ( !is.null(startdate) ) {
    if ( is.numeric(startdate) || is.character(startdate) ) {
      startdate <- lubridate::ymd(startdate, tz = timezone)
    } else if ( lubridate::is.POSIXct(startdate) ) {
      startdate <- lubridate::force_tz(startdate, tzone = timezone)
    } else if ( !is.null(startdate) ) {
      stop(paste0(
        "Required parameter 'startdate' must be integer or character",
        " in Ymd format or of class POSIXct."))
    }
  }

  # enddate
  if ( !is.null(enddate) ) {
    if ( is.numeric(enddate) || is.character(enddate) ) {
      enddate <- lubridate::ymd(enddate, tz = timezone)
    } else if ( lubridate::is.POSIXct(enddate) ) {
      enddate <- lubridate::force_tz(enddate, tzone = timezone)
    } else if ( !is.null(enddate) ) {
      stop(paste0(
        "Required parameter 'enddate' must be integer or character",
        " in Ymd format or of class POSIXct."))
    }
    enddate <- enddate + lubridate::dhours(23)
  }

  # Handle case where one of the dates is missing
  if ( !is.null(startdate) && is.null(enddate) ) {
    enddate <- startdate + lubridate::dhours(23)
  } else if ( is.null(startdate) && !is.null(enddate) ) {
    startdate <- enddate - lubridate::dhours(23)
  }

  # Sanity check
  if ( enddate < min(ws_monitor$data$datetime) ||
       startdate > max(ws_monitor$data$datetime) ) {
    stop("The requested dates do not overlap with dates in 'ws_monitor'.")
  }

  # Calculate dailiy averages --------------------------------------------------

  mon <- monitor_subset(ws_monitor, tlim = c(startdate, enddate))

  monDaily <- monitor_dailyStatistic(mon, FUN = get("mean"))

  return(monDaily$data)

}
