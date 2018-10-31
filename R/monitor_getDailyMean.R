#' @title Calculate Daily Means for a Monitor Object
#'
#' @description Calculates and returns daily means for a monitor.
#' 
#' @param ws_monitor \emph{ws_monitor} object.
#' @param monitorID Monitor ID of interest.
#' @param startdate Desired start date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param enddate Desired end date (integer or character in Ymd format
#'        or \code{POSIXct}).
#'
#' @return A vector of daily means for
#'
#' @importFrom dplyr filter
#' @export
#' 
#' @examples
#' monitor_getDailyMean(PWFSLSmoke::Carmel_Valley, startdate = "2016-08-08")

monitor_getDailyMean <- function(ws_monitor,
                                 monitorID = NULL,
                                 startdate = NULL,
                                 enddate = NULL) {

  # For debugging --------------------------------------------------------------
  
  if (FALSE) {
    
    # Carmel Valley
    ws_monitor <- PWFSLSmoke::Carmel_Valley
    monitorID <- "060530002_01"
    startdate <- "2016-08-07"
    enddate <- NULL
    
  }
  
  # Validate arguments ---------------------------------------------------------
  
  if ( !monitor_isMonitor(ws_monitor) ) {
    stop("Required parameter 'ws_monitor' is not a valid ws_monitor object")
  } else if ( monitor_isEmpty(ws_monitor) ) {
    stop("Required parameter 'ws_monitor' is empty.")
  }
  
  if ( nrow(ws_monitor$meta) == 1 ) {
    monitorID <- ws_monitor$meta$monitorID[1]
  } else {
    if ( is.null(monitorID) ) {
      stop("Argument 'monitorID' must be defined.")
    } else if ( !monitorID %in% ws_monitor$meta$monitorID ) {
      stop(paste0("Monitor ", monitorID, " is not found in 'ws_monitor'"))
    }
    ws_monitor <- monitor_subset(ws_monitor, monitorIDs = monitorID)
  }
  
  # Time limits ----------------------------------------------------------------
  
  # Subset based on startdate and enddate
  
  timezone <- ws_monitor$meta$timezone[1]
  
  # TODO:  code to handle three options:
  # TODO:  1) startdate defined, enddate == NULL
  # TODO:  2) startdate == NULL, enddate defined
  # TODO:  3) both defined
  
  if ( !is.null(startdate) && is.null(enddate) ) {
    
    if ( is.numeric(startdate) || is.character(startdate) ) {
      startdate <- lubridate::ymd(startdate, tz = timezone)
    } else if ( lubridate::is.POSIXct(startdate) ) {
      startdate <- lubridate::force_tz(startdate, tzone = timezone)
    } else if ( !is.null(startdate) ) {
      stop(paste0(
        "Required parameter 'startdate' must be integer or character",
        " in Ymd format or of class POSIXct."))
    }
    enddate <- startdate + lubridate::dhours(23)
    
    # TODO:  } else if ( is.null(startdate) && !is.null(enddate) ) {
    
    # TODO:  } else if ( !is.null(startdate) && !is.null(enddate) ) {
    
  }
  
  # Calculate dailiy averages --------------------------------------------------
  
  mon <- monitor_subset(ws_monitor, tlim=c(startdate, enddate))
  
  monDaily <- monitor_dailyStatistic(mon, FUN=get("mean"))
  
  dailyMean <- monDaily$data %>%
    filter(monDaily$data$datetime == startdate) %>%
    pull(2)
  
  
  return(dailyMean)
  
}