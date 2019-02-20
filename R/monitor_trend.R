#' #' @keywords ws_monitor
#' #'
#' #' @export
#' #'
#' #' @title Calculate trends for ws_monitor bject
#' #'
#' #' @description This function calculates 'trend' metric timeseres for every
#' #' monitor in the \code{ws_monitor}.
#' #'
#' #' The initial "pwfsl" implementation calculates NowCast timeseries for each
#' #' monitor and then subtracts the hourly pm2.5 value from the damped NowCast
#' #' timeseries.
#' #'
#' #' @param ws_monitor \emph{ws_monitor} object
#' #' @param type Character string specifying the type of trend algorithm to be
#' #' used.
#' #'
#' #' @return A \emph{ws_monitor} object where all data values have been replaced
#' #' with 'trend' values.
#'
#' monitor_trend <- function(ws_monitor,
#'                           type = "pwfsl") {
#'
#'   # Validate parameters --------------------------------------------------------
#'
#'   if ( monitor_isEmpty(ws_monitor) ) {
#'     stop("ws_monitor object contains zero monitors")
#'   }
#'
#'   # type
#'   validTypes <- c("pwfsl")
#'   if ( !is.null(type) ) {
#'     if ( !type %in% validTypes ) {
#'       stop(
#'         paste0(
#'           "Invalid type: \"", type, "\". ",
#'           "The following types are supported: \"",
#'           paste0(validTypes, sep = "|"), "\""
#'         )
#'       )
#'     }
#'   } else {
#'     type <- "pwfsl"
#'   }
#'
#'   # Calculate trends -----------------------------------------------------------
#'
#'   # Comparison
#'   a <- monitor_nowcast(ws_monitor)
#'   b <- monitor_rollingMean(ws_monitor, 5, align = "right")
#'
#'   a_data <- a$data[,-1] # omit datetime
#'   b_data <- b$data[,-1] # omit datetime
#'   trend_data <- a_data - b_data
#'
#'   ws_monitor$data[,-1] <- trend_data
#'
#'   # Return ---------------------------------------------------------------------
#'
#'   return(ws_monitor)
#'
#' }
