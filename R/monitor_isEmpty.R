#' @keywords ws_monitor
#' @export
#' @title Test for an Empty ws_monitor Object
#' @param ws_monitor \emph{ws_monitor} object
#' @return \code{TRUE} if no monitors exist in \code{ws_monitor}, \code{FALSE} otherwise.
#' @description Convenience function for \code{nrow(ws_monitor$meta) == 0}.
#' This makes for more readable code in the many functions that need to test for this.
monitor_isEmpty <- function(ws_monitor) {
  
  return( nrow(ws_monitor$meta) == 0 )
  
}
