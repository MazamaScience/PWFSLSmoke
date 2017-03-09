#' @keywords ws_monitor
#' @export
#' @title Reorder a ws_monitor Object
#' @param ws_monitor emph{ws_monitor} object
#' @param monitorIDs optional vector of monitor IDs used to reorder the meta and data dataframes
#' @return A emph{ws_monitor} object reordered to match \code{monitorIDs}.
#' @description This function is a convenience function that merely wraps the \link{monitor_subset} function
#' which reorders as well as subsets.

monitor_reorder <- function(ws_monitor, monitorIDs=NULL) {

  return( monitor_subset(ws_monitor, monitorIDs=monitorIDs) )
  
}
