#' @keywords ws_monitor
#'
#' @export
#'
#' @title Reorder a ws_monitor bject
#'
#' @param ws_monitor \emph{ws_monitor} object
#' @param monitorIDs Optional vector of monitor IDs used to reorder the meta and
#' data dataframes.
#' @param dropMonitors Logical specifying whether to remove monitors with no data.
#'
#' @return A \emph{ws_monitor} object reordered to match \code{monitorIDs}.
#'
#' @description This function is a convenience function that merely wraps the
#' \link{monitor_subset} function which reorders as well as subsets.

monitor_reorder <- function(
  ws_monitor,
  monitorIDs = NULL,
  dropMonitors = FALSE
) {

  return( monitor_subset(ws_monitor,
                         monitorIDs = monitorIDs,
                         dropMonitors = dropMonitors) )

}
