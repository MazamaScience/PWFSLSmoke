#' @keywords ws_monitor
#' @export
#' @title Write monitor data as CSV
#' @param ws_monitor \emph{ws_monitor} object
#' @param saveFile optional filename where CSV will be written
#' @param metaOnly flag specifying whether to return \code{ws_monitor$meta} only
#' as a machine parseable CSV
#' @param dataOnly flag specifying whether to return \code{ws_monitor$data} only
#' as a machine parseable CSV
#' @param quietly do not print to console, just return the string representation
#' of the CSV
#' @description Prints out the contents of the \code{ws_monitor} object as CSV.
#' By default, the output is a text string with "human readable" CSV that
#' includes both \code{meta} and \code{data}. When saved as a file, this format
#' is useful for point-and-click spreadsheet users who want to have everything
#' on a single sheet.
#'
#' To obtain machine parseable CSV strings you can use \code{metaOnly} or
#' \code{dataOnly} which are mutually exclusive but which return CSV strings
#' that can be automatically ingested.
#'
#' By default, the CSV formatted text is returned invisibly but not saved to a
#' file unless \code{saveFile} is specified.
#' @note This function wraps the \link{monitor_print} function but defaults
#' to \code{quietly = FALSE}.
#' @examples
#' data("Carmel_Valley")
#' Carmel_Valley <- monitor_subset(Carmel_Valley, tlim = c(20160802,20160803))
#' monitor_print(Carmel_Valley)
#' monitor_print(Carmel_Valley, metaOnly = TRUE)
#' monitor_print(Carmel_Valley, dataOnly = TRUE)

monitor_writeCSV <- function(ws_monitor,
                             saveFile = NULL,
                             metaOnly = FALSE,
                             dataOnly = FALSE,
                             quietly = TRUE) {

  # Sanity check
  if ( monitor_isEmpty(ws_monitor) )
    stop("ws_monitor object contains zero monitors")

  monitor_print(ws_monitor, saveFile, metaOnly, dataOnly, quietly)

}
