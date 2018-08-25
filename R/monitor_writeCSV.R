#' @keywords ws_monitor
#' @export
#' @title Write Monitor Data as CSV File
#' @param ws_monitor \emph{ws_monitor} object
#' @param saveFile filename where CSV will be written
#' @param metaOnly flag specifying whether to use \code{ws_monitor$meta} for a machine parseable CSV
#' @param dataOnly flag specifying whether to use \code{ws_monitor$data} for a machine parseable CSV
#' @description Writes the contents of the \code{ws_monitor} object as CSV file.
#' By default, the output is a "human readable" CSV that includes both \code{meta} and \code{data}.
#' This format is useful for point-and-click spreadsheet users who want to have 
#' everything on a single sheet.
#' 
#' To obtain machine parseable CSV files you can use \code{metaOnly} or \code{dataOnly} which are mutually
#' exclusive but which generate CSV files that can be automatically ingested.
#' @examples
#' \dontrun{
#' data("Carmel_Valley")
#' Carmel_Valley <- monitor_subset(Carmel_Valley, tlim=c(20160802,20160803))
#' monitor_writeCSV(Carmel_Valley, 'CarmelValley.csv')
#' monitor_writeCSV(Carmel_Valley, 'CarmelValley_meta.csv', metaOnly = TRUE)
#' monitor_writeCSV(Carmel_Valley, 'CarmelValley_data.csv', dataOnly = TRUE)
#' }

monitor_writeCSV <- function(ws_monitor,
                             saveFile = "ws_monitor.csv",
                             metaOnly = FALSE,
                             dataOnly = FALSE) {
  
  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) stop("ws_monitor object contains zero monitors")

  monitor_print(ws_monitor, saveFile, metaOnly, dataOnly, quietly = TRUE)

}
