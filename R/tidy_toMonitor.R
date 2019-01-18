#' @title Convert `ws_tidy` data to a `ws_monitor` object
#'
#' @description
#' Changes read-optomized 'tidy' formatted monitor data into a write-optomized
#' `ws_monitor` format. If the given data is already a `ws_monitor` object,
#' it is returned as is. This function is the inverse of \code{\link{monitor_toTidy}}.
#'
#' @param data Data to potentially convert.
#' @return `ws_monitor` object
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' ws_monitor <- monitor_subset(
#'   Northwest_Megafires,
#'   monitorIDs = c('530470009_01', '530470010_01')
#' )
#'
#' ws_monTidy <- monitor_toTidy(ws_monitor)
#' ws_monMon <- tidy_toMonitor(ws_monTidy)
#' head(ws_monMon$data)
#' head(ws_monitor$data)
#
tidy_toMonitor <- function(data = NULL) {
  if (monitor_isTidy(data)) {
    metaColumns <-
      names(dplyr::select(data, -.data$datetime, -.data$pm25))
    wideData <- tidyr::spread(data, .data$datetime, .data$pm25)
    
    meta <- dplyr::select(wideData, metaColumns) %>%
      as.data.frame()
    rownames(meta) <- meta$monitorID
    
    data_wide <-
      dplyr::select(wideData, -metaColumns, .data$monitorID)
    data <-
      tidyr::gather(data_wide, "datetime", "pm25", -.data$monitorID) %>%
      tidyr::spread("monitorID", "pm25") %>%
      as.data.frame()
    
    ws_monitor <- structure(list(data = data, meta = meta),
                            class = c("ws_monitor", "list"))
    
  } else if (monitor_isMonitor(data)) {
    message("Data is already a ws_monitor object.")
    ws_monitor <- data
    
  } else {
    stop("Data is not in a reconized format.")
  }
  
  return(ws_monitor)
}
