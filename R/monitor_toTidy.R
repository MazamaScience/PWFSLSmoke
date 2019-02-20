#' @title Convert `ws_monitor` data to a tidy format
#'
#' @description
#' Changes write-optomized `ws_monitor` formatted data into a read-optomized
#' 'tidy' format that is useful for 'tidyverse' functions. If the given data is
#' already in a tidy format, it is returned as is.
#'
#' @param data Data to potentially convert.
#' @return 'Tidy' formatted `ws_monitor` data.
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
#'
#' \dontrun{
#' ws_monTidy2 <- monitor_toTidy(ws_monTidy)
#' }
#'
monitor_toTidy <- function(data = NULL) {

  if (monitor_isMonitor(data)) {

    monMeta <- data[["meta"]]
    monData <- data[["data"]]

    ws_monTidy <-  monData %>%
      tidyr::gather("monitorID", "pm25", -.data$datetime) %>%
      dplyr::inner_join(monMeta, by = "monitorID") %>%
      tibble::as_tibble()

  } else if (monitor_isTidy(data)) {

    message("Data is already in a tidy format.")
    ws_monTidy <- data

  } else {
    stop("Data is not in a reconized format.")
  }

  return(ws_monTidy)
}
