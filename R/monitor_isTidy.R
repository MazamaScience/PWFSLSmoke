#' @title Check if data is tidy-formatted ws_monitor data
#'
#' @description
#' Verifies that the given data can be treated as tidy-formatted "ws_monitor"
#' data. This is done by verifying that the data is a tibble data.frame object
#' with columns for information in all `ws_monitor` objects.
#'
#' @param data Data to validate.
#'
#' @return True if the data is in a recognized 'Tidy' format, otherwise False.
#'
#' @export
#'
#' @examples
#' ws_monitor <- monitor_subset(
#'   Northwest_Megafires,
#'   monitorIDs = c('530470009_01', '530470010_01')
#' )
#'
#' ws_monTidy <- monitor_toTidy(ws_monitor)
#' monitor_isTidy(ws_monTidy)
#'
#' \dontrun{
#' monitor_isTidy(ws_monitor)
#' }
#'
monitor_isTidy <- function(data = NULL) {

  if (is.null(data)) {
    stop("Data parameter cannot be NULL")
  }

  requiredColumns <- c(
    "datetime", "monitorID", "pm25", "longitude", "latitude", "elevation",
    "timezone", "countryCode", "stateCode"
  )

  isTidy <- (
    all(
      c("tbl_df", "tbl", "data.frame") %in% class(data)) &&
      all(requiredColumns %in% colnames(data)
    )
  )

  return(isTidy)
}
