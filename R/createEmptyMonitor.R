#' @keywords internal
#' @export
#' @importFrom rlang .data
#'
#' @title Create empty monitor object
#'
#' @return A empty \emph{ws_monitor} object.
#'
#' @description The list of columns in the returned \code{meta} dataframe is:
#'
#' \preformatted{
#' > names(meta)
#'  [1] "monitorID"             "longitude"             "latitude"
#'  [4] "elevation"             "timezone"              "countryCode"
#'  [7] "stateCode"             "siteName"              "agencyName"
#' [10] "countyName"            "msaName"               "monitorType"
#' [13] "monitorInstrument"     "aqsID"                 "pwfslID"
#' [16] "pwfslDataIngestSource" "telemetryAggregator"   "telemetryUnitID"
#' }
#'
#' @examples
#' emptyMonitor <- createEmptyMonitor()
#' monitor_isMonitor(emptyMonitor)
#' monitor_isEmpty(emptyMonitor)

createEmptyMonitor <- function() {

  meta <- createEmptyMetaDataframe()
  data <-
    tibble(datetime = MazamaCoreUtils::parseDatetime(2019010100, timezone = "UTC")) %>%
    filter(.data$datetime == MazamaCoreUtils::parseDatetime(2018010100, timezone = "UTC"))

  # Create the 'ws_monitor' object
  ws_monitor <- list(meta = meta, data = data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))

  return(ws_monitor)

}
