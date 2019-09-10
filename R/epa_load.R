#' @keywords EPA
#' @export
#' @title Load Processed EPA Monitoring Data
#' @param year desired year (integer or character representing YYYY)
#' @param parameterCode pollutant code
#' @param baseUrl base URL for EPA .RData files
#' @return A \emph{ws_monitor} object with EPA data for an entire year.
#' @description Please use \code{\link{airsis_loadAnnual}} instead of this
#' function. It will soon be deprecated.

epa_load <- function(year=strftime(lubridate::now(tzone = "UTC"), "%Y", tz = "UTC"),
                     parameterCode='88101',
                     baseUrl='https://haze.airfire.org/monitoring/EPA/RData/') {

  # TODO:  .Deprecate() this function at some point

  ws_monitor <- epa_loadAnnual(year, parameterCode)

  return(ws_monitor)

}
