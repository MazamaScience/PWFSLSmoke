#' @keywords AirNow
#' @export
#' @title Load Processed AirNow Monitoring Data
#' @param year desired year (integer or character representing YYYY)
#' @param month desired month (integer or character representing MM)
#' @param parameter parameter of interest
#' @param baseUrl base URL for AirNow meta and data files
#' @return A \emph{ws_monitor} object with AirNow data.
#' @description Please use \code{\link{airnow_loadAnnual}} instead of this
#' function. It will soon be deprecated.


airnow_load <- function(year=2017,
                        month=NULL,
                        parameter='PM2.5',
                        baseUrl='https://haze.airfire.org/monitoring/AirNow/RData/') {

  # TODO:  .Deprecate() this function at some point

  # # Convert to character for consistency
  # year <- as.character(year)
  #
  # # Sanity check
  # validParams <- c("PM2.5")
  # if ( !parameter %in% validParams ) {
  #   paramsString <- paste(validParams, collapse=", ")
  #   stop(paste0("Parameter '", parameter, "' is not supported. Try one of: ", paramsString))
  # }
  #
  # # Create filepath
  # if ( is.null(month) ) {
  #   yearMonth <- lubridate::ymd(paste0(year,"0101"))
  #   part1 <- strftime(yearMonth, "%Y/airnow_", tz="UTC")
  #   part2 <- strftime(yearMonth, "_%Y.RData", tz="UTC")
  # } else {
  #   yearMonth <- lubridate::ymd(paste0(year,sprintf("%02d",month),"01"))
  #   part1 <- strftime(yearMonth, "%Y/%m/airnow_", tz="UTC")
  #   part2 <- strftime(yearMonth, "_%Y_%m.RData", tz="UTC")
  # }
  # filepath <- paste0(part1,parameter,part2)
  #
  # # Define a 'connection' object so we can be sure to close it no matter what happens
  # conn <- url(paste0(baseUrl,filepath))
  # result <- try( suppressWarnings(ws_monitor <- get(load(conn))),
  #                silent=TRUE )
  # close(conn)
  #
  # if ( "try-error" %in% class(result) ) {
  #   if ( is.null(month) ) {
  #     stop(paste0("No AirNow data available for ",year), call.=FALSE)
  #   } else {
  #     stop(paste0("No AirNow data available for ",stringr::str_sub(yearMonth,1,6)), call.=FALSE)
  #   }
  # }

  ws_monitor <- airnow_loadAnnual(year)

  return(ws_monitor)

}
