#' @keywords AIRSIS
#' @export
#' @title Load Processed AIRSIS Monitoring Data
#' @param year desired year (integer or character representing YYYY)
#' @param baseUrl base URL for AIRSIS meta and data files
#' @return A \emph{ws_monitor} object with AIRSIS data.
#' @description Please use \code{\link{airsis_loadAnnual}} instead of this
#' function. It will soon be deprecated.

airsis_load <- function(year=2017,
                        baseUrl='https://haze.airfire.org/monitoring/AIRSIS/RData/') {

  # TODO:  .Deprecate() this function at some point

  # # Create filepath
  # filepath <- paste0(year,"/airsis_PM2.5_",year,".RData")
  #
  # # Define a 'connection' object so we can be sure to close it no matter what happens
  # conn <- url(paste0(baseUrl,filepath))
  # result <- try( suppressWarnings(ws_monitor <- get(load(conn))),
  #                silent=TRUE )
  # close(conn)
  #
  # if ( "try-error" %in% class(result) ) {
  #   stop(paste0("No AIRSIS data available for ",year), call.=FALSE)
  # }

  ws_monitor <- airsis_loadAnnual(year)

  return(ws_monitor)

}
