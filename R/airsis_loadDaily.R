#' @keywords AIRSIS
#' @export
#' @title Load Recent AIRSIS Monitoring Data
#' @param baseUrl location of the AIRSIS latest data file
#' @return A \emph{ws_monitor} object with AIRSIS data.
#' @description Loads pre-generated .RData files containing the most recent AIRSIS data.
#' 
#' The daily files are generated once a day, shortly after midnight and contain data for the
#' previous 45 days. 
#' 
#' For the most recent data, use \code{airsis_loadLatest()}.
#' 
#' Avaialble RData and associated log files can be seen at:
#' \href{https://haze.airfire.org/monitoring/AIRSIS/RData/latest}{https://haze.airfire.org/monitoring/AIRSIS/RData/latest}
#' @seealso \code{\link{airsis_load}}
#' @seealso \code{\link{airsis_loadLatest}}
#' @examples
#' \dontrun{
#' airsis <- airsis_loadDaily()
#' }

airsis_loadDaily <- function(baseUrl='https://haze.airfire.org/monitoring/AIRSIS/RData/') {
  
  # Create filepath
  filepath <- paste0("latest/airsis_PM2.5_latest45.RData")
  
  # Define a 'connection' object so we can be sure to close it no matter what happens
  conn <- url(paste0(baseUrl,filepath))
  result <- try( suppressWarnings(ws_monitor <- get(load(conn))),
                 silent=TRUE )
  close(conn)
  
  if ( "try-error" %in% class(result) ) {
    stop(paste0("URL unavailable: ",paste0(baseUrl,filepath)), call.=FALSE)
  }
  
  
  return(ws_monitor)
}
