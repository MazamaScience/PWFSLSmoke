#' @keywords WRCC
#' @export
#' @title Load Recent WRCC Monitoring Data
#' @param baseUrl location of the WRCC latest data file
#' @return A \emph{ws_monitor} object with WRCC data.
#' @description Loads pre-generated .RData files containing the most recent WRCC data.
#' 
#' The daily files are generated once a day, shortly after midnight and contain data for the
#' previous 45 days. 
#' 
#' For the most recent data, use \code{wrcc_loadLatest()}.
#' 
#' Avaialble RData and associated log files can be seen at:
#' \href{https://haze.airfire.org/monitoring/WRCC/RData/latest}{https://haze.airfire.org/monitoring/WRCC/RData/latest}
#' @seealso \code{\link{wrcc_load}}
#' @seealso \code{\link{wrcc_loadLatest}}
#' @examples
#' \dontrun{
#' wrcc <- wrcc_loadDaily()
#' }

wrcc_loadDaily <- function(baseUrl='https://haze.airfire.org/monitoring/RData/') {
  
  # Create filepath
  filepath <- paste0("latest/wrcc_PM2.5_latest45.RData")
  
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
