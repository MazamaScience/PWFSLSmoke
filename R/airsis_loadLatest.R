#' @keywords AIRSIS
#' @export
#' @title Load Recent AIRSIS Monitoring Data
#' @param baseUrl location of the AIRSIS latest data file
#' @param file name of the AIRSIS latest data file
#' @description The most recent 45 days of AIRSIS data are updated in real time
#' at PWFSL and can be loaded with this function.
#' @return A \emph{ws_monitor} object with AIRSIS data.
#' @examples
#' \dontrun{
#' airsis <- airsis_loadLatest()
#' }

airsis_loadLatest <- function(baseUrl='https://haze.airfire.org/monitoring/RData/',
                              file='airsis_pm25_latest.RData') {
  
  # Define a 'connection' object so we can be sure to close it
  conn <- url(paste0(baseUrl,file))
  ws_monitor <- get(load(conn))
  if ( isOpen(conn) ) close(conn)
  
  return(ws_monitor)
}
