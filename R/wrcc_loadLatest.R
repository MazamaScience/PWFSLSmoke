#' @keywords WRCC
#' @export
#' @title Load Recent WRCC Monitoring Data
#' @param baseUrl location of the WRCC latest data file
#' @param file name of the WRCC latest data file
#' @description The most recent 45 days of WRCC data are updated in real time
#' at PWFSL and can be loaded with this function.
#' @return A \emph{ws_monitor} object with WRCC data.
#' @examples
#' \dontrun{
#' wrcc <- wrcc_loadLatest()
#' }

wrcc_loadLatest <- function(baseUrl='https://haze.airfire.org/monitoring/RData/',
                            file='wrcc_pm25_latest.RData') {
  
  ws_monitor <- get(load(url(paste0(baseUrl,file))))
  
  return(ws_monitor)
}
