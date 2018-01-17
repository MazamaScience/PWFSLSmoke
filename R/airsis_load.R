#' @keywords AIRSIS
#' @export
#' @title Load Processed AIRSIS Monitoring Data
#' @param year desired year (integer or character representing YYYY)
#' @param baseUrl base URL for AIRSIS meta and data files
#' @return A \emph{ws_monitor} object with AIRSIS data.
#' @description Loads pre-generated .RData files containing AIRSIS PM2.5 data.
#' 
#' Avaialble RData and associated log files can be seen at:
#' \href{https://haze.airfire.org/monitoring/AIRSIS/RData/}{https://haze.airfire.org/monitoring/AIRSIS/RData/}
#' @seealso \code{\link{airsis_loadDaily}}
#' @seealso \code{\link{airsis_loadLatest}}
#' @examples
#' \dontrun{
#' airsis <- airsis_load(2017)
#' airsis_conus <- monitor_subset(airsis, stateCodes=CONUS)
#' monitorLeaflet(airsis_conus)
#' }

airsis_load <- function(year=2017,
                        baseUrl='https://haze.airfire.org/monitoring/AIRSIS/RData/') {
  
  # Create filepath
  filepath <- paste0(year,"/airsis_PM2.5_",year,".RData")
  
  # Define a 'connection' object so we can be sure to close it no matter what happens
  conn <- url(paste0(baseUrl,filepath))
  result <- try( suppressWarnings(ws_monitor <- get(load(conn))),
                 silent=TRUE )
  close(conn)
  
  if ( "try-error" %in% class(result) ) {
    stop(paste0("No AIRSIS data available for ",year), call.=FALSE)
  }
  
  return(ws_monitor)
  
}
