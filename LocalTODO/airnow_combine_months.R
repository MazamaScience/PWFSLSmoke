#' @keywords AirNow
#' @export
#' @title Load Processed AirNow Monitoring Data
#' @param startdate desired start date (integer or character representing YYYYMMDD)
#' @param enddate desired end date (integer or character representing YYYYMMDD)
#' @param parameter parameter of interest
#' @param baseUrl base URL for AirNow meta and data files
#' @return A \emph{ws_monitor} object with AirNow data.
#' @description Loads pre-generated .RData files containing covering the specified time range
#' including all days within each month in the range. These separate, monthly files are combined
#' into a single \emph{ws_monitor} object.
#' 
#' 
#' Avaialble RData and associated log files can be seen at:
#' \href{https://haze.airfire.org/monitoring/AirNow/RData/}{https://haze.airfire.org/monitoring/AirNow/RData/}
#' @examples
#' \dontrun{
#' airnow <- airnow_load(20150901, 20150930)
#' airnow_conus <- monitor_subset(airnow, stateCodes=CONUS)
#' monitorLeaflet(airnow_conus)
#' }

airnow_load <- function(startdate="20170601",
                        enddate="20171231",
                        parameter='PM2.5',
                        baseUrl='https://haze.airfire.org/monitoring/AirNow/RData/') {
  
  # Sanity check
  validParams <- c("PM2.5")
  if ( !parameter %in% validParams ) {
    paramsString <- paste(validParams, collapse=", ")
    stop(paste0("Parameter '", parameter, "' is not supported. Try one of: ", paramsSstring))
  }
  
  # Only support a single year
  startdate <- as.character(startdate)
  enddate <- as.character(enddate)
  if ( stringr::str_sub(startdate,1,4) != stringr::str_sub(enddate,1,4) ) {
    stop("You may only load AirNow data from a single year.")
  }
  
  # Create a vector of yearMonths covering the time range
  yearMonths <- seq(lubridate::ymd(startdate, tz='UTC'), lubridate::ymd(enddate, tz='UTC'), by='month')
  
  # Loop over yearMonths and add each ws_monitor object to a list
  firstMonth <- TRUE
  for ( yearMonth in yearMonths ) {
    
    ym <- strftime(yearMonth, "%Y%m", tz="UTC")
    part1 <- strftime(yearMonth, "%Y/%m/airnow_", tz="UTC")
    part2 <- strftime(yearMonth, "_%Y_%m.RData", tz="UTC")
    filepath <- paste0(part1,parameter,part2)
    
    # Define a 'connection' object so we can be sure to close it
    conn <- url(paste0(baseUrl,filepath))
    mon <- get(load(conn))
    close(conn)
    
    if ( firstMonth ) {
      ws_monitor <- mon
      firstMonth <- FALSE
    } else {
      # NOTE:  The way we grow the ws_monitor object is an example of what NOT to do in R but
      # NOTE:  we are limited by the fact that monitor_join() can only join 2 monitors.
      ws_monitor <- monitor_combine(list(ws_monitor,mon))
    }

  }
  
  # Combine into a single ws_monitor object and return
  ws_monitor <- monitor_combine(monitorList)
  
  return(ws_monitor)
  
}
