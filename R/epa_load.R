#' @keywords EPA
#' @export
#' @title Load Processed EPA Monitoring Data
#' @param year desired year (integer or character representing YYYY)
#' @param parameterCode pollutant code
#' @param baseUrl base URL for EPA .RData files
#' @return A \emph{ws_monitor} object with EPA data for an entire year.
#' @description Loads a pre-generated .RData file containing a year's worth of
#' monitoring data.
#' 
#' EPA parameter codes include:
#' \enumerate{
# #' \item{44201}{ -- Ozone}
# #' \item{42401}{ -- SO2}
# #' \item{42101}{ -- CO}
# #' \item{42602}{ -- NO2}
#' \item{88101}{ -- PM2.5 FRM/FEM Mass (begins in 2008)}
#' \item{88502}{ -- PM2.5 non FRM/FEM Mass (begins in 1998)}
# #' \item{81102}{ -- PM10 Mass}
# #' \item{SPEC}{ -- PM2.5 Speciation}
# #' \item{WIND}{ -- Wind}
# #' \item{TEMP}{ -- Temperature}
# #' \item{PRESS}{ -- Barometric Pressure}
# #' \item{RH_DP}{ -- RH and dewpoint}
# #' \item{HAPS}{ -- HAPs}
# #' \item{VOCS}{ -- VOCs}
# #' \item{NONOxNOy}
#' }
#'
#' Avaialble RData and associated log files can be seen at:
#' \href{https://haze.airfire.org/monitoring/EPA/RData/}{https://haze.airfire.org/monitoring/EPA/RData/}
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
#' @examples
#' \dontrun{
#' epa_frm <- epa_load(2015, 88101)
#' epa_frm_conus <- monitor_subset(epa_frm, stateCodes=CONUS)
#' monitorLeaflet(epa_frm_conus)
#' }

epa_load <- function(year=strftime(lubridate::now(),"%Y",tz="UTC"),
                     parameterCode='88101',
                     baseUrl='https://haze.airfire.org/monitoring/EPA/RData/') {
  
  # Sanity Check -- validate parameter code
  validParameterCodes <- c("44201", "42401", "42101", "42602", "88101", "88502", "81102", "SPEC",
                           "WIND", "TEMP", "PRESS", "RH_DP", "HAPS", "VOCS", "NONOxNOy")
  
  if ( is.null(parameterCode) ) {
    stop("Required parameter 'parameterCode' is missing")
  } else {
    parameterCode <- as.character(parameterCode)
    if ( !parameterCode %in% validParameterCodes ) {
      stop(paste0("parameterCode '",parameterCode,"' is not in: ", paste0(validParameterCodes, collapse=", ")))
    }
  }
  
  # Sanity check: year is supplied and valid
  if ( is.null(year) ) {
    stop(paste0("Required parameter 'year' is missing"))
  } else if ( year < 1990 ) {
    stop(paste0("No data available before 1990"))
  } else if ( (parameterCode=="88101" && year<2008) ||
              (parameterCode=="88502" && year<1998) || 
              (parameterCode=="SPEC" && year<2001) ||
              (parameterCode=="HAPS" && year<1993) ) {
    stop(sprintf("No data available for parameter code %s in year %i", parameterCode, year))
  }
  
  # Create a filename based on the parameter code
  if ( parameterCode %in% c('88101','88502') ) {
    filepath <- paste0(year,'/epa_PM2.5_',parameterCode,'_',year,'.RData')
  } else {
    stop(paste0("PWFSL has not yet processed data for parameterCode '",parameterCode,"'"))
  }

  
  # Define a 'connection' object so we can be sure to close it no matter what happens
  conn <- url(paste0(baseUrl,filepath))
  result <- try( suppressWarnings(ws_monitor <- get(load(conn))),
                 silent=TRUE )
  close(conn)
  
  if ( "try-error" %in% class(result) ) {
    stop(paste0("No EPA data available for ",year), call.=FALSE)
  }
  
  
  return(ws_monitor)
  
}
