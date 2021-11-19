#' @keywords EPA
#' @export
#' @title Load annual EPA monitoring data
#' @param year Desired year (integer or character representing YYYY).
#' @param parameterCode Pollutant code.
#' @param baseUrl Base URL for 'annual' EPA data files.
#' @param dataDir Local directory containing 'annual' data files.
#' @return A \emph{ws_monitor} object with EPA data.
#' @description Loads pre-generated .RData files containing annual
#' EPA data.
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
#' # Fail gracefully if any resources are not available
#' try({
#'
#' epa_loadAnnual(2000, "88502") %>%
#'   monitor_subset(stateCodes = 'WA', tlim=c(20000701,20000801)) %>%
#'   monitor_map()
#'
#' }, silent = FALSE)
#' }

epa_loadAnnual <- function(
  year = NULL,
  parameterCode = NULL,
  baseUrl = 'https://haze.airfire.org/monitoring',
  dataDir = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  # Sanity Check -- validate parameter code
  # validParameterCodes <- c("44201", "42401", "42101", "42602", "88101", "88502",
  #                          "81102", "SPEC", "WIND", "TEMP", "PRESS", "RH_DP",
  #                          "HAPS", "VOCS", "NONOxNOy")
  validParameterCodes <- c("88101", "88502")

  if ( is.null(parameterCode) ) {
    stop("Required parameter 'parameterCode' is missing")
  } else {
    parameterCode <- as.character(parameterCode)
    if ( !parameterCode %in% validParameterCodes ) {
      stop(paste0("PWFSL has not yet processed EPA data for parameterCode '",
                  parameterCode, "'"))
    }
  }

  # Sanity check: year is supplied and valid
  if ( is.null(year) ) {
    stop(paste0("Required parameter 'year' is missing"))
  }
  year <- as.numeric(year)

  lastYear <- lubridate::now(tzone = "UTC") %>% lubridate::year() - 1

  if ( parameterCode == "88101" ) {
    parameter <- "PM2.5"
    if ( ! year %in% 2008:lastYear) {
      stop(sprintf("No EPA data available for parameter code %s in year %i",
                   parameterCode, year))
    }
  } else if  ( parameterCode == "88502" ) {
    parameter <- "PM2.5"
    if ( ! year %in% 1998:lastYear) {
      stop(sprintf("No EPA data available for parameter code %s in year %i",
                   parameterCode, year))
    }
  }

  # Load data ------------------------------------------------------------------

  # Create URL and filename according to the PWFSLSmoke naming scheme
  baseUrl <- paste0(baseUrl, '/EPA/RData/', year)
  filename <- paste0("epa_", parameter, "_", parameterCode, '_', year, ".RData")

  ws_monitor <- MazamaCoreUtils::loadDataFile(filename, baseUrl, dataDir)

  return(ws_monitor)

}
