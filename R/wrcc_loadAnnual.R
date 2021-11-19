#' @keywords WRCC
#'
#' @title Load annual WRCC monitoring data
#'
#' @description Loads pre-generated .RData files containing annual
#' WRCC data.
#'
#' If \code{dataDir} is defined, data will be loaded from this local
#' dirctory. Otherwise, data will be loaded from the monitoring data repository
#' maintained by PWFSL.
#'
#' The annual files loaded by this function are updated on the 15'th of each
#' month and cover the period from the beginning of the year to the end of the
#' last month.
#'
#' For data during the last 45 days, use \code{wrcc_loadDaily()}.
#'
#' For the most recent data, use \code{wrcc_loadLatest()}.
#'
#' WRCC parameters include the following:
#' \enumerate{
# #' \item{BARPR}
# #' \item{BC}
# #' \item{CO}
# #' \item{NO}
# #' \item{NO2}
# #' \item{NO2Y}
# #' \item{NO2X}
# #' \item{NOX}
# #' \item{NOOY}
# #' \item{OC}
# #' \item{OZONE}
# #' \item{PM10}
#' \item{PM2.5}
# #' \item{PRECIP}
# #' \item{RHUM}
# #' \item{SO2}
# #' \item{SRAD}
# #' \item{TEMP}
# #' \item{UV-AETH}
# #' \item{WD}
# #' \item{WS}
#' }
#'
#' Available WRCC RData and associated log files can be seen at:
#' \href{https://haze.airfire.org/monitoring/WRCC/RData}{https://haze.airfire.org/monitoring/WRCC/RData}
#' @seealso \code{\link{wrcc_loadDaily}}
#' @seealso \code{\link{wrcc_loadLatest}}
#'
#' @param year Desired year (integer or character representing YYYY).
#' @param parameter Parameter of interest.
#' @param baseUrl Base URL for 'annual' WRCC data files.
#' @param dataDir Local directory containing 'annual' data files.
#'
#' @return A \emph{ws_monitor} object with WRCC data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' wrcc_loadAnnual(2017) %>%
#'   monitor_subset(stateCodes='MT', tlim=c(20170701,20170930)) %>%
#'   monitor_dailyStatistic() %>%
#'   monitor_timeseriesPlot(style = 'gnats', ylim=c(0,300), xpd=NA)
#'   addAQIStackedBar()
#'   addAQILines()
#'   title("Montana 2017 -- WRCC Daily Average PM2.5")
#'
#' }, silent = FALSE)
#' }

wrcc_loadAnnual <- function(
  year = NULL,
  parameter = 'PM2.5',
  baseUrl = 'https://haze.airfire.org/monitoring',
  dataDir = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(year) ) {
    stop("Required parameter 'year' is missing.")
  }
  year <- as.numeric(year)

  validParams <- c("PM2.5")
  if ( !parameter %in% validParams ) {
    paramsString <- paste(validParams, collapse=", ")
    stop(paste0("'", parameter,
                "' is not a supported parameter. Use 'parameter = ",
                paramsString, "'"),
         call.=FALSE)
  }

  if ( year < 2010 ) {
    stop("PWFSL has no annual data files for WRCC prior to 2010.")
  }

  # Load the data --------------------------------------------------------------

  # Create URL and filename according to the PWFSLSmoke naming scheme
  baseUrl <- paste0(baseUrl, '/WRCC/RData/', year)
  filename <- paste0("wrcc_", parameter, "_", year, ".RData")

  ws_monitor <- MazamaCoreUtils::loadDataFile(filename, baseUrl, dataDir)

  # Return ---------------------------------------------------------------------

  return(ws_monitor)

}
