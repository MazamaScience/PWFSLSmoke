#' @keywords AirNow
#' @keywords AIRSIS
#' @keywords WRCC
#' @export
#' @title Load annual PM2.5 monitoring data
#' @param year Desired year (integer or character representing YYYY).
#' @param parameter Parameter of interest.
#' @param baseUrl Base URL for data files.
#' @param dataDir Local directory containing 'daily' data files.
#' @param aqsPreference Preferred data source for AQS data when annual data
#' files are available from both `epa` and `airnow`.
#' @return A \emph{ws_monitor} object with PM2.5 monitoring data.
#' @description Wrapper function to load and combine annual
#' data from AirNow, AIRSIS and WRCC.
#'
#' If \code{dataDir} is defined, data will be loaded from this local
#' dirctory. Otherwise, data will be loaded from the monitoring data repository
#' maintained by PWFSL.
#'
#' The annual files loaded by this function are updated on the 15'th of each
#' month and cover the period from the beginning of the year to the end of the
#' last month.
#'
#' For data during the last 45 days, use \code{monitor_loadDaily()}.
#'
#' For the most recent data, use \code{monitor_loadLatest()}.
#'
#' Currently supported parameters include the following:
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
#' Avaialble RData files can be seen at:
#' \href{https://haze.airfire.org/monitoring/}{https://haze.airfire.org/monitoring/}
#' @seealso \code{\link{monitor_loadDaily}}
#' @seealso \code{\link{monitor_loadLatest}}
#' @examples
#' \dontrun{
#' monitor_loadAnnual(2014) %>%
#'   monitor_subset(stateCodes='MT', tlim=c(20140801,20140901)) %>%
#'   monitor_map()
#' }

monitor_loadAnnual <- function(year = NULL,
                               parameter='PM2.5',
                               baseUrl='https://haze.airfire.org/monitoring',
                               dataDir = NULL,
                               aqsPreference = "airnow") {

  # Validate parameters --------------------------------------------------------

  if ( is.null(year) ) {
    stop("Required parameter 'year' is missing.")
  } else if ( year < 1998 ) {
    stop("PWFSL has processed no data prior to 1998.")
  }
  year <- as.numeric(year)

  # Load data ------------------------------------------------------------------

  # Cutoff years
  firstAirnowYear <- 2016
  firstAirsisYear <- 2004
  firstWrccYear <- 2010
  firstEpa88101Year <- 2008
  firstEpa88502Year <- 1998
  lastYear <- lubridate::now() %>% lubridate::year() - 1

  # Only add to the monitorList if data are available
  monitorList <- list()

  # AirNow annual files start in 2016
  if ( as.numeric(year) >= firstAirnowYear ) {
    if ( aqsPreference == 'airnow' || year > lastYear ) {
      monitorList$airnow <- airnow_loadAnnual(year, parameter, baseUrl, dataDir)
    }
  }

  # AIRSIS annual files start in 2004
  if ( as.numeric(year >= firstAirsisYear) ) {
    monitorList$airsis <- airsis_loadAnnual(year, parameter, baseUrl, dataDir)
  }

  # WRCC annual files start in 2010
  if ( as.numeric(year >= firstWrccYear) ) {
    monitorList$wrcc <- wrcc_loadAnnual(year, parameter, baseUrl, dataDir)
  }

  # NOTE:  EPA 88101 and 88502 share monitor IDs and usually need to go
  # NOTE:  a monitor_join() step. Because of the fact that monitor_join()
  # NOTE:  can only handle two monitors at a time, we combine the two epa
  # NOTE:  ws_monitor objects by themselves before combining them with other
  # NOTE:  ws_monitor objects that all use different monitorIDs.

  # TODO:  The epa_PM2.5_88502_2014.RData file is missing

  # Assemble EPA ws_monitor object ---------------------------------------------
  epaList <- list()
  if ( year %in% firstEpa88101Year:lastYear ) {
    if ( aqsPreference == "epa" || year < firstAirnowYear ) {
      result <- try({
        epaList$epa_88101 <- epa_loadAnnual(year, "88101", baseUrl, dataDir)
      }, silent = FALSE)
    }
  }
  if ( year %in% firstEpa88502Year:lastYear ) {
    if ( aqsPreference == "epa" || year < firstAirnowYear ) {
      result <- try({
        epaList$epa_88502 <- epa_loadAnnual(year, "88502", baseUrl, dataDir)
      }, silent = FALSE)
    }
  }
  if ( length(epaList) == 2 ) {
    monitorList$epa <- monitor_combine(epaList)
  } else if  (length(epaList) == 1 ) {
    monitorList$epa <- epaList[[1]]
  }

  # Final combination
  ws_monitor <- monitor_combine(monitorList)

  return(ws_monitor)

}
