#' @keywords AirNow
#' @keywords AIRSIS
#' @keywords WRCC
#' @export
#' @title Download annual PM2.5 monitoring data
#' @param year Desired year (integer or character representing YYYY).
#' @param parameter Parameter of interest.
#' @param baseUrl Base URL for data files.
#' @param dataDir Local directory in which to save the data file.
#' @param ... Additional arguments passed to \code{download.file}.
#' @description Downloads 'annual' data files into \code{dataDir} for later use.
#' Downloaded versions of PWFSL monitoring .RData files allow users to work with
#' the package without access to the internet. Once data are downloaded to
#' \code{dataDir}, any of the data loading functions can be called with the
#' \code{dataDir} argument to replace internet downloads with local file access.
#'
#' The recommended directory for PWFSL monitoring data is
#' \code{"~/data/monitoring/RData"}.
#'
#' For data during the last 45 days, use \code{monitor_downloadDaily()}.
#'
#' For the most recent data, use \code{monitor_downloadLatest()}.
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
#' \href{https://haze.airfire.org/monitoring/latest/RData/}{https://haze.airfire.org/monitoring/latest/RData/}
#' @seealso \code{\link{monitor_loadDaily}}
#' @examples
#' \donttest{
#' library(PWFSLSmoke)
#'
#' monitor_loadAnnual(2018) %>%
#'   monitor_subset(stateCodes = "WA", tlim = c(20180701, 20181001)) %>%
#'   monitor_timeseriesPlot(style = 'gnats')
#' }

monitor_downloadAnnual <- function(
  year = NULL,
  parameter='PM2.5',
  baseUrl='https://haze.airfire.org/monitoring',
  dataDir = "~/Data/monitoring/RData",
  ...
) {

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
  lastYear <- lubridate::now(tzone = "UTC") %>% lubridate::year() - 1

  # Rename baseUrl to avoid confusing R in the following function calls
  pwfslBase <- baseUrl

  # AirNow annual files start in 2016
  if ( as.numeric(year) >= firstAirnowYear ) {
    downloadDataFile(filename = paste0("airnow_", parameter, "_", year, ".RData"),
                     baseUrl = paste0(pwfslBase, '/AirNow/RData/', year),
                     dataDir,
                     ...)
  }

  # AIRSIS annual files start in 2004
  if ( as.numeric(year >= firstAirsisYear) ) {
    downloadDataFile(filename = paste0("airsis_", parameter, "_", year, ".RData"),
                     baseUrl = paste0(pwfslBase, '/AIRSIS/RData/', year),
                     dataDir,
                     ...)
  }

  # WRCC annual files start in 2010
  if ( as.numeric(year >= firstWrccYear) ) {
    downloadDataFile(filename = paste0("wrcc_", parameter, "_", year, ".RData"),
                     baseUrl = paste0(pwfslBase, '/WRCC/RData/', year),
                     dataDir,
                     ...)
  }

  # EPA PM2.5 data are split into two parameter codes
  if ( parameter == 'PM2.5' ) {

    # EPA 88101 annual files start in 2008
    if ( year %in% firstEpa88101Year:lastYear ) {
        downloadDataFile(filename = paste0("epa_PM2.5_88101_", year, ".RData"),
                         baseUrl = paste0(pwfslBase, '/EPA/RData/', year),
                         dataDir,
                         ...)
    }

    # WPA 88502 annual files start in 1998
    if ( year %in% firstEpa88502Year:lastYear ) {
      downloadDataFile(filename = paste0("epa_PM2.5_88502_", year, ".RData"),
                       baseUrl = paste0(pwfslBase, '/EPA/RData/', year),
                       dataDir,
                       ...)
    }

  }

  return(invisible(NULL))

}
