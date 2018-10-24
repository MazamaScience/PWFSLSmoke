#' @keywords internal
#' @importFrom utils download.file
#' @export
#' @title Download data from URL to local diredctory
#' @param filename Name of the data file to be loaded.
#' @param baseUrl Base URL for data files.
#' @param dataDir Local directory in which to save the data file.
#' @param ... Additional arguments passed to \code{download.file}.
#' @return A \emph{ws_monitor} object.
#' @description Downloads a data file into \code{dataDir} for later use.
#' Downloaded versions of PWFSL monitoring .RData files allow users to work with
#' the package without access to the internet. Once data are downloaded to
#' \code{dataDir}, any of the data loading functions can be called with the
#' \code{dataDir} argument to replace internet downloads with local file access.
#'
#' The recommended directory for PWFSL monitoring data is
#' \code{"~/data/monitoring/RData"}.

downloadDataFile <- function(filename = NULL,
                             baseUrl = NULL,
                             dataDir = "~/data/monitoring/RData",
                             ...) {

  # Validate parameters --------------------------------------------------------

  if ( is.null(filename) ) {
    stop("Required parameter 'filename' is missing.")
  }

  if ( is.null(baseUrl) ) {
    stop("Required parameter 'baseUrl' is missing.")
  }

  if ( !file.exists(dataDir) ) {
    dir.create(dataDir, recursive = TRUE)
  }

  # Download the data ----------------------------------------------------------

  download.file(url = paste0(baseUrl, '/', filename),
                destfile = file.path(dataDir, filename),
                ...)

}
