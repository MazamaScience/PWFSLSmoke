#' @keywords internal
#' @export
#' @title Load data from URL or local file
#' @param filename Name of the data file to be loaded.
#' @param baseUrl Base URL for data files.
#' @param dataDir Local directory containing data files.
#' @return A \emph{ws_monitor} object.
#' @description Loads pre-generated .RData files

loadDataFile <- function(filename = NULL,
                         baseUrl = 'https://haze.airfire.org/monitoring/latest/RData',
                         dataDir = NULL) {

  # Validate parameters

  if ( is.null(filename) ) {
    stop("Required parameter 'filename' is missing.")
  }

  # Load the data

  if ( is.null(dataDir) ) {

    # Load from a URL
    filepath <- paste0(baseUrl, '/', filename)
    # Define a 'connection' object so we can close it no matter what happens
    conn <- url(filepath)
    result <- try({
      suppressWarnings(ws_monitor <- get(load(conn)))
    }, silent=TRUE )
    close(conn)

  } else {

    # Load from a file
    filepath <- file.path(path.expand(dataDir), filename)
    result <- try({
      suppressWarnings(ws_monitor <- get(load(filepath)))
    }, silent = TRUE)

  }

  # NOTE:  We used suppressWarnings() above so that we can have a more
  # NOTE:  uniform error response for the large variety of reasons that
  # NOTE:  loading might fail.

  if ( "try-error" %in% class(result) ) {
    # TODO:  Restore logging when we stop generating "futile.logger" errors
    # TODO:  when logging has not been initialized.
    # # Log the error if logging is enabled. Fail silently otherwise.
    # try({ logger.error("%s", geterrmessage()) }, silent = TRUE)
    stop(paste0("Data file could not be loaded: ", filepath), call.=FALSE)
  }

  return(ws_monitor)
}
