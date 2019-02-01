generic_parseData <- function(fileString = NULL,
                              configurationList = NULL) {

# Validate input ----------------------------------------------------------

  # Make sure fileString is a string (one element character vector)
  if (
    is.null(fileString) ||
    (!is.character(fileString) && length(fileString) > 1)
  ) {
    stop("`fileString` must be a character vector of length one.")
  }


  # If `configurationList` is a string, make sure it's valid JSON, then
  # convert to a list.
  if (is.character(configurationList)) {
    if (jsonlite::validate(configurationList)) {
      configurationList <- as.list(jsonlite::fromJSON(
        configurationList,
        simplifyVector = TRUE,
        simplifyDataFrame = FALSE,
        simplifyMatrix = FALSE,
        flatten = FALSE
      ))
    } else if (!is.list(configurationList)) {
      stop("`configurationList` must be either a valid JSON string, or an R list.")
    }
  }

  if (is.null(names(configurationList))) {
    stop("The configuration list must be convertable to a named list. No names detected.")
  } else {
    names(configurationList) <- tolower(names(configurationList))
  }


# Parse configuration list ------------------------------------------------

  # Make sure the configuration list contains all the required information
  requiredParams <- c(
    "headerRows", "columnTypes", "requiredColumnNames"
  )

  metaParams <- c(
    "monitorID", "latitude", "longitude", "timezone"
  )

  # Find the set of meta parameters that don't exist at the top level of
  # `configurationlist`, and make sure they exist in
  # `configurationlist$extraColumnNames`.




  # convert to appropriate data types
  configurationList["headerRows"] <- as.integer(configurationList[["headerRows"]])



  if (!all(requiredParams %in% names(configurationList))) {
    stop(paste0(
      "`configurationList` must contain entries for all of the following:\n",
      paste(requiredParams, collapse = ", ")
    ))
  }


# Parse data --------------------------------------------------------------

  # Read string as individual lines, skipping the header rows
  dataLines <- readr::read_csv(
    fileString,
    skip = as.integer(configurationList$header_rows)
  )

  ## TODO: additional formatting based on monitor/source type?


# Check for parsing problems ----------------------------------------------

  readr::stop_for_problems(tbl)


# Return parsed data ------------------------------------------------------

  return(tbl)

}
