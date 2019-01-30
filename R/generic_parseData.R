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


  # Make sure the configuration list contains all the required information.
  requiredParams <- c(
    "column_names", "column_types", "monitor_type", "header_rows"
  )

  names(configurationList) <- tolower(names(configurationList))

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
