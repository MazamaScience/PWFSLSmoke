generic_parseData <- function(fileString = NULL,
                              configList = NULL) {

# Validate input ----------------------------------------------------------

  # Make sure fileString is a string (one element character vector)
  if (
    is.null(fileString) ||
    (!is.character(fileString) && length(fileString) > 1)
  ) {
    stop("`fileString` must be a character vector of length one.")
  }

  # If `configList` is a string, make sure it's valid JSON, then
  # convert to a list.
  if (is.character(configList)) {
    if (jsonlite::validate(configList)) {
      configList <- as.list(jsonlite::fromJSON(
        configList,
        simplifyVector = TRUE,
        simplifyDataFrame = FALSE,
        simplifyMatrix = FALSE,
        flatten = FALSE
      ))
    } else if (!is.list(configList)) {
      stop("`configList` must be either a valid JSON string, or an R list.")
    }
  }

  if (is.null(names(configList))) {
    stop("The configuration list must be convertable to a named list. No names detected.")
  } else {
    names(configList) <- tolower(names(configList))
  }


# Parse configuration list ------------------------------------------------

  ## Steps:
  #  - check required parameters
  #  - check required column names
  #  - check required meta parameters
  #  - add missing defaults
  #  - regularize data types


# * Check if all required parameters are present --------------------------

  reqParams <- c(
    "headerRows", "columnTypes", "requiredColumnNames"
  )

  if (!all(reqParams %in% names(configList))) {
    stop(paste0(
      "`configList` must contain entries for all of the following:\n",
      paste(reqParams, collapse = ", ")
    ))
  }


# * Check if required column names are present ----------------------------

  reqColNames <- c("datetime", "pm25")

  if (!all(reqColNames %in% names(configList[["requiredColumnNames"]]))) {
    stop(paste0(
      "`configList$requiredColumnNames` must contain entries for all of",
      "the following:\n",
      paste(reqColNames, collapse = ", ")
    ))
  }


# * Check if meta parameters are present ----------------------------------

  # Find the set of required meta parameters that don't exist at the top level
  # of `configList`, and make sure they exist in
  # `configList$extraColumnNames`

  reqMetaParams <- c(
    "monitorID", "latitude", "longitude", "timezone"
  )

  metaInGlobal <- reqMetaParams %in% names(configList)
  metaGlobal <- reqMetaParams[metaInGlobal]
  metaExtra <- reqMetaParams[!metaInGlobal]

  if (!all(metaExtra %in% names(configList[["extraColumnNames"]]))) {
    stop(paste0(
      "The following parameters must either be specified at the top",
      "level of the configuration list, or in a 'extraColumnNames' sublist:\n",
      paste(reqMetaParams, collapse = ", ")
    ))
  }

  # Remove meta parameters in `configList$extraColumnNames` that also
  # exist at the top level of `configList` (ie the top level has priority)

  toKeep <- !(names(configList[["extraColumnNames"]]) %in% metaGlobal)
  configList[["extraColumnNames"]] <-
    configList[["extraColumnNames"]][toKeep]


# * Add defaults ----------------------------------------------------------

  defaultParams <- list(
    timezone = "UTC",
    decimalMark = ".",
    groupingMark = ",",
    delimiter = ",",
    encoding = "UTF-8"
  )

  configList <- purrr::list_modify(defaultParams, !!!configList)


# * Regularize data types -------------------------------------------------

  configList["headerRows"] <- as.integer(configList[["headerRows"]])


# Parse data --------------------------------------------------------------

  genericLocale <- readr::locale(
    decimal_mark = configList[["decimalMark"]],
    grouping_mark = configList[["groupingMark"]],
    tz = configList[["timezone"]],
    encoding = configList[["encoding"]]
  )

  dataTbl <- readr::delim(
    fileString,
    configList[["delimiter"]],
    col_types = configList[["columnTypes"]],
    locale = genericLocale,
    skip = configList[["headerRows"]]
  )

  readr::stop_for_problems(dataTbl)


# Format column names -----------------------------------------------------

  # TODO: How to capture global (top level) metadata that isn't required?

  # Standardize column names and append data given at the top level of
  # `configList`
  dataTbl <- dataTbl %>%
    rename(!!!configList[["requiredColumnNames"]]) %>%
    rename(!!!configList[["extraColumnNames"]]) %>%
    mutate(!!!configList[metaGlobal])


# Return parsed data ------------------------------------------------------

  return(dataTbl)

}
