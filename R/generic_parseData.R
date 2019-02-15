#' @title Parse generic files
#'
#' @description
#'
#' @param fileString
#' @param configList
#'
#' @return
#'
#' @details
#'
#' @importFrom magrittr %<>%
#' @export
#'
#' @examples
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

  # TODO: should we still convert to lowercase?

  if (is.null(names(configList))) {
    stop("The configuration list must be convertable to a named list. No names detected.")
  } else {
    names(configList) %<>% tolower()
  }


# Parse configuration list ------------------------------------------------

  ## Steps:
  #  - Check required parameters
  #  - Check column names
  #  - Check station metadata
  #  - Check parsing info
  #  - regularize data types


# * Check required parameters ---------------------------------------------

  reqParams <- c(
    "columnnames", "stationmeta", "parsinginfo"
  )

  if (!all(reqParams %in% names(configList))) {
    stop(paste0(
      "`configList` must contain entries for all of the following:\n",
      paste(reqParams, collapse = ", ")
    ))
  }


# * Check column names ----------------------------------------------------

  reqColNames <- c("datetime", "pm25")

  if (!all(reqColNames %in% names(configList[["columnnames"]]))) {
    stop(paste0(
      "`configList$columnnames` must contain entries for all of",
      "the following:\n",
      paste(reqColNames, collapse = ", ")
    ))
  }


# * Check station metadata ------------------------------------------------

  reqMeta <- c(
    "latitude", "longitude"
  )

  if (!all(reqMeta %in% names(configList[["stationmeta"]]))) {
    stop(paste0(
      "configList$stationmeta must contain entries for all of the following:\n",
      paste(reqMeta, collapse = ", ")
    ))
  }

  extraMeta <- c(
    "sitename"
  )

  includedExtraMeta <-
    extraMeta[extraMeta %in% names(configList[["stationmeta"]])]


# * Check parsing info ----------------------------------------------------

  reqParsing <- c(
    "headerRows", "columnTypes"
  )

  if (!all(reqParsing %in% names(configList[["parsinginfo"]]))) {
    stop(paste0(
      "configList$parsinginfo must contain entries for all of the following:\n",
      paste(reqMeta, collapse = ", ")
    ))
  }

  defaultParams <- list(
    tz = "UTC",
    decimalMark = ".",
    groupingMark = ",",
    delimiter = ",",
    encoding = "UTF-8"
  )

  configList[["parsinginfo"]] <-
    purrr::list_modify(defaultParams, !!!configList[["parsinginfo"]])


# * Regularize data types -------------------------------------------------

  configList[["parsinginfo"]][["headerRows"]] %<>% as.integer()
  configList[["stationmeta"]][["longitude"]] %<>% as.numeric()
  configList[["stationmeta"]][["latitude"]] %<>% as.numeric()


# Parse data --------------------------------------------------------------

  genericLocale <- readr::locale(
    decimal_mark = configList[["decimalMark"]],
    grouping_mark = configList[["groupingMark"]],
    tz = configList[["tz"]],
    encoding = configList[["encoding"]]
  )

  dataTbl <- readr::read_delim(
    fileString,
    configList[["delimiter"]],
    col_types = configList[["columnTypes"]],
    locale = genericLocale,
    skip = configList[["headerRows"]]
  )

  readr::stop_for_problems(dataTbl)


# Format column names -----------------------------------------------------

  selectedCols <- c(reqColNames, includedExtraMeta)

  # Standardize column names and append station metadata
  dataTbl <- dataTbl %>%
    rename(!!!configList[["columnnames"]]) %>%
    mutate(!!!configList[["stationmeta"]]) %>%
    select(selectedCols)


# Return parsed data ------------------------------------------------------

  return(dataTbl)

}
