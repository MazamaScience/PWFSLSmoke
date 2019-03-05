#' @title Parse generic air quality files
#'
#' @description
#' Given a string of delimited file data, this function will parse the file as
#' a table of data an apply some transformations and augmentations as specified
#' by a given configuration list.
#'
#' @param fileString Character string of delimited data to parse.
#' @param configList A \code{R} \code{list} or JSON file containing key-value
#'   pairs which affect how the parsing of \code{fileString} is handled. If
#'   \code{configList} is in JSON format, it can be passed in as a file, string,
#'   or URL.
#'
#' @return A tibble of the data contined in \code{fileString} parsed according
#'   to parameters in \code{configList}. The data is coerced into a format that
#'   is more easily convertable into a \code{ws_monitor} object at a later
#'   point.
#'
#' @section Parsing data:
#' Internally, this function uses \code{\link[readr]{read_delim}} to convert
#' \code{fileString} into a tibble. If any lines of data cannot be properly
#' parsed, and error will be thrown anf the problem lines will be printed.
#'
#' @section Creating a configList:
#' For more information on how to build a \code{configList}, see the Rmarkdown
#' document "Working with Generic Data" in the \code{localNotebooks} directory.
#'
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

  ## NOTE on `magrittr::%<>%`:
  #  This pipe (used like `rhs %<>% lhs`) is equivalent to the more common pipe
  #  `lhs <- lhs %>% rhs`.
  #  See "?magrittr::`%<>%`" for more information.

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
    "column_names", "station_meta", "parsing_info"
  )

  if (!all(reqParams %in% names(configList))) {
    stop(paste0(
      "`configList` must contain entries for all of the following:\n",
      paste(reqParams, collapse = ", ")
    ))
  }


# * Check column names ----------------------------------------------------

  reqColNames <- c("datetime", "pm25")

  if (!all(reqColNames %in% names(configList[["column_names"]]))) {
    stop(paste0(
      "`configList$column_names` must contain entries for all of",
      "the following:\n",
      paste(reqColNames, collapse = ", ")
    ))
  }


# * Check station metadata ------------------------------------------------

  reqMeta <- c(
    "latitude", "longitude"
  )

  if (!all(reqMeta %in% names(configList[["station_meta"]]))) {
    stop(paste0(
      "configList$station_meta must contain entries for all of the following:\n",
      paste(reqMeta, collapse = ", ")
    ))
  }

  extraMeta <- c(
    "site_name"
  )

  includedExtraMeta <-
    extraMeta[extraMeta %in% names(configList[["station_meta"]])]


# * Check parsing info ----------------------------------------------------

  reqParsing <- c(
    "header_rows", "column_types"
  )

  if (!all(reqParsing %in% names(configList[["parsing_info"]]))) {
    stop(paste0(
      "configList$parsing_info must contain entries for all of the following:\n",
      paste(reqMeta, collapse = ", ")
    ))
  }

  defaultParams <- list(
    tz = "UTC",
    decimal_mark = ".",
    grouping_mark = ",",
    delimiter = ",",
    encoding = "UTF-8"
  )

  configList[["parsing_info"]] <-
    purrr::list_modify(defaultParams, !!!configList[["parsing_info"]])


# * Regularize data types -------------------------------------------------

  ## NOTE on `magrittr::%<>%`:
  #  This pipe (used like `rhs %<>% lhs`) is equivalent to the more common pipe
  #  `lhs <- lhs %>% rhs`.
  #  See "?magrittr::`%<>%`" for more information.

  configList[["parsing_info"]][["header_rows"]] %<>% as.integer()
  configList[["station_meta"]][["longitude"]] %<>% as.numeric()
  configList[["station_meta"]][["latitude"]] %<>% as.numeric()


# Parse data --------------------------------------------------------------

  genericLocale <- readr::locale(
    decimal_mark = configList[["parsing_info"]][["decimal_mark"]],
    grouping_mark = configList[["parsing_info"]][["grouping_mark"]],
    tz = configList[["parsing_info"]][["tz"]],
    encoding = configList[["parsing_info"]][["encoding"]]
  )

  dataTbl <- readr::read_delim(
    fileString,
    configList[["parsing_info"]][["delimiter"]],
    col_types = configList[["parsing_info"]][["column_types"]],
    locale = genericLocale,
    skip = configList[["parsing_info"]][["header_rows"]]
  )

  readr::stop_for_problems(dataTbl)


# Format column names -----------------------------------------------------

  selectedCols <- c(reqColNames, reqMeta, includedExtraMeta)

  # Create mapping for potential extra station metadata key names from
  # snake_case to camelCase
  camelCaseMeta <- includedExtraMeta %>%
    stringr::str_split("_") %>%
    purrr::map(stringr::str_to_title) %>%
    purrr::map(stringr::str_flatten) %>%
    purrr::map(~stringr::str_replace(.x, "^[A-Z]", stringr::str_to_lower)) %>%
    purrr::set_names(includedExtraMeta, .)


  ## NOTE on `!!!`:
  #  Within the "tidyverse" "tidyeval" framework, `!!!` is way to unquote
  #  multiple arguments in a list, and splice each argument individually into a
  #  function.
  #
  #  In this case, we have a lists containing containing multiple columns we
  #  want to rename and add to our output. Prefixing the lists with `!!!` means
  #  each element of the list will be unquoted, and added as an argument to
  #  `rename` and `mutate`.
  #
  #  For more information, see:
  #  dplyr.tidyverse.org/articles/programming.html#unquote-splicing
  #  https://tidyeval.tidyverse.org/multiple.html#quote-multiple-arguments

  # Standardize column names and append station metadata
  dataTbl <- dataTbl %>%
    rename(!!!configList[["column_names"]]) %>%
    mutate(!!!configList[["station_meta"]]) %>%
    rename(!!!camelCaseMeta) %>%
    select(selectedCols)


# Return parsed data ------------------------------------------------------

  return(dataTbl)

}
