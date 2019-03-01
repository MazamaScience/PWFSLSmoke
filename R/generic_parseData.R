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
#' @return
#'
#' @section Configuration List Structure:
#' \code{configList} can be either an \code{R} \code{list} or a JSON file, but
#' both must follow the same nested key-value format:
#'
#' \code{
#' configList
#'  |_ column_names
#'  |   |_ datetime = [string],
#'  |   |_ pm25 = [string]
#'  |
#'  |_ station_meta
#'  |   |_ latitude = [numeric],
#'  |   |_ longitude = [numeric],
#'  |   |_ site_name = [optional | string]
#'  |
#'  |_ parsing_info
#'      |_ header_rows = [integer]
#'      |_ column_types = [string],
#'      |_ tz = [optional | string | default: "UTC"],
#'      |_ decimal_mark = [optional | string | default: "."],
#'      |_ grouping_mark = [optional | string | default: ","],
#'      |_ delimiter = [optional | string | default: ","],
#'      |_ encoding = [optional | string | default: "UTF-8"]
#' }
#'
#' @section Configuration List Details:
#' Required parameters are the top level keys of the configuration list.
#' Each parameter alters a specific aspect of parsing:
#'
#'  * "column_names" contains entries for mapping column names from the input
#'    file to `ws_monitor` columns names needed to built a `ws_monitor` object.
#'
#'  * "station_meta" contains entries for metadata about the station that
#'    generated the input file, which allows `PWFSLSmoke` to augment the
#'    `ws_monitor` object later in the pipeline. Some entries are required,
#'    others are optional.
#'
#'  * "parsing_info"
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

  if (!all(reqMeta %in% names(configList[["statio_nmeta"]]))) {
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

  configList[["parsing_info"]][["header_rows"]] %<>% as.integer()
  configList[["station_meta"]][["longitude"]] %<>% as.numeric()
  configList[["station_meta"]][["latitude"]] %<>% as.numeric()


# Parse data --------------------------------------------------------------

  genericLocale <- readr::locale(
    decimal_mark = configList[["decimal_mark"]],
    grouping_mark = configList[["grouping_mark"]],
    tz = configList[["tz"]],
    encoding = configList[["encoding"]]
  )

  dataTbl <- readr::read_delim(
    fileString,
    configList[["delimiter"]],
    col_types = configList[["column_types"]],
    locale = genericLocale,
    skip = configList[["header_rows"]]
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

  # Standardize column names and append station metadata
  dataTbl <- dataTbl %>%
    rename(!!!configList[["column_names"]]) %>%
    mutate(!!!configList[["station_meta"]]) %>%
    rename(!!!camelCaseMeta) %>%
    select(selectedCols)


# Return parsed data ------------------------------------------------------

  return(dataTbl)

}
