#' @title Download generic data
#'
#' @description
#' This function takes a location to a delimited file, gets the file, and
#' returns a string containing the file data.
#'
#' @param filePath Either a path to a file, or a connection (\code{http(s)://},
#'   \code{ftp(s)://}).
#'
#' @return A character vector of length 1, containing data from the file located
#'   at \code{filePath}.
#'
#' @details
#' This function is essentailly a wrapper for \code{\link[readr]{read_file}}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # make current directory PWFSLSmoke package directory
#' filePath <- "./localData/airsis_ebam_example-clean.csv"
#'
#' fileString <- generic_downloadData(filePath)
#' }
generic_downloadData <- function(filePath) {

  logger.info(
    paste0(
      "Reading file from location:\n",
      stringr::str_trunc(filePath, 80, side = "right")
    )
  )

  fileString <- readr::read_file(filePath)

  return(fileString)

}
