generic_downloadData <- function(fileURL) {

  logger.info(
    paste0(
      "Reading file from location:\n",
      stringr::str_trunc(fileURL, 80, side = "right")
    )
  )

  fileString <- readr::read_file(fileURL)

  return(fileString)

}
