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
