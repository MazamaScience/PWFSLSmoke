#' @keywords AIRSIS
#' @export
#' @import MazamaCoreUtils
#'
#' @title Parse AIRSIS data string
#'
#' @param fileString character string containing AIRSIS data as a csv
#' @description Raw character data from AIRSIS are parsed into a tibble.
#' The incoming \code{fileString}
#' can be read in directly from AIRSIS using \code{airsis_downloadData()} or from a local
#' file using \code{readr::read_file()}.
#'
#' The type of monitor represented by this fileString is inferred from the column names
#' using \code{airsis_identifyMonitorType()} and appropriate column types are assigned.
#' The character data are then read into a tibble and augmented in the following ways:
#' \enumerate{
#' \item{Longitude, Latitude and any System Voltage values, which are only present in GPS timestamp rows, are
#' propagated foward using a last-observation-carry-forward algorithm'}
#' \item{Longitude, Latitude and any System Voltage values, which are only present in GPS timestamp rows, are
#' propagated backwords using a first-observation-carry-backward algorithm'}
#' \item{GPS timestamp rows are removed'}
#' }
#' @return Dataframe of AIRSIS raw monitor data.
#' @references \href{http://usfs.airsis.com}{Interagency Real Time Smoke Monitoring}
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' library(MazamaWebUtils)
#' fileString <- airsis_downloadData(20150701, 20151231, provider='USFS', unitID='1026')
#' tbl <- airsis_parseData(fileString)
#' }

airsis_parseData <- function(fileString) {

  logger.debug(" ----- airsis_parseData() ----- ")

  # Identify monitor type
  monitorTypeList <- airsis_identifyMonitorType(fileString)

  monitorType <- monitorTypeList$monitorType
  monitorSubtype <- monitorTypeList$monitorSubtype
  rawNames <- monitorTypeList$rawNames
  columnNames <- monitorTypeList$columnNames
  columnTypes <- monitorTypeList$columnTypes

  # Convert the fileString into individual lines
  lines <- readr::read_lines(fileString)

  if ( length(lines) == 1 ) {
    logger.warn("No valid PM2.5 data")
    stop(paste0("No valid PM2.5 data"))
  }

  if ( monitorType == "BAM1020" ) {

    logger.trace("Parsing BAM1020 data ...")

  } else if ( monitorType == "EBAM" ) {

    if ( monitorSubtype == "MULTI" ) {
      logger.trace("Parsing EBAM-Multi data ...")
    } else if ( monitorSubtype == "MULTI2" ) {
      logger.trace("Parsing EBAM-Multi2 data ...")
    } else {
      logger.trace("Parsing EBAM data ...")
    }

  } else if ( monitorType == "ESAM" ) {

    logger.trace("Parsing E-Sampler data ...")

    # NOTE:  Some E-Sampler files from AIRSIS (USFS 1050) have internal rows messed up with header line information
    # NOTE:  We need to remove these first. It seems they can be identified by searching for '%'.
    # NOTE:  Of course, we have to retain the first header line.

    internalHeaderMask <- stringr::str_detect(lines,'%')
    internalHeaderMask[1] <- FALSE
    if ( sum(internalHeaderMask) > 0 ) {
      logger.trace("Removing %d 'internal header line' records from raw data", sum(internalHeaderMask))
      lines <- lines[!internalHeaderMask]
    }

  } else if ( monitorType == "OTHER_1" ) {

    logger.error("Older EBAM 1 file parsing is not supported")
    logger.error("Header line:\n\t%s", paste0(rawNames,collapse=','))
    stop(paste0("Older EBAM 1 file parsing is not supported", call.=FALSE))

  } else if ( monitorType == "OTHER_2" ) {

    logger.error("Older EBAM 2 file parsing is not supported")
    logger.error("Header line:\n\t%s", paste0(rawNames,collapse=','))
    stop(paste0("Older EBAM 2 file parsing is not supported", call.=FALSE))

  } else {

    logger.error("Unknown file parsing is not supported")
    logger.error("Header line:\n\t%s", paste0(rawNames,collapse=','))
    stop(paste0("Unknown file parsing is not supported", call.=FALSE))

  }


  #     Parse the file     ----------------------------------------------------

  # Remove header line, leaving only data
  # NOTE:  We need to guarantee that fakeFile always has a newline so that read_lines will interpret
  # NOTE:  a single data record as literal data and now a path.
  fakeFile <- paste0(paste0(lines[-1], collapse='\n'),'\n')

  tbl <- suppressWarnings( readr::read_csv(fakeFile, col_names=columnNames, col_types=columnTypes) )

  # Print out any problems encountered by readr::read_csv
  problemsDF <- readr::problems(tbl)
  if ( dim(problemsDF)[1] > 0 ) {
    logger.trace("Records skipped with parsing errors:")
    problems <- utils::capture.output(format(problemsDF))
    for (i in seq_along(problems)) {
      logger.trace("%s",problems[i])
    }
  }

  # Add monitor name and type
  tbl$monitorName <- tbl$Alias
  tbl$monitorType <- monitorType

  # Add monitor subtype for EBAM MULTI & MULTI2 seperation QC
  tbl$monitorSubtype <- monitorSubtype

  #     EBAM-Multi fixes     --------------------------------------------------

  if ( monitorType == "EBAM" && (monitorSubtype == "MULTI" || monitorSubtype == "MULTI2") ) {

    # HACK
    # arb2 UnitID=1044 in August, 2018 does not return a "Date.Time.GMT" column
    # We add one here by flooring the "TimeStamp" colum.

    logger.trace("Adding Date.Time.GMT column to EBAM-Multi data.")
    if ( !"Date.Time.GMT" %in% names(tbl) && "TimeStamp" %in% names(tbl) ) {
      # Remove rows where TimeStamp is NA
      badMask <- is.na(tbl$TimeStamp) | tbl$TimeStamp == "NA"
      tbl <- tbl[!badMask,]
      datetime <- lubridate::mdy_hms(tbl$TimeStamp, tz="UTC")
      assignedHour <- lubridate::floor_date(datetime, unit = "hour")
      tbl$Date.Time.GMT <- strftime(assignedHour, "%m/%d/%Y %H:%M:%S", tz='UTC')
    }

    # Add "Sys..Volts" column
    if ( !"Sys..Volts" %in% names(tbl) && "Oceaneering.Unit.Voltage" %in% names(tbl) ) {
      tbl$Sys..Volts <- tbl$Oceaneering.Unit.Voltage
    } else {
      tbl$Sys..Volts <- as.numeric(NA)
    }

  }


  #     E-Sampler fixes     ---------------------------------------------------

  if ( monitorType == "ESAM" ) {

    # UnitID=1050 in July, 2016 has extra rows with some sort of metadata in columns Serial.Number and Data.1
    # We remove those here.

    serialNumberMask <- (tbl$Serial.Number != "") & !is.na(tbl$Serial.Number)
    if ( sum(serialNumberMask) > 0 ) {
      logger.trace("Removing %d 'Serial Number' records from raw data", sum(serialNumberMask))
      tbl <- tbl[!serialNumberMask,]
    }

  }

  #     EBAM1020 fixes     ---------------------------------------------------

  if ( monitorType == "BAM1020" ) {

    # TODO: UnitID=49 on 6/20/10 had every other row missing; also, no lat/lon info.
    # TODO: May want to look into this further if noticed in more recent data as well.

  }

  #     Various fixes     -----------------------------------------------------

  # Check to see if any records remain
  if ( nrow(tbl) == 0 ) {
    logger.error("No data remaining after parsing cleanup")
    stop("No data remaining after parsing cleanup", call.=FALSE)
  }

  # NOTE:  Latitude, Longitude and Sys..Volts are measured at 6am and 6pm
  # NOTE:  as separate GPS entries in the tibble. They need to be carried
  # NOTE:  forward so they appear in all rows.

  gpsMask <- !is.na(tbl$Longitude)

  if (monitorType == "EBAM") {
    voltLabel <- "Sys..Volts"
  } else if (monitorType == "ESAM") {
    voltLabel <- "System.Volts"
  } else {
    # NOTE: BAM1020s don't have voltage data
    voltLabel <- NULL
  }

  # use "quosures" to let us use a variable as a column name
  voltColumn <- rlang::enquo(voltLabel)

  # Propagate data forwards, then backwards to fill in missing values
  tbl <- tbl %>%
    tidyr::fill(.data$Longitude, .data$Latitude, !!voltColumn) %>%
    tidyr::fill(.data$Longitude, .data$Latitude, !!voltColumn, .direction = "up")

  logger.trace("Removing %d 'GPS' records from raw data", sum(gpsMask))
  tbl <- tbl[!gpsMask,]


  logger.trace('Retaining %d rows of raw %s measurements', nrow(tbl), monitorType)

  return(tbl)

}
