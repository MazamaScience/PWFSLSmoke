#' @keywords AIRSIS
#' @export
#' @title Parse AIRSIS Data String
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
#' @examples
#' \dontrun{
#' fileString <- airsis_downloadData(20150701, 20151231, provider='USFS', unitID='1026')
#' tbl <- airsis_parseData(fileString)
#' }

airsis_parseData <- function(fileString) {
  
  # Identify monitor type
  monitorTypeList <- airsis_identifyMonitorType(fileString)
  
  monitorType <- monitorTypeList$monitorType
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
    
    logger.debug("Parsing BAM1020 data ...")
    
  } else if ( monitorType == "EBAM" ) {
    
    logger.debug("Parsing EBAM data ...")
    
  } else if ( monitorType == "ESAM" ) {
    
    logger.debug("Parsing E-Sampler data ...")
    
    # NOTE:  Some E-Sampler files from AIRSIS (USFS 1050) have internal rows messed up with header line information
    # NOTE:  We need to remove these first. It seems they can be identified by searching for '%'.
    # NOTE:  Of course, we have to retain the first header line.
    
    internalHeaderMask <- stringr::str_detect(lines,'%')
    internalHeaderMask[1] <- FALSE
    if ( sum(internalHeaderMask) > 0 ) {
      logger.debug("Removing %d 'internal header line' records from raw data", sum(internalHeaderMask))
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
    logger.debug("Records skipped with parsing errors:")
    problems <- utils::capture.output(format(problemsDF))
    for (i in 1:length(problems)) {
      logger.debug("%s",problems[i])
    }
  }
  
  # Add monitor name and type
  tbl$monitorName <- tbl$Alias
  tbl$monitorType <- monitorType
  
  #     E-Sampler fixes     ---------------------------------------------------
  
  if ( monitorType == "ESAM" ) {
    
    # UnitID=1050 in July, 2016 has extra rows with some sort of metadata in columns Serial.Number and Data.1
    # We remove those here.
    
    serialNumberMask <- (tbl$Serial.Number != "") & !is.na(tbl$Serial.Number)
    if ( sum(serialNumberMask) > 0 ) {
      logger.debug("Removing %d 'Serial Number' records from raw data", sum(serialNumberMask))
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
  
  # Carry data forward to fill in all missing values
  tbl$Longitude <- zoo::na.locf(tbl$Longitude, na.rm=FALSE)
  tbl$Latitude <- zoo::na.locf(tbl$Latitude, na.rm=FALSE)
  # NOTE: BAM1020s don't have voltage data
  if ( monitorType == "EBAM" ) tbl$Sys..Volts <- zoo::na.locf(tbl$Sys..Volts, na.rm=FALSE)
  if ( monitorType == "ESAM" ) tbl$System.Volts <- zoo::na.locf(tbl$System.Volts, na.rm=FALSE)
  
  # Now fill in any missing values at the front end
  tbl$Longitude <- zoo::na.locf(tbl$Longitude, na.rm=FALSE, fromLast=TRUE)
  tbl$Latitude <- zoo::na.locf(tbl$Latitude, na.rm=FALSE, fromLast=TRUE)
  # NOTE: BAM1020s don't have voltage data
  if ( monitorType == "EBAM" ) tbl$Sys..Volts <- zoo::na.locf(tbl$Sys..Volts, na.rm=FALSE, fromLast=TRUE)
  if ( monitorType == "ESAM" ) tbl$System.Volts <- zoo::na.locf(tbl$System.Volts, na.rm=FALSE, fromLast=TRUE)
  
  logger.debug("Removing %d 'GPS' records from raw data", sum(gpsMask))
  tbl <- tbl[!gpsMask,]
  
  
  logger.debug('Retaining %d rows of raw %s measurements', nrow(tbl), monitorType)

  return(tbl)
  
}
