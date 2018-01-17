#' @keywords internal
#' @export
#' @title Parse AIRSIS Dump File Data String
#' @param fileString character string containing AIRSIS dump file
#' @description Raw character data from AIRSIS are parsed into a tibble.
#' The incoming \code{fileString} can be read from a local
#' file using \code{readr::read_file()}.
#' 
#' This function is intended for internal use at the Pacific Wildland
#' Fire Sciences Lab.
#' 
#' @return List of tibbles of AIRSIS raw monitor data from multiple monitors.

# NOTE:  AIRSIS dump files have a format that similar to data downloaded form AIRSIS.
# NOTE:  Each contains a single header line followed by data records. Dump files have
# NOTE:  the following differences:
# NOTE:   * they may contain data for more than one unitID and thus repeated timestamps
# NOTE:   * Latitude and Longitude have been assigned
# NOTE:   * no MasterTable_ID or PDate
#
# Direct download from AIRSIS CSV service:
#
# MasterTable_ID,Alias,Latitude,Longitude,Date/Time/GMT,Start Date/Time (GMT),COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,TimeStamp,PDate
# 53640931,Naches WA (1033),,,4/14/2017 7:00:00 PM,24-MAR-2017 00:00:0,,,,,,,,,,,,, H9495 ,3613-01 R1.57.0,,4/14/2017 7:30:17 PM,4/14/2017 7:30:00 PM                          
# 53640969,Naches WA (1033),46.73171,-120.7054,4/14/2017 7:00:00 PM,,,,,,,,,,,,,,,,13.32,4/14/2017 7:37:19 PM,4/14/2017 7:38:00 PM
#
# Dump file:
#
# Date/Time/GMT,COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,UnitID,Alias,Latitude,Longitude,TimeStamp 
# 4/10/2017 11:00:00 AM,0.018,0.005,16.7,0.3,48,-0.2,87,35,14.2,5.7,0,PM 2.5,,,,1013,1013 Johnsondale,35.9698927402496,-118.541343212128,4/10/2017 11:04:30 AM
# 4/10/2017 12:00:00 PM,-0.005,0.006,16.7,0.3,61,-0.7,88,35,14.2,5.1,0,PM 2.5,,,,1013,1013 Johnsondale,35.9698927402496,-118.541343212128,4/10/2017 12:04:31 PM


airsisDump_parseData <- function(fileString) {
  
  # Identify monitor type
  monitorTypeList <- airsisDump_identifyMonitorType(fileString)
  
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
  
  #     Various fixes     -----------------------------------------------------

  # Check to see if any records remain
  if ( nrow(tbl) == 0 ) {
    logger.warn("No data remaining after parsing cleanup")
    stop("No data remaining after parsing cleanup", call.=FALSE)
  }
  
  # NOTE:  No need to fix Latitude and Longitude as they are already present  

  # Add monitor name and type
  tbl$monitorName <- tbl$Alias
  tbl$monitorType <- monitorType
  
  logger.debug('Retaining %d rows of raw %s measurements', nrow(tbl), monitorType)
  
  # Split the tibble int a list of per-monitor tibbles
  tblList <- split(tbl, tbl$Alias) # list of tibbles
  ### tblList <- lapply(tblList, as.data.frame, stringsAsFactors=FALSE)
  names(tblList) <- make.names(names(tblList))
  
  return(tblList)
}
