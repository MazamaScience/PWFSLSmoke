#' @keywords AIRSIS
#' @export
#' @title Parse AIRSIS Dump File Data String
#' @param fileString character string containing AIRSIS data as a csv
#' @description Raw character data from AIRSIS are parsed into a dataframe.
#' The incoming \code{fileString}
#' can be read in directly from AIRSIS using \code{airsis_downloadData()} or from a local
#' file using \code{readr::read_file()}.
#' 
#' The type of monitor represented by this fileString is inferred from the column names
#' using \code{airsis_identifyMonitorType()} and appropriate column types are assigned.
#' The character data are then read into a dataframe and augmented in the following ways:
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
#' fileString <- airsis_downloadData('USFS',unitID='1026',startdate=20150701,enddate=20151231)
#' df <- airsis_parseData(fileString)
#' }

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
    logger.warn('No valid PM2.5 data')
    stop(paste0('No valid PM2.5 data'))
  }
  
  # Sanity check -- more than just the header line
  if ( length(lines) == 1 ) {
    logger.info("No data available")
    stop("No data avaialble")
  }
  
  if ( monitorType == "BAM1020" ) {
    
    # TODO:  How to assign data with to-the-minute timestamps to a particular hour? floor?
    logger.warn('BAM1020 file parsing is not supported')
    logger.debug('Header line:\n\t%s', paste0(rawNames,collapse=','))
    stop(paste0('BAM1020 file parsing is not supported'), call.=FALSE)
    
  } else if ( monitorType == "EBAM" ) {
    
    logger.debug('Parsing EBAM data...')
    
  } else if ( monitorType == "ESAM" ) {
    
    logger.debug('Parsing E-Sampler data...')
    
    # NOTE:  Some E-Sampler files from AIRSIS (USFS 1050) have internal rows messed up with header line information
    # NOTE:  We need to remove these first. It seems they can be identified by searching for '%'.
    # NOTE:  Of course, we have to retain the first header line.
    internalHeaderLines <- which(stringr::str_detect(lines,'%'))[-1]
    
    logger.debug("Removing %d 'internal header line' records from raw data", length(internalHeaderLines))
    lines <- lines[-internalHeaderLines]
    
  } else if ( monitorType == "OTHER_1" ) {
    
    logger.warn('Older EBAM 1 file parsing is not supported')
    logger.debug('Header line:\n\t%s', paste0(rawNames,collapse=','))
    stop(paste0('Older EBAM 1 file parsing is not supported', call.=FALSE))
    
  } else if ( monitorType == "OTHER_2" ) {
    
    logger.warn('Older EBAM 2 file parsing is not supported')
    logger.debug('Header line:\n\t%s', paste0(rawNames,collapse=','))
    stop(paste0('Older EBAM 2 file parsing is not supported', call.=FALSE))
    
  } else {
    
    logger.warn('Unkown file parsing is not supported')
    logger.debug('Header line:\n\t%s', paste0(rawNames,collapse=','))
    stop(paste0('Unknown file parsing is not supported', call.=FALSE))
    
  }
  
  
  #     Parse the file     ----------------------------------------------------
  
  # Remove header line, leaving only data
  fakeFile <- paste0(lines[-1], collapse='\n')
  
  df <- suppressWarnings( readr::read_csv(fakeFile, col_names=columnNames, col_types=columnTypes) )
  
  # Print out any problems encountered by readr::read_csv
  problemsDF <- readr::problems(df)
  if ( dim(problemsDF)[1] > 0 ) {
    logger.debug('Records skipped with parsing errors:')
    problems <- utils::capture.output(format(problemsDF))
    for (i in 1:length(problems)) {
      logger.debug("%s",problems[i])
    }
  }
  
  #     E-Sampler fixes     ---------------------------------------------------
  
  if ( monitorType == "ESAM" ) {
    
    # UnitID=1050 in July, 2016 has extra rows with some sort of metadata in columns Serial.Number and Data.1
    # We remove those here.
    
    serialNumberMask <- (df$Serial.Number != "") & !is.na(df$Serial.Number)
    logger.debug("Removing %d 'Serial Number' records from raw data", sum(serialNumberMask))
    df <- df[!serialNumberMask,]
    
    missingDataMask <- is.na(df$Conc.mg.m3.)
    logger.debug("Removing %d 'missing data' records from raw data", sum(missingDataMask))
    df <- df[!missingDataMask,]
    
  }
  
  #     Various fixes     -----------------------------------------------------
  
  # NOTE:  Latitude, Longitude and Sys..Volts are measured at 6am and 6pm
  # NOTE:  as separate GPS entries in the dataframe. They need to be carried
  # NOTE:  forward so they appear in all rows.
  
  # Carry data forward to fill in all missing values
  df$Longitude <- zoo::na.locf(df$Longitude, na.rm=FALSE)
  df$Latitude <- zoo::na.locf(df$Latitude, na.rm=FALSE)
  if ( monitorType == "BAM1020" ) df$System.Volts <- zoo::na.locf(df$System.Volts, na.rm=FALSE)
  if ( monitorType == "EBAM" ) df$Sys..Volts <- zoo::na.locf(df$Sys..Volts, na.rm=FALSE)
  if ( monitorType == "ESAM" ) df$System.Volts <- zoo::na.locf(df$System.Volts, na.rm=FALSE)
  
  # Now fill in any missing values at the front end
  df$Longitude <- zoo::na.locf(df$Longitude, na.rm=FALSE, fromLast=TRUE)
  df$Latitude <- zoo::na.locf(df$Latitude, na.rm=FALSE, fromLast=TRUE)
  if ( monitorType == "BAM1020" ) df$System.Volts <- zoo::na.locf(df$System.Volts, na.rm=FALSE, fromLast=TRUE)
  if ( monitorType == "EBAM" ) df$Sys..Volts <- zoo::na.locf(df$Sys..Volts, na.rm=FALSE, fromLast=TRUE)
  if ( monitorType == "ESAM" ) df$System.Volts <- zoo::na.locf(df$System.Volts, na.rm=FALSE, fromLast=TRUE)
  
  # Add monitor name and type
  df$monitorName <- df$Alias
  df$monitorType <- monitorType
  
  logger.debug('Retaining %d rows of raw %s measurements', nrow(df), monitorType)
  
  
  return(df)
}
