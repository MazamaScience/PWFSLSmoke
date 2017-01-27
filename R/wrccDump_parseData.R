#' @keywords WRCC
#' @export
#' @title Parse WRCC Dump File String
#' @param fileString character string containing WRCC dump file
#' @description Raw character data from WRCC are parsed into a dataframe.
#' The incoming \code{fileString} can be read from a local \code{SmokeMon.csv}
#' file using \code{readr::read_file()}.
#' 
#' This function is intended for internal use at the Pacific Wildland
#' Fire Sciences Lab.
#' 
#' @return List of dataframes of WRCC raw monitor data from multiple monitors.

# Below is an excerpt from a SmokeMon.csv file includes multiple named monitors,
# some of which contain no header, some of which contain a header but no data
# and some of which contain a header and data.

# FWS Smoke #1
# :          ,  LST  , Deg , Deg ,     ,ser #,ug/m3, Unk , l/m ,Deg C,  %  ,mbar ,deg C,  %  , m/s , Deg ,volts,     
# :   Date   , Time  ,  GPS  ,  GPS  ,Type   ,Serial ,Conc   , Misc  , Ave.  , Av Air,  Rel  , Barom ,Sensor ,Sensor ,  Wind , Wind  ,Battery,Alarm  
# :MM/DD/YYYY, hh:mm ,  Lat. ,  Lon. ,       ,Number , RT    ,  #1   ,Air Flw,  Temp ,Humidty, Press ,Int AT ,Int RH ,  Speed, Direc ,Voltage,       
# 09/25/2016,00:00,48.344,-117.26,9,18100,0,99999,2,16.7,0,948.79,99999,24,1.4,165,14.2,0
# 09/25/2016,01:00,48.344,-117.26,9,18100,0,99999,2,14.5,0,948.4,99999,26,1.2,141,14.2,0
# 09/25/2016,02:00,48.344,-117.26,9,18100,0,99999,2,15,0,948.6,99999,29,0.80002,27,14.2,0
# 09/25/2016,03:00,48.344,-117.26,9,18100,1,99999,2,11.9,0,949.18,99999,35,0.70002,66,14.2,0
# ...
# 09/27/2016,23:00,48.344,-117.26,9,18100,1,99999,2,25.9,0,940.59,99999,20,1.2,199,14.1,0
# Smoke #11
# :          ,  LST  , Deg , Deg ,     ,ser #,ug/m3, Unk , l/m ,Deg C,  %  ,mbar ,deg C,  %  , m/s , Deg ,volts,     
# :   Date   , Time  ,  GPS  ,  GPS  ,Type   ,Serial ,Conc   , Misc  , Ave.  , Av Air,  Rel  , Barom ,Sensor ,Sensor ,  Wind , Wind  ,Battery,Alarm  
# :MM/DD/YYYY, hh:mm ,  Lat. ,  Lon. ,       ,Number , RT    ,  #1   ,Air Flw,  Temp ,Humidty, Press ,Int AT ,Int RH ,  Speed, Direc ,Voltage,       
# Smoke #13
# Smoke #15
# :          ,  LST  , Deg , Deg ,     ,ser #,ug/m3, Unk , l/m ,Deg C,  %  ,mbar ,deg C,  %  , m/s , Deg ,volts,     
# :   Date   , Time  ,  GPS  ,  GPS  ,Type   ,Serial ,Conc   , Misc  , Ave.  , Av Air,  Rel  , Barom ,Sensor ,Sensor ,  Wind , Wind  ,Battery,Alarm  
# :MM/DD/YYYY, hh:mm ,  Lat. ,  Lon. ,       ,Number , RT    ,  #1   ,Air Flw,  Temp ,Humidty, Press ,Int AT ,Int RH ,  Speed, Direc ,Voltage,       
# Smoke #16
# Smoke #17
# :          ,  LST  , Deg , Deg ,     ,ser #,ug/m3, Unk , l/m ,Deg C,  %  , Unk ,deg C,  %  , m/s , Deg ,volts,     
# :   Date   , Time  ,  GPS  ,  GPS  ,Type   ,Serial ,Conc   , Misc  , Ave.  , Av Air,  Rel  , Misc  ,Sensor ,Sensor ,  Wind , Wind  ,Battery,Alarm  
# :MM/DD/YYYY, hh:mm ,  Lat. ,  Lon. ,       ,Number , RT    ,  #1   ,Air Flw,  Temp ,Humidty,  #2   ,Int AT ,Int RH ,  Speed, Direc ,Voltage,       
# 09/25/2016,00:00,40.733,-106.27,9,871700,930,99999,2,7.6,59,75597,99999,34,2.5001,164,14.4,1
# 09/25/2016,01:00,40.733,-106.27,9,871700,930,99999,2,6.7,67,75714,99999,35,1.4,164,14.4,1
# 09/25/2016,02:00,40.733,-106.27,9,871700,931,99999,2,5.5,78,75792,99999,35,1,169,14.4,1
# 09/25/2016,03:00,40.733,-106.27,9,871700,931,99999,2,4.6,81,75890,99999,35,1.2,169,14.4,1
# ...

wrccDump_parseData <- function(fileString) {
  
  # Configure missing value strings for WRCC dump files
  na_strings <- c('99999')
  
  # Convert the fileString into individual lines
  lines <- readr::read_lines(fileString)
  
  if ( length(lines) <= 4 ) {
    logger.warn("No valid PM2.5 data")
    stop(paste0("No valid PM2.5 data"))
  }
  
  # Remove "FWS " from first line
  lines[1] <- stringr::str_replace(lines[1],'^FWS Smoke','Smoke')
  
  # Find monitor lines and how many lines are associated with each monitor
  isMonitorLine <- stringr::str_detect(lines,'^Smoke')
  monitorLineIndices <- which(isMonitorLine)
  monitorCount <- length(monitorLineIndices)
  monitorLineCount <- diff(monitorLineIndices, lag=1)
  # Add final monitorLineCount value
  monitorLineCount[monitorCount] <- length(lines) - monitorLineIndices[monitorCount] + 1
  
  # Create empty list (no pre-allocation needed when lists are referenced by key instead of integer)
  dfList <- list()
  
  # Loop over monitors
  for ( i in 1:length(monitorLineIndices) ) {
    
    lineIndex <- monitorLineIndices[i]
    lineCount <- monitorLineCount[i]
    
    if ( lineCount <= 4 ) {
      logger.debug("No valid PM2.5 data for %s", lines[lineIndex])
      next
    }
    
    logger.debug("Parsing data for %s", lines[lineIndex])
    
    # For monitors with data, extract associated monitor, header and data lines
    singleMonitorIndices <- seq(lineIndex,length.out=lineCount)
    singleMonitorFileString <- paste(lines[singleMonitorIndices], collapse='\n')
    
    # -------------------------------------------------------------------------
    
    # NOTE:  The next chunk is copied mostly verbatim from wrcc_parseData.R but modified to 
    # NOTE:  handle the minor differences between WRCC dump files and WRCC web downloads.
    # NOTE:  These differences include:
    # NOTE:   * comma separated instead of tab separated
    # NOTE:   * separate column for Time
    
    # Identify monitor type
    monitorTypeList <- wrccDump_identifyMonitorType(singleMonitorFileString)
    
    monitorType <- monitorTypeList$monitorType
    rawNames <- monitorTypeList$rawNames
    columnNames <- monitorTypeList$columnNames
    columnTypes <- monitorTypeList$columnTypes
    
    if ( monitorType == "UNKNOWN" ) {
      logger.debug("WRCC header type == %s", monitorType)
      next
    }
    
    logger.debug("WRCC header type == %s", monitorType)
    
    # Convert the fileString into individual lines
    singleMonitorLines <- readr::read_lines(singleMonitorFileString)
    
    # Strip spaces from the beginning and end
    singleMonitorLines <- stringr::str_replace(singleMonitorLines,'^ *','')
    singleMonitorLines <- stringr::str_replace(singleMonitorLines,' *$','')
    
    # Get monitorName from first line and then remove that line
    monitorName <- make.names(singleMonitorLines[1])
    singleMonitorLines <- singleMonitorLines[-1]
    
    # Remove header lines beginning with ":", leaving only data
    goodLines <- !is.na(singleMonitorLines) & !stringr::str_detect(singleMonitorLines,'^:')
    
    # Read the data into a dataframe
    fakeFile <- paste0(singleMonitorLines[goodLines], collapse='\n')
    df <- readr::read_csv(fakeFile, col_names=columnNames, col_types=columnTypes, na=na_strings)
    
    # Add monitor name
    df$monitorName <- monitorName
    
    # Add monitor type (determined from the 'Type' column after reading in the data)
    
    # Sometimes we get a solid block of 99999s for non-time-location columns
    if ( all(is.na(df$Type)) ) {
      logger.debug("No valid PM2.5 data for %s", monitorName)
      next
    }
    
    monitorTypeCode <- unique(df$Type)
    # NOTE:  Drop all negative values to get rid of -9999 or other missing value flags.
    # NOTE:  Conversion of -9999 to NA happens in the ~QualityControl scripts so that
    # NOTE:  all raw data modifications can be found in one place.
    monitorTypeCode <- monitorTypeCode[monitorTypeCode >= 0]
    
    # Sanity check
    if ( length(monitorTypeCode) > 1 ) {
      logger.error("More than one monitor type detected: %s", paste(monitorTypeCode,sep=", "))
      next
    }
    
    # 0=E-BAM PM2.5, 1=E-BAM PM10, 9=E-Sampler. We only want PM2.5 measurements
    if ( monitorTypeCode == 0 ) {
      df$monitorType <- 'EBAM'
    } else if ( monitorTypeCode == 9 ) {
      df$monitorType <- 'ESAM'
    } else if ( monitorTypeCode == 1 ) {
      logger.error("EBAM PM10 data parsing is not supported")
      next
    } else {
      logger.error("Unsupported monitor type code: %d",monitorTypeCode)
      next
    }
    
    # -------------------------------------------------------------------------
    
    # Prepare for columns modification
    fullColumnNames <- names(df)
    
    # Combine Date and Time into a single column to match web download dataframes
    # where the first column is DateTime in YYmmddHHMM format
    tempDate <- lubridate::mdy(df$Date)
    HHMM <- stringr::str_replace(df$Time,':','')
    df$DateTime <- paste0(strftime(tempDate,"%y%m%d"), HHMM) # year without century! to match web downloads
    # Now remove the Date and Time columns
    df$Date <- NULL
    df$Time <- NULL
    
    # Modify column names to reflect (Date,Time) --> DateTime
    newColumnNames <- fullColumnNames[-1] # drop 'Date'
    newColumnNames[1] <- 'DateTime' # make first column 'DateTime'
    df <- df[,newColumnNames]
    
    # At this point, the dataframe should match what is returned by wrcc_parseData
    
    dfList[[monitorName]] <- df
    
  } # End of loop over monitors
  
  return(dfList)
  
}
