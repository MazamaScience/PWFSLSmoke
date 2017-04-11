#' @keywords AIRSIS
#' @export
#' @title Apply Quality Control to Raw AIRSIS E-Sampler Dataframe
#' @param df single site dataframe created by \code{airsis_downloadData()}
#' @param valid_Longitude range of valid Longitude values
#' @param valid_Latitude range of valid Latitude values
#' @param remove_Lon_zero flag to remove rows where Longitude == 0
#' @param remove_Lat_zero flag to remove rows where Latitude == 0
#' @param valid_Flow range of valid Flow.l.m values
#' @param valid_AT range of valid AT.C. values
#' @param valid_RHi range of valid RHi... values
#' @param valid_Conc range of valid Conc.mg.m3. values
#' @param flagAndKeep flag, rather then remove, bad data during the QC process
#' @description Perform various QC measures on AIRSIS E-Sampler data.
#' 
#' The following columns of data are tested against valid ranges:
#' \itemize{
#' \item{\code{Flow}}
#' \item{\code{AT}}
#' \item{\code{RHi}}
#' \item{\code{ConcHr}}
#' }
#' 
#' A \code{POSIXct datetime} column (UTC) is also added based on \code{TimeStamp}.
#' 
#' @return Cleaned up dataframe of AIRSIS monitor data.
#' @seealso \code{\link{airsis_qualityControl}}

airsis_ESAMQualityControl <- function(df,
                                      valid_Longitude=c(-180,180),
                                      valid_Latitude=c(-90,90),
                                      remove_Lon_zero = TRUE,
                                      remove_Lat_zero = TRUE,
                                      valid_Flow = c(1.999,2.001),     # anything other than 2 is bad
                                      valid_AT = c(-Inf,150),
                                      valid_RHi = c(-Inf,55),
                                      valid_Conc = c(-Inf,984),
                                      flagAndKeep = FALSE) {
  
  # TODO:  What about Alarm?
  
  #   > names(df)
  #    [1] "MasterTable_ID"        "Alias"                 "Latitude"              "Longitude"            
  #    [5] "Conc.mg.m3."           "Flow.l.m."             "AT.C."                 "BP.PA."               
  #    [9] "RHx..."                "RHi..."                "WS.M.S."               "WD.Deg."              
  #   [13] "BV.V."                 "Alarm"                 "Start.Date.Time..GMT." "Serial.Number"        
  #   [17] "System.Volts"          "Data.1"                "Data.2"                "TimeStamp"            
  #   [21] "PDate"                 "monitorName"           "monitorType"          
  
  monitorName <- df$monitorName[1]
  
  # Setup for flagAndKeep argument utility
  if ( flagAndKeep ) {
    
    # verb for logging messages and 
    verb <- "Flagging"
    df$rowID <- as.integer(rownames(df))
    
    # duplicate df and add columns for flags
    dfFlagged <- df
    dfFlagged$QCFlag_anyBad <- FALSE
    dfFlagged$QCFlag_reasonCode <- NA
    dfFlagged$QCFlag_badLon <- FALSE
    dfFlagged$QCFlag_badLat <- FALSE
    dfFlagged$QCFlag_badType <- FALSE # no type info for ESAMs
    dfFlagged$QCFlag_badFlow <- FALSE
    dfFlagged$QCFlag_badAT <- FALSE
    dfFlagged$QCFlag_badRHi <- FALSE
    dfFlagged$QCFlag_badConcHr <- FALSE
    dfFlagged$QCFlag_badDateAndTime <- FALSE
    dfFlagged$QCFlag_duplicateHr <- FALSE
    
  } else {
    
    # verb for logging messages
    verb <- "Discarding"
    
  }
  
  # ----- Missing Values ------------------------------------------------------
  
  # Handle various missing value flags
  
  
  # ----- Location ------------------------------------------------------------
  
  # Latitude and longitude must be in range
  if ( remove_Lon_zero ) {
    goodLonMask <- !is.na(df$Longitude) & (df$Longitude >= valid_Longitude[1]) & (df$Longitude <= valid_Longitude[2]) & (df$Longitude != 0)
  } else {
    goodLonMask <- !is.na(df$Longitude) & (df$Longitude >= valid_Longitude[1]) & (df$Longitude <= valid_Longitude[2])
  }
  
  if ( remove_Lat_zero ) {
    goodLatMask <- !is.na(df$Latitude) & (df$Latitude >= valid_Latitude[1]) & (df$Latitude <= valid_Latitude[2]) & (df$Latitude != 0)
  } else {    
    goodLatMask <- !is.na(df$Latitude) & (df$Latitude >= valid_Latitude[1]) & (df$Latitude <= valid_Latitude[2])
  }
  
  badRows <- !(goodLonMask & goodLatMask)
  badRowCount <- sum(badRows)
  if ( badRowCount > 0 ) {
    logger.info(paste(verb,"%s rows with invalid location information"), badRowCount)
    badLocations <- paste('(',df$Longitude[badRows],',',df$Latitude[badRows],')',sep='')
    logger.debug("Bad locations: %s", paste0(badLocations, collapse=", "))
    
    if ( flagAndKeep ) {
      
      # Flag bad longitudes
      dfFlagged$QCFlag_badLon[df$rowID[!goodLonMask]] <- TRUE
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodLonMask]] <- paste0(dfFlagged$QCFlag_reasonCode[df$rowID[!goodLonMask]],"badLon ")
      
      
      # Flag bad latitudes
      dfFlagged$QCFlag_badLat[df$rowID[!goodLatMask]] <- TRUE
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodLatMask]] <- paste0(dfFlagged$QCFlag_reasonCode[df$rowID[!goodLatMask]],"badLat ")
      
      
      # Flag any bad
      dfFlagged$QCFlag_anyBad <- dfFlagged$QCFlag_anyBad | dfFlagged$QCFlag_badLon | dfFlagged$QCFlag_badLat
      
    }
    
  }
  
  df <- df[goodLonMask & goodLatMask,]
  
  
  # ----- Time ----------------------------------------------------------------
  
  # TODO:  How best to assign TimeStamp column with second accuracy to an hourly datetime variable?
  # TODO:  Should we use TimeStampm or PDate?
  # TODO:  Are these data in GMT?
  
  # NOTE: It appears the ESAM "TimeStamp" data drifts throughout the day, with >60 minutes between timestamps during most
  # NOTE: hours in the day, and then a daily re-synch. For now we are assuming this is a communication issue rather than
  # NOTE: an issue in the actual sampling period. For example, we are assuming that a record that is received at 4:44pm is 
  # NOTE: actually a record for 4:00pm (which is really representative of data during the 3:00 hour -- see NOTE below).
  
  # Add a POSIXct datetime
  df$datetime <- lubridate::floor_date(lubridate::mdy_hms(df$TimeStamp), unit="hour") - lubridate::dhours(1)
  if ( flagAndKeep ) {
    # TODO: Unable to get datetime moved from df to dfFlagged without timezone and/or display getting messed up.
    # For now just duplicating the calculation, then assigning row values to NA after the fact for rows that were
    # removed from df prior to calculating datetime above. Clean up later if possible.
    dfFlagged$datetime <- lubridate::floor_date(lubridate::mdy_hms(dfFlagged$TimeStamp), unit="hour") - lubridate::dhours(1)
    dfFlagged$datetime[ which(!(dfFlagged$rowID %in% df$rowID)) ] <- NA
  }
  
  # NOTE: The time above truncates the timestamp to the top of an hour, and then subtracts one hour,
  # NOTE: since the measurement that comes in at a few minutes past the hour is actually representative
  # NOTE: of the data over the previous hour (e.g. reading received at 12:04 is actually the average of 
  # NOTE: the data during Hour 11). This allows for a simpler understanding of the averages, since an
  # NOTE: hour's average will be representative of the data within that hour (this is similar to
  # NOTE: how an average over a year, such as 2016, is referred to as 2016's value, not 2017's, even
  # NOTE: though the average wasn't available until the beginning of 2017).
  
  # Leland Tarnay QC -----------------------------------------------------------
  
  ###tmp.2013_NIFC_GOES65_wrcc$concQA <- with(tmp.2013_NIFC_GOES65_wrcc,
  ###                                         ifelse(Flow < 2 "FlowLow",
  ###                                         ifelse(Flow > 2, "FlowHigh",
  ###                                         ifelse(AT > 150, "HighTemp",
  ###                                         ifelse(RHi > 55,"HighRHi",
  ###                                         ifelse(ConcHr < 0, "Negative",
  ###                                         ifelse(ConcHr > 984, "HighConc", 'OK')))))))
  ####create a concHR numerical column, with NA values that aren't verbose about errors..
  ###  
  ###tmp.2013_NIFC_GOES65_wrcc$concHR <- with(tmp.2013_NIFC_GOES65_wrcc,
  ###                                         ifelse(concQA == 'Negative', 0,
  ###                                         ifelse(concQA == 'OK', ConcHr, NA)))
  
  goodFlow <- !is.na(df$Flow.l.m.) & df$Flow.l.m. >= valid_Flow[1] & df$Flow.l.m. <= valid_Flow[2]
  goodAT <- !is.na(df$AT.C.) & df$AT.C. >= valid_AT[1] & df$AT.C. <= valid_AT[2]
  goodRHi <- !is.na(df$RHi...) & df$RHi... >= valid_RHi[1] & df$RHi... <= valid_RHi[2]
  goodConcHr <- !is.na(df$Conc.mg.m3.) & df$Conc.mg.m3. >= valid_Conc[1] & df$Conc.mg.m3. <= valid_Conc[2]
  gooddatetime <- !is.na(df$datetime) & df$datetime < lubridate::now("UTC") # saw a future date once
  
  logger.debug("Flow has %s missing or out of range values", sum(!goodFlow))
  if ( sum(!goodFlow) > 0 ) {
    logger.debug("Bad Flow values:  %s", paste0(sort(df$Flow.l.m.[!goodFlow]), collapse=", "))
    if ( flagAndKeep ) {
      # Flag bad flow
      dfFlagged$QCFlag_badFlow[df$rowID[!goodFlow]] <- TRUE
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodFlow]] <- paste0(dfFlagged$QCFlag_reasonCode[df$rowID[!goodFlow]],"badFlow ")
    }
  }
  logger.debug("AT has %s missing or out of range values", sum(!goodAT))
  if ( sum(!goodAT) > 0 ) {
    logger.debug("Bad AT values:  %s", paste0(sort(df$AT.C.[!goodAT]), collapse=", "))
    if ( flagAndKeep ) {
      # Flag bad air temperature
      dfFlagged$QCFlag_badAT[df$rowID[!goodAT]] <- TRUE
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodAT]] <- paste0(dfFlagged$QCFlag_reasonCode[df$rowID[!goodAT]],"badAT ")
    }
  }
  logger.debug("RHi has %s missing or out of range values", sum(!goodRHi))
  if ( sum(!goodRHi) > 0 ) {
    logger.debug("Bad RHi values:  %s", paste0(sort(df$RHi...[!goodRHi]), collapse=", "))
    if ( flagAndKeep ) {
      # Flag bad relative humidity
      dfFlagged$QCFlag_badRHi[df$rowID[!goodRHi]] <- TRUE
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodRHi]] <- paste0(dfFlagged$QCFlag_reasonCode[df$rowID[!goodRHi]],"badRHi ")
    }
  }
  logger.debug("Conc has %s missing or out of range values", sum(!goodConcHr))
  if ( sum(!goodConcHr) > 0 ) {
    logger.debug("Bad Conc values:  %s", paste0(sort(df$Conc.mg.m3.[!goodConcHr]), collapse=", "))
    if ( flagAndKeep ) {
      # Flag bad concentration
      dfFlagged$QCFlag_badConcHr[df$rowID[!goodConcHr]] <- TRUE
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodConcHr]] <- paste0(dfFlagged$QCFlag_reasonCode[df$rowID[!goodConcHr]],"badConcHr ")
    }
  }
  logger.debug("datetime has %s missing or out of range values", sum(!gooddatetime))
  if ( sum(!gooddatetime) > 0 ) {
    logger.debug("Bad datetime values:  %s", paste0(sort(df$datetime[!gooddatetime]), collapse=", "))
    if ( flagAndKeep ) {
      # Flag bad dateandtime
      dfFlagged$QCFlag_badDateAndTime[df$rowID[!gooddatetime]] <- TRUE
      dfFlagged$QCFlag_reasonCode[df$rowID[!gooddatetime]] <- paste0(dfFlagged$QCFlag_reasonCode[df$rowID[!gooddatetime]],"badDateAndTime ")
    }
  }
  
  goodMask <- goodFlow & goodAT & goodRHi & goodConcHr & gooddatetime
  
  # Flag any bad
  if ( flagAndKeep ) {
    dfFlagged$QCFlag_anyBad <- (dfFlagged$QCFlag_anyBad | dfFlagged$QCFlag_badFlow | dfFlagged$QCFlag_badAT | 
                                  dfFlagged$QCFlag_badRHi | dfFlagged$QCFlag_badConcHr | dfFlagged$QCFlag_badDateAndTime)
  }
  
  df <- df[goodMask,]
  
  badQCCount <- sum(!goodMask)
  if ( badQCCount > 0 ) {
    logger.info(paste(verb,"%s rows because of QC logic"), badQCCount)
  }
  
  
  # ----- Duplicate Hours -----------------------------------------------------
    
  # For hours with multiple records, discard all but the one with the latest processing date/time
  # NOTE: Current setup for this section assumes that the last entry will be the latest one.  May 
  # NOTE: want to build in functionality to ensure that the latest is picked if more than one exists
  # NOTE: (for example, if the data is not in order by timestamp for whatever reason)
   
  dupHrMask <- duplicated(df$datetime,fromLast = TRUE)
  dupHrCount <- sum(dupHrMask)
  uniqueHrMask <- !dupHrMask
  
  if ( dupHrCount > 0 ) {
    logger.info(paste(verb,"%s duplicate time entries"), dupHrCount)
    logger.debug("Duplicate Hours (may be >1 per timestamp):  %s", paste0(sort(unique(df$TimeStamp[dupHrMask])), collapse=", "))
    # TODO: the line above caused an error for at least one ESAM (1054); consider updating to TimeStamp or other timestamp
    if ( flagAndKeep ) {
      # Flag duplicate hours
      dfFlagged$QCFlag_duplicateHr[df$rowID[!dupHrMask]] <- TRUE
      dfFlagged$QCFlag_reasonCode[df$rowID[!dupHrMask]] <- "duplicateHr"
      
      # Flag any bad 
      dfFlagged$QCFlag_anyBad <- dfFlagged$QCFlag_anyBad | dfFlagged$QCFlag_duplicateHr
    }
  }

  df <- df[uniqueHrMask,]
  
  
  # ----- More QC -------------------------------------------------------------
  
  # TODO:  Other QC?
  
  if ( flagAndKeep ) {
    logger.debug("Retaining %d rows of measurements; %d bad rows flagged", nrow(df), sum(dfFlagged$QCFlag_anyBad))    
  } else {
    logger.debug("Retaining %d rows of validated measurements", nrow(df))
  }
  
  # ----- Final cleanup -------------------------------------------------------
  
  if ( flagAndKeep ) {
    dfFlagged$QCFlag_reasonCode <- trimws(dfFlagged$QCFlag_reasonCode)
    dfFlagged$QCFlag_reasonCode <- substring(dfFlagged$QCFlag_reasonCode, 3)
    df <- dfFlagged
    df$rowID <- NULL
  }
  
  return(df)
  
}
