#' @keywords AIRSIS
#' @export
#' @title Apply Quality Control to Raw AIRSIS BAM1020 Dataframe
#' @param df single site dataframe created by \code{airsis_parseData()}
#' @param valid_Longitude range of valid Longitude values
#' @param valid_Latitude range of valid Latitude values
#' @param remove_Lon_zero flag to remove rows where Longitude == 0
#' @param remove_Lat_zero flag to remove rows where Latitude == 0
#' @param valid_Flow range of valid Flow values
#' @param valid_AT range of valid AT values
#' @param valid_RHi range of valid RHi values
#' @param valid_Conc range of valid ConcHr values
#' @param flagAndKeep flag, rather than remove, bad data during the QC process
#' @description Perform various QC measures on AIRSIS BAM1020 data.
#' 
#' A \code{POSIXct datetime} column (UTC) is also added based on \code{DateTime}.
#' 
#' @return Cleaned up dataframe of AIRSIS monitor data.
#' @seealso \code{\link{airsis_qualityControl}}

airsis_BAM1020QualityControl <- function(df,
                                         valid_Longitude=c(-180,180),
                                         valid_Latitude=c(-90,90),
                                         remove_Lon_zero = TRUE,
                                         remove_Lat_zero = TRUE,
                                         valid_Flow = c(.834*.95,.834*1.05),
                                         valid_AT = c(-Inf,45),
                                         valid_RHi = c(-Inf,45),
                                         valid_Conc = c(-Inf,984),
                                         flagAndKeep = FALSE) {
  
  #  > names(df)
  #  [1] "MasterTable_ID"   "Alias"            "Latitude"         "Longitude"
  #  [5] "Conc..ug.m3."     "Qtot..m3."        "WS..KTS."         "Ozone..ppb."
  #  [9] "RTM09..mg3."      "RH...."           "Ambient.Temp..C." "TimeStamp"
  #  [13] "PDate"            "System.Volts"     "monitorName"      "monitorType"
  
  monitorName <- df$monitorName[1]
  
  # ----- Missing Values ------------------------------------------------------
  
  # Handle various missing value flags
  
  # ----- Setup for flagAndKeep argument utility ------------------------------
  
  if ( flagAndKeep ) {
    # verb for logging messages
    verb <- "Flagging"
    
    df$rowID <- as.integer(rownames(df))
    
    # duplicate df and add columns for flags
    dfFlagged <- df
    dfFlagged$QCFlag_anyBad <- FALSE
    dfFlagged$QCFlag_reasonCode <- NA
    dfFlagged$QCFlag_badLon <- FALSE
    dfFlagged$QCFlag_badLat <- FALSE
    dfFlagged$QCFlag_badType <- FALSE
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
      # apply flags
      dfFlagged$QCFlag_badLon[df$rowID[!goodLonMask]] <- TRUE
      dfFlagged$QCFlag_badLat[df$rowID[!goodLatMask]] <- TRUE
      dfFlagged$QCFlag_anyBad <- dfFlagged$QCFlag_anyBad | dfFlagged$QCFlag_badLon | dfFlagged$QCFlag_badLat
      # apply reason codes
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodLonMask]] <- paste(dfFlagged$QCFlag_reasonCode[df$rowID[!goodLonMask]],"badLon")
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodLatMask]] <- paste(dfFlagged$QCFlag_reasonCode[df$rowID[!goodLatMask]],"badLat")
    }
  }

  df <- df[goodLonMask & goodLatMask,]

  # Sanity check -- row count
  if (nrow(df) < 1) {
    err_msg <- paste0("No valid PM2.5 data for ", monitorName)
    logger.error(err_msg)
    stop(err_msg, call.=FALSE)
  }
  
  # ----- Time ----------------------------------------------------------------
  
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
  
  # ----- Type ----------------------------------------------------------------

  # No 'Type' column in BAM1020 files
  
  # QC ------------------------------------------------------------------------
  
  
  # Leland Tarnay QC -----------------------------------------------------------
  
  ### dat.2012arbbamraw$concHR <- ifelse(dat.2012arbbamraw$Qtot.m3.<.834*.95,NA,
  ###                                    ifelse(dat.2012arbbamraw$Qtot.m3.>.834*1.05,NA,
  ###                                    ifelse(dat.2012arbbamraw$IT.C.>45,NA,
  ###                                    ifelse(dat.2012arbbamraw$RH...> 45,NA,
  ###                                    ifelse(dat.2012arbbamraw$Conc.mg.<0,0,
  ###                                    ifelse(dat.2012arbbamraw$Conc.mg.>.984,NA,
  ###                                    ifelse(dat.2012arbbamraw$Delta.C.>25,NA,
  ###                                    dat.2012arbbamraw$Conc.mg.*1000)))))))
  
  # TODO: Consider logic to throw out pm25 data if temperature changes by more than 2 degC in adjacent hrs
  # TODO: (see NOTE in section 2.2 here: https://www.arb.ca.gov/airwebmanual/instrument_manuals/Documents/BAM-1020-9800%20Manual%20Rev%20G.pdf)
  
  goodFlow <- !is.na(df$Qtot..m3.) & df$Qtot..m3. >= valid_Flow[1] & df$Qtot..m3. <= valid_Flow[2]
  goodAT <- !is.na(df$Ambient.Temp..C.) & df$Ambient.Temp..C. >= valid_AT[1] & df$Ambient.Temp..C. <= valid_AT[2]
  goodRHi <- !is.na(df$RH....) & df$RH.... >= valid_RHi[1] & df$RH.... <= valid_RHi[2]
  goodConcHr <- !is.na(df$'Conc..\u00B5g.m3.') & df$'Conc..\u00B5g.m3.' >= valid_Conc[1] & df$'Conc..\u00B5g.m3.' <= valid_Conc[2]
  gooddatetime <- !is.na(df$datetime) & df$datetime < lubridate::now("UTC") # saw a future date once
  
  logger.debug("Flow has %s missing or out of range values", sum(!goodFlow))
  if (sum(!goodFlow) > 0) logger.debug("Bad Flow values:  %s", paste0(sort(unique(df$Qtot..m3.[!goodFlow]),na.last=TRUE), collapse=", "))
  logger.debug("AT has %s missing or out of range values", sum(!goodAT))
  if (sum(!goodAT) > 0) logger.debug("Bad AT values:  %s", paste0(sort(unique(df$Ambient.Temp..C.[!goodAT]),na.last=TRUE), collapse=", "))
  logger.debug("RHi has %s missing or out of range values", sum(!goodRHi))
  if (sum(!goodRHi) > 0) logger.debug("Bad RHi values:  %s", paste0(sort(unique(df$RH....[!goodRHi]),na.last=TRUE), collapse=", "))
  logger.debug("Conc has %s missing or out of range values", sum(!goodConcHr))
  if (sum(!goodConcHr) > 0) logger.debug("Bad Conc values:  %s", paste0(sort(unique(df$'Conc..\u00B5g.m3.'[!goodConcHr]),na.last=TRUE), collapse=", "))
  logger.debug("datetime has %s missing or out of range values", sum(!gooddatetime))
  if (sum(!gooddatetime) > 0) logger.debug("Bad datetime values:  %s", paste0(sort(unique(df$datetime[!gooddatetime]),na.last=TRUE), collapse=", "))
  
  goodMask <- goodFlow & goodAT & goodRHi & goodConcHr & gooddatetime
  badQCCount <- sum(!goodMask)
  
  if ( badQCCount > 0 ) {
    logger.info(paste(verb,"%s rows because of QC logic"), badQCCount)
    if ( flagAndKeep ) {
      # apply flags
      dfFlagged$QCFlag_badFlow[df$rowID[!goodFlow]] <- TRUE
      dfFlagged$QCFlag_badAT[df$rowID[!goodAT]] <- TRUE
      dfFlagged$QCFlag_badRHi[df$rowID[!goodRHi]] <- TRUE
      dfFlagged$QCFlag_badConcHr[df$rowID[!goodConcHr]] <- TRUE
      dfFlagged$QCFlag_badDateAndTime[df$rowID[!gooddatetime]] <- TRUE
      dfFlagged$QCFlag_anyBad <- (dfFlagged$QCFlag_anyBad | dfFlagged$QCFlag_badFlow | dfFlagged$QCFlag_badAT | 
                                    dfFlagged$QCFlag_badRHi | dfFlagged$QCFlag_badConcHr | dfFlagged$QCFlag_badDateAndTime)
      # apply reason codes
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodFlow]] <- paste(dfFlagged$QCFlag_reasonCode[df$rowID[!goodFlow]],"badFlow")
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodAT]] <- paste(dfFlagged$QCFlag_reasonCode[df$rowID[!goodAT]],"badAT")
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodRHi]] <- paste(dfFlagged$QCFlag_reasonCode[df$rowID[!goodRHi]],"badRHi")
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodConcHr]] <- paste(dfFlagged$QCFlag_reasonCode[df$rowID[!goodConcHr]],"badConcHr")
      dfFlagged$QCFlag_reasonCode[df$rowID[!gooddatetime]] <- paste(dfFlagged$QCFlag_reasonCode[df$rowID[!gooddatetime]],"badDateAndTime")
    }
  }
  
  df <- df[goodMask,]
  
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
    if ( flagAndKeep ) {
      # apply flags
      dfFlagged$QCFlag_duplicateHr[df$rowID[dupHrMask]] <- TRUE
      dfFlagged$QCFlag_anyBad <- dfFlagged$QCFlag_anyBad | dfFlagged$QCFlag_duplicateHr
      # apply reason code
      dfFlagged$QCFlag_reasonCode[df$rowID[dupHrMask]] <- paste(dfFlagged$QCFlag_reasonCode[df$rowID[dupHrMask]],"duplicateHr")
    }
  }
  
  df <- df[uniqueHrMask,]
  
  # ----- More QC -------------------------------------------------------------
  
  # NOTE:  Additional QC would go here
  
  if ( flagAndKeep ) {
    logger.debug("Retaining %d rows of measurements; %d bad rows flagged", nrow(df), sum(dfFlagged$QCFlag_anyBad))    
  } else {
    logger.debug("Retaining %d rows of validated measurements", nrow(df))
  }
  
  # ----- Final cleanup -------------------------------------------------------
  
  if ( flagAndKeep ) {
    dfFlagged$QCFlag_reasonCode <- stringr::str_sub(dfFlagged$QCFlag_reasonCode, 3)
    dfFlagged$QCFlag_reasonCode <- stringr::str_trim(dfFlagged$QCFlag_reasonCode)
    df <- dfFlagged
    df$rowID <- NULL
  }
    
  return(df)
  
}
