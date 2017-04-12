#' @keywords WRCC
#' @export
#' @title Apply Quality Control to Raw WRCC EBAM Dataframe
#' @param df single site dataframe created by \code{wrcc_parseData()}
#' @param valid_Longitude range of valid Longitude values
#' @param valid_Latitude range of valid Latitude values
#' @param remove_Lon_zero flag to remove rows where Longitude == 0
#' @param remove_Lat_zero flag to remove rows where Latitude == 0
#' @param valid_Flow range of valid Flow values
#' @param valid_AT range of valid AT values
#' @param valid_RHi range of valid RHi values
#' @param valid_Conc range of valid ConcHr values
#' @param flagAndKeep flag, rather than remove, bad data during the QC process
#' @description Perform various QC measures on WRCC EBAM data.
#' 
#' The any numeric values matching the following are converted to \code{NA}
#' \itemize{
#' \item{\code{x < -900}}
#' \item{\code{x == -9.9899}}
#' \item{\code{x == 99999}}
#' }
#' 
#' The following columns of data are tested against valid ranges:
#' \itemize{
#' \item{\code{Flow}}
#' \item{\code{AT}}
#' \item{\code{RHi}}
#' \item{\code{ConcHr}}
#' }
#' 
#' A \code{POSIXct datetime} column (UTC) is also added based on \code{DateTime}.
#' 
#' @return Cleaned up dataframe of WRCC monitor data.
#' @seealso \code{\link{wrcc_qualityControl}}

wrcc_EBAMQualityControl <- function(df,
                                    valid_Longitude=c(-180,180),
                                    valid_Latitude=c(-90,90),
                                    remove_Lon_zero = TRUE,
                                    remove_Lat_zero = TRUE,
                                    valid_Flow = c(16.7*0.95,16.7*1.05),
                                    valid_AT = c(-Inf,45),
                                    valid_RHi = c(-Inf,45),
                                    valid_Conc = c(-Inf,984),
                                    flagAndKeep = FALSE) {
  
  # TODO:  What about Alarm?
  
  # NOTE:  > names(df)
  # NOTE:   [1] "DateTime"       "GPSLat"         "GPSLon"         "Type"           "SerialNumber"   "ConcRT"        
  # NOTE:   [7] "Misc1"          "AvAirFlw"       "AvAirTemp"      "RelHumidity"    "Misc2"          "SensorIntAT"   
  # NOTE:  [13] "SensorIntRH"    "WindSpeed"      "WindDir"        "BatteryVoltage" "Alarm"          "monitorName"   
  # NOTE:  [19] "monitorType"   
  
  monitorName <- df$monitorName[1]
  
  # Setup for flagAndKeep argument utility
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
  
  # Handle various missing value flags (lots of variants of -99x???)
  df[df < -900] <- NA
  df[df == -9.9899] <- NA
  df[df == 99999] <- NA
  
  # ----- Location ------------------------------------------------------------
  
  # Latitude and longitude must be in range
  if (remove_Lon_zero) {
    goodLonMask <- !is.na(df$GPSLon) & (df$GPSLon >= valid_Longitude[1]) & (df$GPSLon <= valid_Longitude[2]) & (df$GPSLon != 0)
  } else {
    goodLonMask <- !is.na(df$GPSLon) & (df$GPSLon >= valid_Longitude[1]) & (df$GPSLon <= valid_Longitude[2])
  }
  
  if (remove_Lat_zero) {
    goodLatMask <- !is.na(df$GPSLat) & (df$GPSLat >= valid_Latitude[1]) & (df$GPSLat <= valid_Latitude[2]) & (df$GPSLat != 0)
  } else {    
    goodLatMask <- !is.na(df$GPSLat) & (df$GPSLat >= valid_Latitude[1]) & (df$GPSLat <= valid_Latitude[2])
  }
  
  badRows <- !(goodLonMask & goodLatMask)
  badRowCount <- sum(badRows)
  if ( badRowCount > 0 ) {
    logger.info(paste(verb,"%s rows with invalid location information"), badRowCount)
    badLocations <- paste('(',df$GPSLon[badRows],',',df$GPSLat[badRows],')',sep='')
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
  
  # TODO:  Convert longitudes <-10 to positive so they don't end up in China. (Is this still an issue?)
  
  # ----- Time ----------------------------------------------------------------
  
  # Add a POSIXct datetime based on YYmmddHHMM DateTime
  df$datetime <- parseDatetime(paste0('20',df$DateTime))
  if ( flagAndKeep ) {
    # TODO: Unable to get datetime moved from df to dfFlagged without timezone and/or display getting messed up.
    # For now just duplicating the calculation, then assigning row values to NA after the fact for rows that were
    # removed from df prior to calculating datetime above. Clean up later if possible.
    dfFlagged$datetime <- parseDatetime(paste0('20',dfFlagged$DateTime))
    dfFlagged$datetime[ which(!(dfFlagged$rowID %in% df$rowID)) ] <- NA
  }
  
  # ----- Type ----------------------------------------------------------------
  
  # Type: 0=E-BAM PM2.5, 1=E-BAM PM10, 9=E-Sampler. We only want PM2.5 measurements
  goodTypeMask <- !is.na(df$Type) & (df$Type == 0)
  
  badRows <- !goodTypeMask
  badRowCount <- sum(badRows)
  if ( badRowCount > 0 ) {
    logger.info(paste(verb,"%s rows with invalid Type information"), badRowCount)
    logger.debug("Bad Types:  %s", paste0(sort(df$Type[badRows]), collapse=", "))
    if ( flagAndKeep ) {
      # apply flags
      dfFlagged$QCFlag_badType[df$rowID[!goodTypeMask]] <- TRUE
      dfFlagged$QCFlag_anyBad <- dfFlagged$QCFlag_anyBad | dfFlagged$QCFlag_badType
      # apply reason code 
      dfFlagged$QCFlag_reasonCode[df$rowID[!goodTypeMask]] <- paste(dfFlagged$QCFlag_reasonCode[df$rowID[!goodTypeMask]],"badType")
    }
  }
  
  df <- df[goodTypeMask,]
  
  if (nrow(df) < 1) {
    logger.warn("No valid PM2.5 data for %s", monitorName)
    stop(paste0("No valid PM2.5 data for ", monitorName))
  }
  
  # Leland Tarnay QC for E-BAM ------------------------------------------------
  
  ###tmp.2014_YOSE_ebam1_ftp$concQA <- with(tmp.2014_YOSE_ebam1_ftp,
  ###                              ifelse(Flow < 16.7 * .95, "FlowLow",
  ###                              ifelse(Flow > 16.7 * 1.05, "FlowHigh",
  ###                              ifelse(AT > 45, "HighTemp",
  ###                              ifelse(RHi > 45,"HighRHi",
  ###                              ifelse(ConcHr < 0, "Negative",
  ###                              ifelse(ConcHr > .984, "HighConc", 'OK')))))))
  ###  
  ###tmp.2014_YOSE_ebam1_ftp$concHR <- with(tmp.2014_YOSE_ebam1_ftp,
  ###                             ifelse(concQA == 'Negative', 0,
  ###                             ifelse(concQA == 'OK', ConcHr * 1000 , NA)))
  
  goodFlow <- !is.na(df$AvAirFlw) & df$AvAirFlw >= valid_Flow[1] & df$AvAirFlw <= valid_Flow[2]
  goodAT <- !is.na(df$AvAirTemp) & df$AvAirTemp >= valid_AT[1] & df$AvAirTemp <= valid_AT[2]
  goodRHi <- !is.na(df$SensorIntRH) & df$SensorIntRH >= valid_RHi[1] & df$SensorIntRH <= valid_RHi[2]
  goodConcHr <- !is.na(df$ConcRT) & df$ConcRT >= valid_Conc[1] & df$ConcRT <= valid_Conc[2]
  gooddatetime <- !is.na(df$datetime) & df$datetime < lubridate::now("UTC") # saw a future date once
  
  logger.debug("Flow has %s missing or out of range values", sum(!goodFlow))
  if (sum(!goodFlow) > 0) logger.debug("Bad Flow values:  %s", paste0(sort(df$AvAirFlw[!goodFlow]), collapse=", "))
  logger.debug("AT has %s missing or out of range values", sum(!goodAT))
  if (sum(!goodAT) > 0) logger.debug("Bad AT values:  %s", paste0(sort(df$AvAirTemp[!goodAT]), collapse=", "))
  logger.debug("RHi has %s missing or out of range values", sum(!goodRHi))
  if (sum(!goodRHi) > 0) logger.debug("Bad RHi values:  %s", paste0(sort(df$SensorIntRH[!goodRHi]), collapse=", "))
  logger.debug("Conc has %s missing or out of range values", sum(!goodConcHr))
  if (sum(!goodConcHr) > 0) logger.debug("Bad Conc values:  %s", paste0(sort(df$ConcRT[!goodConcHr]), collapse=", "))
  logger.debug("datetime has %s missing or out of range values", sum(!gooddatetime))
  if (sum(!gooddatetime) > 0) logger.debug("Bad datetime values:  %s", paste0(sort(df$datetime[!gooddatetime]), collapse=", "))
  
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
      dfFlagged$QCFlag_duplicateHr[df$rowID[!dupHrMask]] <- TRUE
      dfFlagged$QCFlag_anyBad <- dfFlagged$QCFlag_anyBad | dfFlagged$QCFlag_duplicateHr
      # apply reason code
      dfFlagged$QCFlag_reasonCode[df$rowID[!dupHrMask]] <- paste(dfFlagged$QCFlag_reasonCode[df$rowID[!dupHrMask]],"duplicateHr")
    }
  }
  
  df <- df[uniqueHrMask,]
  
  # ----- More QC -------------------------------------------------------------
  
  # TODO:  Other QC?
  
  # TODO:  Multiply ConcRT by 1000?
  
  if ( flagAndKeep ) {
    logger.debug("Retaining %d rows of measurements; %d bad rows flagged", nrow(df), sum(dfFlagged$QCFlag_anyBad))    
  } else {
    logger.debug("Retaining %d rows of validated measurements", nrow(df))
  }
  
  # ----- Final cleanup -------------------------------------------------------
  
  if ( flagAndKeep ) {
    dfFlagged$QCFlag_reasonCode <- stringr::str_trim(dfFlagged$QCFlag_reasonCode)
    dfFlagged$QCFlag_reasonCode <- stringr::str_sub(dfFlagged$QCFlag_reasonCode, 3)
    df <- dfFlagged
    df$rowID <- NULL
  }
  
  return(df)
  
}
