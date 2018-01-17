#' @keywords WRCC
#' @export
#' @title Apply Quality Control to Raw WRCC EBAM Titbble
#' @param tbl single site titbble created by \code{wrcc_parseData()}
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
#' @return Cleaned up titbble of WRCC monitor data.
#' @seealso \code{\link{wrcc_qualityControl}}

wrcc_EBAMQualityControl <- function(tbl,
                                    valid_Longitude=c(-180,180),
                                    valid_Latitude=c(-90,90),
                                    remove_Lon_zero = TRUE,
                                    remove_Lat_zero = TRUE,
                                    valid_Flow = c(16.7*0.95,16.7*1.05),
                                    valid_AT = c(-Inf,45),
                                    valid_RHi = c(-Inf,45),
                                    valid_Conc = c(-Inf,5000),
                                    flagAndKeep = FALSE) {
  
  # TODO:  What about Alarm?
  
  # NOTE:  > names(tbl)
  # NOTE:   [1] "DateTime"       "GPSLat"         "GPSLon"         "Type"           "SerialNumber"   "ConcRT"        
  # NOTE:   [7] "Misc1"          "AvAirFlw"       "AvAirTemp"      "RelHumidity"    "Misc2"          "SensorIntAT"   
  # NOTE:  [13] "SensorIntRH"    "WindSpeed"      "WindDir"        "BatteryVoltage" "Alarm"          "monitorName"   
  # NOTE:  [19] "monitorType"   
  
  monitorName <- tbl$monitorName[1]
  
  # ----- Missing Values ------------------------------------------------------
  
  # Handle various missing value flags (lots of variants of -99x???)
  tbl[tbl < -900] <- NA
  tbl[tbl == -9.9899] <- NA
  tbl[tbl == 99999] <- NA
  
  # ----- Setup for flagAndKeep argument utility ------------------------------
  
  if ( flagAndKeep ) {
    # verb for logging messages
    verb <- "Flagging"
    
    tbl$rowID <- as.integer(rownames(tbl))
    
    # duplicate tbl and add columns for flags
    tblFlagged <- tbl
    tblFlagged$QCFlag_anyBad <- FALSE
    tblFlagged$QCFlag_reasonCode <- NA
    tblFlagged$QCFlag_badLon <- FALSE
    tblFlagged$QCFlag_badLat <- FALSE
    tblFlagged$QCFlag_badType <- FALSE # no type info for ESAMs
    tblFlagged$QCFlag_badFlow <- FALSE
    tblFlagged$QCFlag_badAT <- FALSE
    tblFlagged$QCFlag_badRHi <- FALSE
    tblFlagged$QCFlag_badConcHr <- FALSE
    tblFlagged$QCFlag_badDateAndTime <- FALSE
    tblFlagged$QCFlag_duplicateHr <- FALSE
  } else {
    # verb for logging messages
    verb <- "Discarding"
  }
  

  
  # ----- Location ------------------------------------------------------------
  
  # Latitude and longitude must be in range
  if (remove_Lon_zero) {
    goodLonMask <- !is.na(tbl$GPSLon) & (tbl$GPSLon >= valid_Longitude[1]) & (tbl$GPSLon <= valid_Longitude[2]) & (tbl$GPSLon != 0)
  } else {
    goodLonMask <- !is.na(tbl$GPSLon) & (tbl$GPSLon >= valid_Longitude[1]) & (tbl$GPSLon <= valid_Longitude[2])
  }
  
  if (remove_Lat_zero) {
    goodLatMask <- !is.na(tbl$GPSLat) & (tbl$GPSLat >= valid_Latitude[1]) & (tbl$GPSLat <= valid_Latitude[2]) & (tbl$GPSLat != 0)
  } else {    
    goodLatMask <- !is.na(tbl$GPSLat) & (tbl$GPSLat >= valid_Latitude[1]) & (tbl$GPSLat <= valid_Latitude[2])
  }
  
  badRows <- !(goodLonMask & goodLatMask)
  badRowCount <- sum(badRows)
  if ( badRowCount > 0 ) {
    logger.debug(paste(verb,"%s rows with invalid location information"), badRowCount)
    badLocations <- paste('(',tbl$GPSLon[badRows],',',tbl$GPSLat[badRows],')',sep='')
    logger.debug("Bad locations: %s", paste0(badLocations, collapse=", "))
    if ( flagAndKeep ) {
      # apply flags
      tblFlagged$QCFlag_badLon[tbl$rowID[!goodLonMask]] <- TRUE
      tblFlagged$QCFlag_badLat[tbl$rowID[!goodLatMask]] <- TRUE
      tblFlagged$QCFlag_anyBad <- tblFlagged$QCFlag_anyBad | tblFlagged$QCFlag_badLon | tblFlagged$QCFlag_badLat
      # apply reason codes
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodLonMask]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodLonMask]],"badLon")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodLatMask]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodLatMask]],"badLat")
    }
  }
  
  tbl <- tbl[goodLonMask & goodLatMask,]
  
  # ----- Time ----------------------------------------------------------------
  
  # Add a POSIXct datetime based on YYmmddHHMM DateTime
  tbl$datetime <- parseDatetime(paste0('20',tbl$DateTime))
  if ( flagAndKeep ) {
    # TODO: Unable to get datetime moved from tbl to tblFlagged without timezone and/or display getting messed up.
    # For now just duplicating the calculation, then assigning row values to NA after the fact for rows that were
    # removed from tbl prior to calculating datetime above. Clean up later if possible.
    tblFlagged$datetime <- parseDatetime(paste0('20',tblFlagged$DateTime))
    tblFlagged$datetime[ which(!(tblFlagged$rowID %in% tbl$rowID)) ] <- NA
  }
  
  # ----- Type ----------------------------------------------------------------
  
  # Type: 0=E-BAM PM2.5, 1=E-BAM PM10, 9=E-Sampler. We only want PM2.5 measurements
  goodTypeMask <- !is.na(tbl$Type) & (tbl$Type == 0)
  
  badRows <- !goodTypeMask
  badRowCount <- sum(badRows)
  if ( badRowCount > 0 ) {
    logger.debug(paste(verb,"%s rows with invalid Type information"), badRowCount)
    logger.debug("Bad Types:  %s", paste0(sort(unique(tbl$Type[badRows]),na.last=TRUE), collapse=", "))
    if ( flagAndKeep ) {
      # apply flags
      tblFlagged$QCFlag_badType[tbl$rowID[!goodTypeMask]] <- TRUE
      tblFlagged$QCFlag_anyBad <- tblFlagged$QCFlag_anyBad | tblFlagged$QCFlag_badType
      # apply reason code 
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodTypeMask]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodTypeMask]],"badType")
    }
  }
  
  tbl <- tbl[goodTypeMask,]
  
  if (nrow(tbl) < 1) {
    logger.warn("No valid PM2.5 data for %s", monitorName)
  }
  
  # Leland Tarnay QC for E-BAM ------------------------------------------------
  
  # NOTE:  Override ConcHr high value with 5000 as per conversation with Mike Broughton

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
  
  goodFlow <- !is.na(tbl$AvAirFlw) & tbl$AvAirFlw >= valid_Flow[1] & tbl$AvAirFlw <= valid_Flow[2]
  goodAT <- !is.na(tbl$AvAirTemp) & tbl$AvAirTemp >= valid_AT[1] & tbl$AvAirTemp <= valid_AT[2]
  goodRHi <- !is.na(tbl$SensorIntRH) & tbl$SensorIntRH >= valid_RHi[1] & tbl$SensorIntRH <= valid_RHi[2]
  goodConcHr <- !is.na(tbl$ConcRT) & tbl$ConcRT >= valid_Conc[1] & tbl$ConcRT <= valid_Conc[2]
  gooddatetime <- !is.na(tbl$datetime) & tbl$datetime < lubridate::now("UTC") # saw a future date once
  
  logger.debug("Flow has %s missing or out of range values", sum(!goodFlow))
  if (sum(!goodFlow) > 0) logger.debug("Bad Flow values:  %s", paste0(sort(unique(tbl$AvAirFlw[!goodFlow]),na.last=TRUE), collapse=", "))
  logger.debug("AT has %s missing or out of range values", sum(!goodAT))
  if (sum(!goodAT) > 0) logger.debug("Bad AT values:  %s", paste0(sort(unique(tbl$AvAirTemp[!goodAT]),na.last=TRUE), collapse=", "))
  logger.debug("RHi has %s missing or out of range values", sum(!goodRHi))
  if (sum(!goodRHi) > 0) logger.debug("Bad RHi values:  %s", paste0(sort(unique(tbl$SensorIntRH[!goodRHi]),na.last=TRUE), collapse=", "))
  logger.debug("Conc has %s missing or out of range values", sum(!goodConcHr))
  if (sum(!goodConcHr) > 0) logger.debug("Bad Conc values:  %s", paste0(sort(unique(tbl$ConcRT[!goodConcHr]),na.last=TRUE), collapse=", "))
  logger.debug("datetime has %s missing or out of range values", sum(!gooddatetime))
  if (sum(!gooddatetime) > 0) logger.debug("Bad datetime values:  %s", paste0(unique(sort(tbl$datetime[!gooddatetime]),na.last=TRUE), collapse=", "))
  
  goodMask <- goodFlow & goodAT & goodRHi & goodConcHr & gooddatetime
  badQCCount <- sum(!goodMask)
  
  if ( badQCCount > 0 ) {
    logger.debug(paste(verb,"%s rows because of QC logic"), badQCCount)
    if ( flagAndKeep ) {
      # apply flags
      tblFlagged$QCFlag_badFlow[tbl$rowID[!goodFlow]] <- TRUE
      tblFlagged$QCFlag_badAT[tbl$rowID[!goodAT]] <- TRUE
      tblFlagged$QCFlag_badRHi[tbl$rowID[!goodRHi]] <- TRUE
      tblFlagged$QCFlag_badConcHr[tbl$rowID[!goodConcHr]] <- TRUE
      tblFlagged$QCFlag_badDateAndTime[tbl$rowID[!gooddatetime]] <- TRUE
      tblFlagged$QCFlag_anyBad <- (tblFlagged$QCFlag_anyBad | tblFlagged$QCFlag_badFlow | tblFlagged$QCFlag_badAT | 
                                    tblFlagged$QCFlag_badRHi | tblFlagged$QCFlag_badConcHr | tblFlagged$QCFlag_badDateAndTime)
      # apply reason codes
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodFlow]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodFlow]],"badFlow")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodAT]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodAT]],"badAT")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodRHi]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodRHi]],"badRHi")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodConcHr]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodConcHr]],"badConcHr")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!gooddatetime]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!gooddatetime]],"badDateAndTime")
    }
  }

  tbl <- tbl[goodMask,]
  
  # ----- Duplicate Hours -----------------------------------------------------
  
  # For hours with multiple records, discard all but the one with the latest processing date/time
  # NOTE: Current setup for this section assumes that the last entry will be the latest one.  May 
  # NOTE: want to build in functionality to ensure that the latest is picked if more than one exists
  # NOTE: (for example, if the data is not in order by timestamp for whatever reason)
  
  dupHrMask <- duplicated(tbl$datetime,fromLast = TRUE)
  dupHrCount <- sum(dupHrMask)
  uniqueHrMask <- !dupHrMask
  
  if ( dupHrCount > 0 ) {
    logger.debug(paste(verb,"%s duplicate time entries"), dupHrCount)
    logger.debug("Duplicate Hours (may be >1 per timestamp):  %s", paste0(sort(unique(tbl$TimeStamp[dupHrMask])), collapse=", "))
    if ( flagAndKeep ) {
      # apply flags
      tblFlagged$QCFlag_duplicateHr[tbl$rowID[dupHrMask]] <- TRUE
      tblFlagged$QCFlag_anyBad <- tblFlagged$QCFlag_anyBad | tblFlagged$QCFlag_duplicateHr
      # apply reason code
      tblFlagged$QCFlag_reasonCode[tbl$rowID[dupHrMask]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[dupHrMask]],"duplicateHr")
    }
  }
  
  tbl <- tbl[uniqueHrMask,]
  
  # ----- More QC -------------------------------------------------------------
  
  # NOTE:  Additional QC would go here
  
  if ( flagAndKeep ) {
    logger.debug("Retaining %d rows of measurements; %d bad rows flagged", nrow(tbl), sum(tblFlagged$QCFlag_anyBad))    
  } else {
    logger.debug("Retaining %d rows of validated measurements", nrow(tbl))
  }
  
  # ----- Final cleanup -------------------------------------------------------
  
  if ( flagAndKeep ) {
    tblFlagged$QCFlag_reasonCode <- stringr::str_sub(tblFlagged$QCFlag_reasonCode, 3)
    tblFlagged$QCFlag_reasonCode <- stringr::str_trim(tblFlagged$QCFlag_reasonCode)
    tbl <- tblFlagged
    tbl$rowID <- NULL
  }
  
  return(tbl)
  
}
