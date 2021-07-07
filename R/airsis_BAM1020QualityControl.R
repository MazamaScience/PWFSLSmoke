#' @keywords AIRSIS
#' @export
#' @import MazamaCoreUtils
#'
#' @title Apply Quality Control to raw AIRSIS BAM1020 dataframe
#'
#' @param tbl single site tibble created by \code{airsis_parseData()}
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
#' @return Cleaned up tibble of AIRSIS monitor data.
#' @seealso \code{\link{airsis_qualityControl}}

airsis_BAM1020QualityControl <- function(
  tbl,
  valid_Longitude = c(-180,180),
  valid_Latitude = c(-90,90),
  remove_Lon_zero = TRUE,
  remove_Lat_zero = TRUE,
  valid_Flow = c(.834*.95,.834*1.05),
  valid_AT = c(-Inf,45),
  valid_RHi = c(-Inf,45),
  valid_Conc = c(-Inf,5000),
  flagAndKeep = FALSE
) {

  logger.debug(" ----- airsis_BAM1020QualityControl() ----- ")

  #  > names(tbl)
  #  [1] "MasterTable_ID"   "Alias"            "Latitude"         "Longitude"
  #  [5] "Conc..ug.m3."     "Qtot..m3."        "WS..KTS."         "Ozone..ppb."
  #  [9] "RTM09..mg3."      "RH...."           "Ambient.Temp..C." "TimeStamp"
  #  [13] "PDate"            "System.Volts"     "monitorName"      "monitorType"

  monitorName <- tbl$monitorName[1]

  # ----- Missing Values ------------------------------------------------------

  # Handle various missing value flags

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
    tblFlagged$QCFlag_badType <- FALSE
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
  if ( remove_Lon_zero ) {
    goodLonMask <- !is.na(tbl$Longitude) & (tbl$Longitude >= valid_Longitude[1]) & (tbl$Longitude <= valid_Longitude[2]) & (tbl$Longitude != 0)
  } else {
    goodLonMask <- !is.na(tbl$Longitude) & (tbl$Longitude >= valid_Longitude[1]) & (tbl$Longitude <= valid_Longitude[2])
  }

  if ( remove_Lat_zero ) {
    goodLatMask <- !is.na(tbl$Latitude) & (tbl$Latitude >= valid_Latitude[1]) & (tbl$Latitude <= valid_Latitude[2]) & (tbl$Latitude != 0)
  } else {
    goodLatMask <- !is.na(tbl$Latitude) & (tbl$Latitude >= valid_Latitude[1]) & (tbl$Latitude <= valid_Latitude[2])
  }

  badRows <- !(goodLonMask & goodLatMask)
  badRowCount <- sum(badRows)
  if ( badRowCount > 0 ) {
    logger.trace(paste(verb,"%s rows with invalid location information"), badRowCount)
    badLocations <- paste('(',tbl$Longitude[badRows],',',tbl$Latitude[badRows],')',sep = '')
    logger.trace("Bad locations: %s", unique(badLocations))
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

  # Sanity check -- row count
  if (nrow(tbl) < 1 && !flagAndKeep) {
    err_msg <- paste0("No valid PM2.5 data for ", monitorName)
    logger.warn(err_msg) # This is more of a warning than some error in the data.
    stop(err_msg, call. = FALSE)
  }

  # ----- Time ----------------------------------------------------------------

  # Add a POSIXct datetime
  tbl$datetime <- lubridate::floor_date(lubridate::mdy_hms(tbl$TimeStamp), unit = "hour") - lubridate::dhours(1)
  if ( flagAndKeep ) {
    # TODO: Unable to get datetime moved from tbl to tblFlagged without timezone and/or display getting messed up.
    # For now just duplicating the calculation, then assigning row values to NA after the fact for rows that were
    # removed from tbl prior to calculating datetime above. Clean up later if possible.
    tblFlagged$datetime <- lubridate::floor_date(lubridate::mdy_hms(tblFlagged$TimeStamp), unit = "hour") - lubridate::dhours(1)
    tblFlagged$datetime[ which(!(tblFlagged$rowID %in% tbl$rowID)) ] <- NA
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

  # NOTE:  Override ConcHr high value with 5000 as per conversation with Mike Broughton

  ### dat.2012arbbamraw$concHR <- ifelse(dat.2012arbbamraw$Qtot.m3.<.834*.95,NA,
  ###                                    ifelse(dat.2012arbbamraw$Qtot.m3.>.834*1.05,NA,
  ###                                    ifelse(dat.2012arbbamraw$IT.C.>45,NA,
  ###                                    ifelse(dat.2012arbbamraw$RH...> 45,NA,
  ###                                    ifelse(dat.2012arbbamraw$Conc.mg.<0,0,
  ###                                    ifelse(dat.2012arbbamraw$Conc.mg.>.984,NA,
  ###                                    ifelse(dat.2012arbbamraw$Delta.C.>25,NA,
  ###                                    dat.2012arbbamraw$Conc.mg.*1000)))))))

  # TODO: Consider logic to throw out pm25 data if temperature changes by more than 2 degC in adjacent hrs
  # TODO: (see NOTE in section 2.2 here: https://www.arb.ca.gov/airwebmanual/instrument_manuals/Documents/BAM-1020-9800%20Manual%20Rev%20G.ptbl)

  goodFlow <- !is.na(tbl$Qtot..m3.) & tbl$Qtot..m3. >= valid_Flow[1] & tbl$Qtot..m3. <= valid_Flow[2]
  goodAT <- !is.na(tbl$Ambient.Temp..C.) & tbl$Ambient.Temp..C. >= valid_AT[1] & tbl$Ambient.Temp..C. <= valid_AT[2]
  goodRHi <- !is.na(tbl$RH....) & tbl$RH.... >= valid_RHi[1] & tbl$RH.... <= valid_RHi[2]
  goodConcHr <- !is.na(tbl$'Conc..\u00B5g.m3.') & tbl$'Conc..\u00B5g.m3.' >= valid_Conc[1] & tbl$'Conc..\u00B5g.m3.' <= valid_Conc[2]
  gooddatetime <- !is.na(tbl$datetime) & tbl$datetime < lubridate::now(tzone = "UTC") # saw a future date once

  logger.trace("Flow has %s missing or out of range values", sum(!goodFlow))
  if (sum(!goodFlow) > 0) logger.trace("Bad Flow values:  %s", paste0(sort(unique(tbl$Qtot..m3.[!goodFlow]),na.last = TRUE), collapse = ", "))
  logger.trace("AT has %s missing or out of range values", sum(!goodAT))
  if (sum(!goodAT) > 0) logger.trace("Bad AT values:  %s", paste0(sort(unique(tbl$Ambient.Temp..C.[!goodAT]),na.last = TRUE), collapse = ", "))
  logger.trace("RHi has %s missing or out of range values", sum(!goodRHi))
  if (sum(!goodRHi) > 0) logger.trace("Bad RHi values:  %s", paste0(sort(unique(tbl$RH....[!goodRHi]),na.last = TRUE), collapse = ", "))
  logger.trace("Conc has %s missing or out of range values", sum(!goodConcHr))
  if (sum(!goodConcHr) > 0) logger.trace("Bad Conc values:  %s", paste0(sort(unique(tbl$'Conc..\u00B5g.m3.'[!goodConcHr]),na.last = TRUE), collapse = ", "))
  logger.trace("datetime has %s missing or out of range values", sum(!gooddatetime))
  if (sum(!gooddatetime) > 0) logger.trace("Bad datetime values:  %s", paste0(sort(unique(tbl$datetime[!gooddatetime]),na.last = TRUE), collapse = ", "))

  goodMask <- goodFlow & goodAT & goodRHi & goodConcHr & gooddatetime
  badQCCount <- sum(!goodMask)

  if ( badQCCount > 0 ) {
    logger.trace(paste(verb,"%s rows because of QC logic"), badQCCount)
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

  # Sanity check -- row count
  if (nrow(tbl) < 1 && !flagAndKeep) {
    err_msg <- paste0("No valid PM2.5 data for ", monitorName)
    logger.warn(err_msg) # This is more of a warning than some error in the data.
    stop(err_msg, call. = FALSE)
  }

  # ----- Duplicate Hours -----------------------------------------------------

  # For hours with multiple records, discard all but the one with the latest processing date/time
  # NOTE: Current setup for this section assumes that the last entry will be the latest one.  May
  # NOTE: want to build in functionality to ensure that the latest is picked if more than one exists
  # NOTE: (for example, if the data is not in order by timestamp for whatever reason)

  dupHrMask <- duplicated(tbl$datetime,fromLast = TRUE)
  dupHrCount <- sum(dupHrMask)
  uniqueHrMask <- !dupHrMask

  if ( dupHrCount > 0 ) {
    logger.trace(paste(verb,"%s duplicate time entries"), dupHrCount)
    logger.trace("Duplicate Hours (may be >1 per timestamp):  %s", paste0(sort(unique(tbl$TimeStamp[dupHrMask])), collapse = ", "))
    if ( flagAndKeep ) {
      # apply flags
      tblFlagged$QCFlag_duplicateHr[tbl$rowID[dupHrMask]] <- TRUE
      tblFlagged$QCFlag_anyBad <- tblFlagged$QCFlag_anyBad | tblFlagged$QCFlag_duplicateHr
      # apply reason code
      tblFlagged$QCFlag_reasonCode[tbl$rowID[dupHrMask]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[dupHrMask]],"duplicateHr")
    }
  }

  tbl <- tbl[uniqueHrMask,]

  # Sanity check -- row count
  if (nrow(tbl) < 1 && !flagAndKeep) {
    err_msg <- paste0("No valid PM2.5 data for ", monitorName)
    logger.warn(err_msg) # This is more of a warning than some error in the data.
    stop(err_msg, call. = FALSE)
  }

  # ----- More QC -------------------------------------------------------------

  # NOTE:  Additional QC would go here

  if ( flagAndKeep ) {
    logger.trace("Retaining %d rows of measurements; %d bad rows flagged", nrow(tbl), sum(tblFlagged$QCFlag_anyBad))
  } else {
    logger.trace("Retaining %d rows of validated measurements", nrow(tbl))
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
