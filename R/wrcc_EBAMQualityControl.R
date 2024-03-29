#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Apply Quality Control to raw WRCC EBAM tibble
#'
#' @param tbl Single site titbble created by \code{wrcc_parseData()}.
#' @param valid_Longitude Range of valid Longitude values.
#' @param valid_Latitude Range of valid Latitude values.
#' @param remove_Lon_zero Logical specifying removal of rows where Longitude == 0.
#' @param remove_Lat_zero Logical specifying removal of rows where Latitude == 0.
#' @param valid_Flow Range of valid Flow values.
#' @param valid_AT Range of valid AT values.
#' @param valid_RHi Range of valid RHi values.
#' @param valid_Conc Range of valid ConcHr values.
#' @param flagAndKeep Logical specifying flagging, rather than removal, of bad
#' data during the QC process.
#'
#' @description Perform various QC measures on WRCC EBAM data.
#'
#' Any numeric values matching the following are converted to \code{NA}
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
#' @return Cleaned up tibble of WRCC monitor data.
#' @seealso \code{\link{wrcc_qualityControl}}

wrcc_EBAMQualityControl <- function(
  tbl,
  valid_Longitude = c(-180, 180),
  valid_Latitude = c(-90, 90),
  remove_Lon_zero = TRUE,
  remove_Lat_zero = TRUE,
  valid_Flow = c(16.7*0.95, 16.7*1.05),
  valid_AT = c(-Inf, 45),
  # NOTE:  2021-07-07 Update RHi from 45 -> 50 as per conversation with Pete Lahm
  valid_RHi = c(-Inf, 50),
  # NOTE:  Update ConcHr from 984 -> 5000 as per conversation with Mike Broughton
  valid_Conc = c(-Inf, 5000),
  flagAndKeep = FALSE
) {

  logger.debug(" ----- wrcc_EBAMQualityControl() ----- ")

  # TODO:  What about Alarm?

  # NOTE:  > names(tbl)
  # NOTE:   [1] "DateTime"       "GPSLat"         "GPSLon"         "Type"           "SerialNumber"   "ConcRT"
  # NOTE:   [7] "Misc1"          "AvAirFlw"       "AvAirTemp"      "RelHumidity"    "Misc2"          "SensorIntAT"
  # NOTE:  [13] "SensorIntRH"    "WindSpeed"      "WindDir"        "BatteryVoltage" "Alarm"          "monitorName"
  # NOTE:  [19] "monitorType"

  monitorName <- tbl$monitorName[1]

  # ----- Missing Values -------------------------------------------------------

  # Handle various missing value flags (lots of variants of -99x???)
  tbl[tbl < -900] <- NA
  tbl[tbl == -9.9899] <- NA
  tbl[tbl == 99999] <- NA

  # ----- Setup for flagAndKeep argument utility -------------------------------

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

  # ----- Location -------------------------------------------------------------

  # Latitude and longitude must be in range
  if ( remove_Lon_zero ) {
    goodLonMask <- !is.na(tbl$GPSLon) & (tbl$GPSLon >= valid_Longitude[1]) & (tbl$GPSLon <= valid_Longitude[2]) & (tbl$GPSLon != 0)
  } else {
    goodLonMask <- !is.na(tbl$GPSLon) & (tbl$GPSLon >= valid_Longitude[1]) & (tbl$GPSLon <= valid_Longitude[2])
  }

  if ( remove_Lat_zero ) {
    goodLatMask <- !is.na(tbl$GPSLat) & (tbl$GPSLat >= valid_Latitude[1]) & (tbl$GPSLat <= valid_Latitude[2]) & (tbl$GPSLat != 0)
  } else {
    goodLatMask <- !is.na(tbl$GPSLat) & (tbl$GPSLat >= valid_Latitude[1]) & (tbl$GPSLat <= valid_Latitude[2])
  }

  badRows <- !(goodLonMask & goodLatMask)
  badRowCount <- sum(badRows)

  if ( badRowCount > 0 ) {
    logger.trace(paste(verb, "%s rows with invalid location information"), badRowCount)
    badLocations <- paste('(', tbl$GPSLon[badRows], ',', tbl$GPSLat[badRows], ')', sep = '')
    logger.trace("Bad locations: %s", paste0(badLocations, collapse = ", "))
    if ( flagAndKeep ) {
      # apply flags
      tblFlagged$QCFlag_badLon[tbl$rowID[!goodLonMask]] <- TRUE
      tblFlagged$QCFlag_badLat[tbl$rowID[!goodLatMask]] <- TRUE
      tblFlagged$QCFlag_anyBad <- tblFlagged$QCFlag_anyBad | tblFlagged$QCFlag_badLon | tblFlagged$QCFlag_badLat
      # apply reason codes
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodLonMask]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodLonMask]], "badLon")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodLatMask]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodLatMask]], "badLat")
    }
  }

  tbl <- tbl[goodLonMask & goodLatMask,]

  # ----- Time -----------------------------------------------------------------

  # Add a POSIXct datetime based on YYmmddHHMM DateTime
  tbl$datetime <- MazamaCoreUtils::parseDatetime(paste0('20', tbl$DateTime), timezone = "UTC")
  if ( flagAndKeep ) {
    # TODO: Unable to get datetime moved from tbl to tblFlagged without timezone and/or display getting messed up.
    # For now just duplicating the calculation, then assigning row values to NA after the fact for rows that were
    # removed from tbl prior to calculating datetime above. Clean up later if possible.
    tblFlagged$datetime <- MazamaCoreUtils::parseDatetime(paste0('20', tblFlagged$DateTime), timezone = "UTC")
    tblFlagged$datetime[ which(!(tblFlagged$rowID %in% tbl$rowID)) ] <- NA
  }

  # ----- Type -----------------------------------------------------------------

  # Type: 0=E-BAM PM2.5, 1=E-BAM PM10, 9=E-Sampler. We only want PM2.5 measurements
  goodTypeMask <- !is.na(tbl$Type) & (tbl$Type == 0)

  badRows <- !goodTypeMask
  badRowCount <- sum(badRows)
  if ( badRowCount > 0 ) {
    logger.trace(paste(verb, "%s rows with invalid Type information"), badRowCount)
    logger.trace("Bad Types:  %s", paste0(sort(unique(tbl$Type[badRows]), na.last = TRUE), collapse = ", "))
    if ( flagAndKeep ) {
      # apply flags
      tblFlagged$QCFlag_badType[tbl$rowID[!goodTypeMask]] <- TRUE
      tblFlagged$QCFlag_anyBad <- tblFlagged$QCFlag_anyBad | tblFlagged$QCFlag_badType
      # apply reason code
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodTypeMask]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodTypeMask]], "badType")
    }
  }

  tbl <- tbl[goodTypeMask,]

  if (nrow(tbl) < 1) {
    logger.warn("No valid PM2.5 data for %s", monitorName)
  }

  # Leland Tarnay QC for E-BAM -------------------------------------------------

  # NOTE:  2022-09-07 Conversation with Amber Ortega: Newer E-BAMs can be run
  # NOTE:  at a flow rate of 12.5. So either ~16.7 or ~12.5 should be considered
  # NOTE:  valid. I didn't want to modify the function signature to support this
  # NOTE:  So I'm just adding it in here.

  goodFlow1 <- !is.na(tbl$AvAirFlw) & tbl$AvAirFlw >= valid_Flow[1] & tbl$AvAirFlw <= valid_Flow[2]
  goodFlow2 <- !is.na(tbl$AvAirFlw) & tbl$AvAirFlw >= 12.5*0.95 & tbl$AvAirFlw <= 12.5*1.05
  goodFlow <- goodFlow1 | goodFlow2

  goodAT <- !is.na(tbl$AvAirTemp) & tbl$AvAirTemp >= valid_AT[1] & tbl$AvAirTemp <= valid_AT[2]
  goodRHi <- !is.na(tbl$SensorIntRH) & tbl$SensorIntRH >= valid_RHi[1] & tbl$SensorIntRH <= valid_RHi[2]
  goodConcHr <- !is.na(tbl$ConcRT) & tbl$ConcRT >= valid_Conc[1] & tbl$ConcRT <= valid_Conc[2]
  gooddatetime <- !is.na(tbl$datetime) & tbl$datetime < lubridate::now(tzone = "UTC") # saw a future date once

  logger.trace("Flow has %s missing or out of range values", sum(!goodFlow))
  if (sum(!goodFlow) > 0) logger.trace("Bad Flow values:  %s", paste0(sort(unique(tbl$AvAirFlw[!goodFlow]), na.last = TRUE), collapse = ", "))
  logger.trace("AT has %s missing or out of range values", sum(!goodAT))
  if (sum(!goodAT) > 0) logger.trace("Bad AT values:  %s", paste0(sort(unique(tbl$AvAirTemp[!goodAT]), na.last = TRUE), collapse = ", "))
  logger.trace("RHi has %s missing or out of range values", sum(!goodRHi))
  if (sum(!goodRHi) > 0) logger.trace("Bad RHi values:  %s", paste0(sort(unique(tbl$SensorIntRH[!goodRHi]), na.last = TRUE), collapse = ", "))
  logger.trace("Conc has %s missing or out of range values", sum(!goodConcHr))
  if (sum(!goodConcHr) > 0) logger.trace("Bad Conc values:  %s", paste0(sort(unique(tbl$ConcRT[!goodConcHr]), na.last = TRUE), collapse = ", "))
  logger.trace("datetime has %s missing or out of range values", sum(!gooddatetime))
  if (sum(!gooddatetime) > 0) logger.trace("Bad datetime values:  %s", paste0(sort(unique(tbl$datetime[!gooddatetime]), na.last = TRUE), collapse = ", "))

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
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodFlow]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodFlow]], "badFlow")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodAT]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodAT]], "badAT")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodRHi]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodRHi]], "badRHi")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodConcHr]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodConcHr]], "badConcHr")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!gooddatetime]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!gooddatetime]], "badDateAndTime")
    }
  }

  tbl <- tbl[goodMask,]

  # ----- Duplicate Hours ------------------------------------------------------

  # For hours with multiple records, discard all but the one with the latest processing date/time
  # NOTE: Current setup for this section assumes that the last entry will be the latest one.  May
  # NOTE: want to build in functionality to ensure that the latest is picked if more than one exists
  # NOTE: (for example, if the data is not in order by timestamp for whatever reason)

  dupHrMask <- duplicated(tbl$datetime, fromLast = TRUE)
  dupHrCount <- sum(dupHrMask)
  uniqueHrMask <- !dupHrMask

  if ( dupHrCount > 0 ) {
    logger.trace(paste(verb, "%s duplicate time entries"), dupHrCount)
    logger.trace("Duplicate Hours (may be >1 per timestamp):  %s", paste0(sort(unique(tbl$TimeStamp[dupHrMask])), collapse = ", "))
    if ( flagAndKeep ) {
      # apply flags
      tblFlagged$QCFlag_duplicateHr[tbl$rowID[dupHrMask]] <- TRUE
      tblFlagged$QCFlag_anyBad <- tblFlagged$QCFlag_anyBad | tblFlagged$QCFlag_duplicateHr
      # apply reason code
      tblFlagged$QCFlag_reasonCode[tbl$rowID[dupHrMask]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[dupHrMask]], "duplicateHr")
    }
  }

  tbl <- tbl[uniqueHrMask,]

  # ----- More QC --------------------------------------------------------------

  # Lift negative concentrations to zero
  tbl$ConcRT[tbl$ConcRT < 0] <- 0

  if ( flagAndKeep ) {
    logger.trace("Retaining %d rows of measurements; %d bad rows flagged", nrow(tbl), sum(tblFlagged$QCFlag_anyBad))
  } else {
    logger.trace("Retaining %d rows of validated measurements", nrow(tbl))
  }

  # ----- Final cleanup --------------------------------------------------------

  if ( flagAndKeep ) {
    tblFlagged$QCFlag_reasonCode <- stringr::str_sub(tblFlagged$QCFlag_reasonCode, 3)
    tblFlagged$QCFlag_reasonCode <- stringr::str_trim(tblFlagged$QCFlag_reasonCode)
    tbl <- tblFlagged
    tbl$rowID <- NULL
  }

  # ----- Return ---------------------------------------------------------------

  return(tbl)

}
