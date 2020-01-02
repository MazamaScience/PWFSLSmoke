#' @keywords AIRSIS
#' @export
#' @import MazamaCoreUtils
#'
#' @title Identify AIRSIS monitor type
#'
#' @param df dataframe or raw character string containing AIRSIS data
#' @description Examine the column names of the incoming dataframe (or first line
#' of raw text) to identify different types of monitor data provided by AIRSIS.
#'
#' The return is a list includes everything needed to identify and parse the raw
#' data using \code{readr::read_csv()}:
#'
#' \itemize{
#' \item{\code{monitorType}}{ -- identification string}
#' \item{\code{rawNames}}{ -- column names from the data (including special characters)}
#' \item{\code{columnNames}}{ -- assigned column names (special characters repaced with '.')}
#' \item{\code{columnTypes}}{ -- column type string for use with \code{readr::read_csv()}}
#' }
#'
#' The \code{monitorType} will be one of:
#' \itemize{
#' \item{"\code{BAM1020}"}{ -- BAM1020 (e.g. USFS #49 in 2010)}
#' \item{"\code{EBAM}"}{ -- EBAM (e.g. USFS #1026 in 2010)}
#' \item{"\code{ESAM}"}{ -- E-Sampler (e.g. USFS #1002 in 2010)}
# \item{"\code{OTHER_1}"}{ -- Older EBAM 1 (e.g. USFS #70 in 2010)}
# \item{"\code{OTHER_2}"}{ -- Older EBAM 2 (e.g. APCD #4 in 2010)}
#' \item{"\code{UNKOWN}"}{ -- ???}
#' }
#'
#' @return List including \code{monitorType}, \code{rawNames}, \code{columnNames} and \code{columnTypes}.
#' @references \href{http://usfs.airsis.com}{Interagency Real Time Smoke Monitoring}
#' @examples
#' \dontrun{
#' fileString <- airsis_downloadData( 20150701, 20151231, provider='USFS', unitID='1026')
#' monitorTypeList <- airsis_identifyMonitorType(fileString)
#' }

airsis_identifyMonitorType <- function(df) {

  logger.debug(" ----- airsis_identifyMonitorType() ----- ")

  #     Different header styles     -------------------------------------------

  # ARB2_ebam+multi (Starting Jan 2020) : unitID = '1039', provider = 'arb2'
  # NOTE: WIP due to incompatible format on sever side.
  ebamMultiPlus_header <- "MasterTable_ID,Alias,Latitude,Longitude,Date/Time/GMT,Start,Date/Time,(GMT),COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial,Number,Version,Sys.,Volts,TimeStamp,PDate"
  ebamMultiPlus_rawNames <- unlist(stringr::str_split(ebamMultiPlus_header, ','))
  ebamMultiPlus_names <- make.names(ebamMultiPlus_rawNames)
  ebamMultiPlus_types <- 'ccdddddcddddddddddicdidddd'

  # ARB3_ebamMulti2 (Starting December, 2019)
  ebamMulti2_header <- "MasterTable_ID,Alias,Latitude,Longitude,ConcRT,ConcHR,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Status,TimeStamp,PDate"
  ebamMulti2_rawNames <- unlist(stringr::str_split(ebamMulti2_header, ','))
  ebamMulti2_names <- make.names(ebamMulti2_rawNames)
  ebamMulti2_types <- 'ccddddddddddddidccc'

  # ARB2_ebamMulti (Starting in August, 2018)
  ebamMulti_header <- "MasterTable_ID,Alias,Latitude,Longitude,COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Oceaneering Unit Voltage,Type,TimeStamp,PDate"
  ebamMulti_rawNames <- unlist(stringr::str_split(ebamMulti_header, ','))
  ebamMulti_names <- make.names(ebamMulti_rawNames)
  ebamMulti_types <- 'ccddddddddddddidccc'

  # provider=USFS, unitID=1002, year=2010
  esam_header <- "MasterTable_ID,Alias,Latitude,Longitude,Conc(mg/m3),Flow(l/m),AT(C),BP(PA),RHx(%),RHi(%),WS(M/S),WD(Deg),BV(V),Alarm,Start Date/Time (GMT),Serial Number,System Volts,Data 1,Data 2,TimeStamp,PDate"
  esam_rawNames <- unlist(stringr::str_split(esam_header, ','))
  esam_names <- make.names(esam_rawNames)
  esam_types <- 'ccdddddddddddiccdcccc'

  # provider=USFS, unitID=1026, year=2010
  ebam_header <- "MasterTable_ID,Alias,Latitude,Longitude,Date/Time/GMT,Start Date/Time (GMT),COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,TimeStamp,PDate"
  ebam_rawNames <- unlist(stringr::str_split(ebam_header, ','))
  ebam_names <- make.names(ebam_rawNames)
  ebam_types <- 'ccddccddddddddddicccdcc'

  # provider=USFS, unitID=49, year=2010
  bam1020_header <- "MasterTable_ID,Alias,Latitude,Longitude,Conc (\u00B5g/m3),Qtot (m3),WS (KTS),Ozone (ppb),RTM09 (mg3),RH (%),Ambient Temp (C),TimeStamp,PDate"
  bam1020_rawNames <- unlist(stringr::str_split(bam1020_header, ','))
  bam1020_names <- make.names(bam1020_rawNames)
  bam1020_types <- 'ccdddddddddcc'

  # provider=USFS, unitID=70, year=2010
  olderEbam_1_header <- "MasterTable_ID,Alias,Latitude,Longitude,Time,DataCol1,eBam,TimeStamp,PDate"
  olderEbam_1_rawNames <- unlist(stringr::str_split(olderEbam_1_header, ','))
  olderEbam_1_names <- make.names(olderEbam_1_rawNames)
  olderEbam_1_types <- 'ccddcdccc'

  # provider=APCD, unitID=4, year=2010
  olderEbam_2_header <- "MasterTable_ID,Alias,Latitude,Longitude,TimeStamp,PDate"
  olderEbam_2_rawNames <- unlist(stringr::str_split(olderEbam_2_header, ','))
  olderEbam_2_names <- make.names(olderEbam_2_rawNames)
  olderEbam_2_types <- 'ccddcc'

  # Need to duplicate these to handle the addition of the 'UnitID' column before 'Alias'

  # ARB2_ebamMulti (Starting in August, 2018)
  # HACK:  The ebamMulti type was hacked in to support an new data type from arb2.airsis.com. No unitID requests have ever worked with arb2.airsis.com

  # provider=USFS, unitID=1002, year=2010
  unitid_esam_header <- "MasterTable_ID,UnitID,Alias,Latitude,Longitude,Conc(mg/m3),Flow(l/m),AT(C),BP(PA),RHx(%),RHi(%),WS(M/S),WD(Deg),BV(V),Alarm,Start Date/Time (GMT),Serial Number,System Volts,Data 1,Data 2,TimeStamp,PDate"
  unitid_esam_rawNames <- unlist(stringr::str_split(unitid_esam_header, ','))
  unitid_esam_names <- make.names(unitid_esam_rawNames)
  unitid_esam_types <- 'cccdddddddddddiccdcccc'

  # provider=USFS, unitID=1026, year=2010
  unitid_ebam_header <- "MasterTable_ID,UnitID,Alias,Latitude,Longitude,Date/Time/GMT,Start Date/Time (GMT),COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,TimeStamp,PDate"
  unitid_ebam_rawNames <- unlist(stringr::str_split(unitid_ebam_header, ','))
  unitid_ebam_names <- make.names(unitid_ebam_rawNames)
  unitid_ebam_types <- 'cccddccddddddddddicccdcc'

  # provider=USFS, unitID=49, year=2010
  unitid_bam1020_header <- "MasterTable_ID,UnitID,Alias,Latitude,Longitude,Conc (\u00B5g/m3),Qtot (m3),WS (KTS),Ozone (ppb),RTM09 (mg3),RH (%),Ambient Temp (C),TimeStamp,PDate"
  unitid_bam1020_rawNames <- unlist(stringr::str_split(unitid_bam1020_header, ','))
  unitid_bam1020_names <- make.names(unitid_bam1020_rawNames)
  unitid_bam1020_types <- 'cccdddddddddcc'

  # provider=USFS, unitID=70, year=2010
  unitid_olderEbam_1_header <- "MasterTable_ID,UnitID,Alias,Latitude,Longitude,Time,DataCol1,eBam,TimeStamp,PDate"
  unitid_olderEbam_1_rawNames <- unlist(stringr::str_split(unitid_olderEbam_1_header, ','))
  unitid_olderEbam_1_names <- make.names(unitid_olderEbam_1_rawNames)
  unitid_olderEbam_1_types <- 'cccddcdccc'

  # provider=APCD, unitID=4, year=2010
  unitid_olderEbam_2_header <- "MasterTable_ID,UnitID,Alias,Latitude,Longitude,TimeStamp,PDate"
  unitid_olderEbam_2_rawNames <- unlist(stringr::str_split(unitid_olderEbam_2_header, ','))
  unitid_olderEbam_2_names <- make.names(unitid_olderEbam_2_rawNames)
  unitid_olderEbam_2_types <- 'cccddcc'



  #     Determine file type     -----------------------------------------------

  # Get data column names from dataframe or character string
  if ( class(df)[1] == "character" ) {
    lines <- readr::read_lines(df)
    colNames <- make.names(unlist(stringr::str_split(lines[1],',')))
  } else {
    colNames <- make.names(names(df))
  }

  # NOTE:  Use setdiff() to find all names from the standard header that are NOT in colNames.
  # NOTE:  If none are found, we have a match. Note that it is important to start with the
  # NOTE:  most detailed header and work our way toward the simpler ones.

  monitorType <- "UNKNOWN"
  monitorSubtype <- ""
  rawNames <- vector('character')
  columnNames <- vector('character')
  columnTypes <- vector('character')

  # NOTE:  Compare the more specific header lines first
  if ( dplyr::setequal(unitid_esam_names, colNames) ) {
    monitorType <- "ESAM"
    rawNames <- unitid_esam_rawNames
    columnNames <- unitid_esam_names
    columnTypes <- unitid_esam_types
  } else if ( dplyr::setequal(unitid_ebam_names, colNames) ) {
    monitorType <- "EBAM"
    rawNames <- unitid_ebam_rawNames
    columnNames <- unitid_ebam_names
    columnTypes <- unitid_ebam_types
  } else if ( dplyr::setequal(unitid_bam1020_names, colNames) ) {
    monitorType <- "BAM1020"
    rawNames <- unitid_bam1020_rawNames
    columnNames <- unitid_bam1020_names
    columnTypes <- unitid_bam1020_types

  # NOTE:  Now check for the older, pre-unitid headers in the same order
  } else if ( dplyr::setequal(esam_names, colNames) ) {
    monitorType <- "ESAM"
    rawNames <- esam_rawNames
    columnNames <- esam_names
    columnTypes <- esam_types
  } else if ( dplyr::setequal(ebamMulti_names, colNames) ) {
    monitorType <- "EBAM"
    monitorSubtype <- "MULTI"
    rawNames <- ebamMulti_rawNames
    columnNames <- ebamMulti_names
    columnTypes <- ebamMulti_types
  } else if ( dplyr::setequal(ebamMulti2_names, colNames) ) {
    monitorType <- "EBAM"
    monitorSubtype <- "MULTI2"
    rawNames <- ebamMulti2_rawNames
    columnNames <- ebamMulti2_names
    columnTypes <- ebamMulti2_types
  } else if ( dplyr::setequal(ebamMultiPlus_names, colNames) ) {
    monitorType <- "EBAM"
    monitorSubtype <- "MULTIPLUS"
    rawNames <- ebamMultiPlus_rawNames
    columnNames <- ebamMultiPlus_names
    columnTypes <- ebamMultiPlus_types
  } else if ( dplyr::setequal(ebam_names, colNames) ) {
    monitorType <- "EBAM"
    rawNames <- ebam_rawNames
    columnNames <- ebam_names
    columnTypes <- ebam_types
  } else if ( dplyr::setequal(bam1020_names, colNames) ) {
    monitorType <- "BAM1020"
    rawNames <- bam1020_rawNames
    columnNames <- bam1020_names
    columnTypes <- bam1020_types
  }

  monitorTypeList <- list(monitorType = monitorType,
                          monitorSubtype = monitorSubtype,
                          rawNames = rawNames,
                          columnNames = columnNames,
                          columnTypes = columnTypes)

  return(monitorTypeList)
}
