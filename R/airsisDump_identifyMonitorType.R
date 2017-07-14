#' @keywords internal
#' @export
#' @title Identify AIRSIS Dump File Monitor Type
#' @param df dataframe or raw character string containing AIRSIS data
#' @description Examine the column names of the incoming dataframe or the first line
#' of raw text to identify different types of monitor data provided by AIRSIS.
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
#' monitorTypeList <- airsisDump_identifyMonitorType(fileString)
#' }

airsisDump_identifyMonitorType <- function(df) {
  
  #     Different header styles     -------------------------------------------

  # USFS_bam1020.csv
  bam1020_header <- "Conc (\xb5/m3),Qtot (m3),WS (KTS),Ozone (ppb),RTM09 (mg3),RH (%),Ambient Temp (C),UnitID,Alias,Latitude,Longitude,TimeStamp"
  bam1020_header <- stringr::str_replace(bam1020_header,'\xb5','\u00B5')
  bam1020_rawNames <- unlist(stringr::str_split(bam1020_header, ','))
  bam1020_names <- make.names(bam1020_rawNames)
  bam1020_types <- 'ddddddddcddc'

  # USFS_esam.csv, APCD_esam.csv
  esam_header <- "Conc(mg/m3),Flow(l/m),AT(C),BP(PA),RHx(%),RHi(%),WS(M/S),WD(Deg),BV(V),Start Date/Time (GMT),Serial Number,System Volts,UnitID,Alias,Latitude,Longitude,TimeStamp"
  esam_rawNames <- unlist(stringr::str_split(esam_header, ','))
  esam_names <- make.names(esam_rawNames)
  esam_types <- 'dddddddddccdccddc'

  # USFS_ebam.csv
  ebam_header <- "Date/Time/GMT,COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,UnitID,Alias,Latitude,Longitude,TimeStamp"
  ebam_rawNames <- unlist(stringr::str_split(ebam_header, ','))
  ebam_names <- make.names(ebam_rawNames)
  ebam_types <- 'cddddddddddicccdccddc'
  
  # TCAPCD_ebam.csv
  ebam_header_2 <- "Date/Time/GMT,Start Date/Time (GMT),COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,UnitID,Alias,Latitude,Longitude,TimeStamp"
  ebam_rawNames_2 <- unlist(stringr::str_split(ebam_header_2, ','))
  ebam_names_2 <- make.names(ebam_rawNames_2)
  ebam_types_2 <- 'ccddddddddddicccdccddc'
  
  # WASHOE.csv
  ebam_header_3 <- "Date/Time/GMT,Conc (\xb5g/m3),Qtot (m3),COncRT,ConcHr,WS (KTS),Ozone (ppb),Flow,W/S,RTM09 (mg3),RH (%),W/D,AT,Ambient Temp (C),RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Start Date/Time (GMT),Sys. Volts,UnitID,Alias,Latitude,Longitude,TimeStamp"
  # NOTE:  make.names() complains with "invalid multibyte string 2" when unicode variables are present
  # NOTE:  Replace HTML hex entitity for 'MICRO' found in some header lines with unicode
  ebam_header_3 <- stringr::str_replace(ebam_header_3,'\xb5','\U00B5')
  ebam_rawNames_3 <- unlist(stringr::str_split(ebam_header_3, ','))
  ebam_names_3 <- make.names(ebam_rawNames_3)
  ebam_types_3 <- 'cdddddddddddddddddiccccdccddc'
  
  
  #     Determine file type     -----------------------------------------------
  
  # Get data column names from dataframe or character string
  if ( class(df)[1] == "character" ) {
    lines <- readr::read_lines(df)
    # NOTE:  make.names() complains with "invalid multibyte string 2" when unicode variables are present
    # NOTE:  Replace HTML hex entitity for 'MICRO' found in some header lines with 'u'
    line1 <- stringr::str_replace(lines[1],'\xb5','\u00BB5')
    colNames <- make.names(unlist(stringr::str_split(line1,',')))
  } else {
    colNames <- make.names(names(df))
  }
  
  # NOTE:  Use dplyr::setequal() to find all names from the standard header that are NOT in colNames.
  # NOTE:  If none are found, we have a match. Note that it is important to start with the
  # NOTE:  most detailed header and work our way toward the simpler ones.
  
  monitorType <- "UNKNOWN"
  rawNames <- vector('character')
  columnNames <- vector('character')
  columnTypes <- vector('character')
  if ( dplyr::setequal(bam1020_names, colNames) ) {
    monitorType <- "BAM1020"
    rawNames <- bam1020_rawNames
    columnNames <- bam1020_names
    columnTypes <- bam1020_types
  } else if ( dplyr::setequal(ebam_names, colNames) ) {
    monitorType <- "EBAM"
    rawNames <- ebam_rawNames
    columnNames <- ebam_names
    columnTypes <- ebam_types
  } else if ( dplyr::setequal(ebam_names_2, colNames) ) {
    monitorType <- "EBAM"
    rawNames <- ebam_rawNames_2
    columnNames <- ebam_names_2
    columnTypes <- ebam_types_2
  } else if ( dplyr::setequal(ebam_names_3, colNames) ) {
    monitorType <- "EBAM"
    rawNames <- ebam_rawNames_3
    columnNames <- ebam_names_3
    columnTypes <- ebam_types_3
  } else if ( dplyr::setequal(esam_names, colNames) ) {
    monitorType <- "ESAM"
    rawNames <- esam_rawNames
    columnNames <- esam_names
    columnTypes <- esam_types
  }
  
  monitorTypeList <- list(monitorType=monitorType,
                          rawNames=rawNames,
                          columnNames=columnNames,
                          columnTypes=columnTypes)
  
  return(monitorTypeList)
}
