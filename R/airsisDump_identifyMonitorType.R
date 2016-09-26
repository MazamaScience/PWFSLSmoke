#' @keywords AIRSIS
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
#' fileString <- readr::read_file(/home/monitors/USFS_ebam.csv')
#' monitorTypeList <- airsisDump_identifyMonitorType(fileString)
#' }

airsisDump_identifyMonitorType <- function(df) {
  
  #     Different header styles     -------------------------------------------
  
  # provider=USFS, unitID=1002, year=2010
  esam_header <- "Conc(mg/m3),Flow(l/m),AT(C),BP(PA),RHx(%),RHi(%),WS(M/S),WD(Deg),BV(V),Start Date/Time (GMT),Serial Number,System Volts,UnitID,Alias,Latitude,Longitude,TimeStamp"
  esam_rawNames <- unlist(stringr::str_split(esam_header, ','))
  esam_names <- make.names(esam_rawNames)
  esam_types <- 'dddddddddccdccddc'
  
  # provider=USFS, unitID=1026, year=2010
  ebam_header <- "Date/Time/GMT,COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,UnitID,Alias,Latitude,Longitude,TimeStamp"
  ebam_rawNames <- unlist(stringr::str_split(ebam_header, ','))
  ebam_names <- make.names(ebam_rawNames)
  ebam_types <- 'cddddddddddicccdccddc'
  
#   # provider=USFS, unitID=49, year=2010
#   bam1020_header <- "MasterTable_ID,Alias,Latitude,Longitude,Conc (\u00B5g/m3),Qtot (m3),WS (KTS),Ozone (ppb),RTM09 (mg3),RH (%),Ambient Temp (C),TimeStamp,PDate"
#   bam1020_rawNames <- unlist(stringr::str_split(bam1020_header, ','))
#   bam1020_names <- make.names(bam1020_rawNames)
#   bam1020_types <- 'ccdddddddddcc'
#   
#   # provider=USFS, unitID=70, year=2010
#   olderEbam_1_header <- "MasterTable_ID,Alias,Latitude,Longitude,Time,DataCol1,eBam,TimeStamp,PDate"
#   olderEbam_1_rawNames <- unlist(stringr::str_split(olderEbam_1_header, ','))
#   olderEbam_1_names <- make.names(olderEbam_1_rawNames)
#   olderEbam_1_types <- 'ccddcdccc'
#   
#   # provider=APCD, unitID=4, year=2010
#   olderEbam_2_header <- "MasterTable_ID,Alias,Latitude,Longitude,TimeStamp,PDate"
#   olderEbam_2_rawNames <- unlist(stringr::str_split(olderEbam_2_header, ','))
#   olderEbam_2_names <- make.names(olderEbam_2_rawNames)
#   olderEbam_2_types <- 'ccddcc'
  
  
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
  rawNames <- vector('character')
  columnNames <- vector('character')
  columnTypes <- vector('character')
  if ( length(setdiff(esam_names, colNames)) == 0 ) {
    monitorType <- "ESAM"
    rawNames <- esam_rawNames
    columnNames <- esam_names
    columnTypes <- esam_types
  } else if ( length(setdiff(ebam_names, colNames)) == 0 ) {
    monitorType <- "EBAM"
    rawNames <- ebam_rawNames
    columnNames <- ebam_names
    columnTypes <- ebam_types
#   } else if ( length(setdiff(bam1020_names, colNames)) == 0 ) {
#     monitorType <- "BAM1020"
#     rawNames <- bam1020_rawNames
#     columnNames <- bam1020_names
#     columnTypes <- bam1020_types
  # } else if ( length(setdiff(olderEbam_1_names, colNames)) == 0 ) {
  #   monitorType <- "OTHER_1"
  # } else if ( length(setdiff(olderEbam_2_names, colNames)) == 0 ) {
  #   monitorType <- "OTHER_2"
  }
  
  monitorTypeList <- list(monitorType=monitorType,
                          rawNames=rawNames,
                          columnNames=columnNames,
                          columnTypes=columnTypes)
  
  return(monitorTypeList)
}
