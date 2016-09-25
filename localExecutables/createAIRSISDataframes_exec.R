#!/usr/bin/env Rscript

# This script parses mobile PM2.5 monitoring data from EBAM mobile monitors
# and creates 'master', 'metadata' and 'data' .RData files for this class of
# monitors.
#
# All EBAM data files share a common core of attributes as seen in the header
# line of ~ebam.csv files.:
#
#   Date/Time/GMT,COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,UnitID,Alias,Latitude,Longitude,TimeStamp
#
# Files ending in ~ebam_bam.csv share these column but include a few more which
# typically contain missing values:
#
#   Date/Time/GMT,Conc (?g/m3),Qtot (m3),COncRT,ConcHr,WS (KTS),Ozone (ppb),Flow,W/S,RTM09 (mg3),RH (%),W/D,AT,Ambient Temp (C),RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Start Date/Time (GMT),Sys. Volts,UnitID,Alias,Latitude,Longitude,TimeStamp
#
# This script will process a variety of ~ebam.csv and ~bam.csv files to produce
# the following:
#
# 1) A single "master database" EBAM_2014.RData file containing a dataframe with
#    the same columns as an ~ebam.csv file but for data from all source files.
#
#    The duplicated() function will be used when adding new data to ensure
#    that records remain unique.  This dataframe will be in 'long format'
#    that the reshape2 package can work on.
#
# 2) An EBAM_SitesMetadata.RData file containing metadata for all monitors
#    encountered.  This file will be generated from the master file in 1).
#
# 3) An EBAM_PM2.5_Latest.RData file containing PM2.5 data in a block
#    organized as [datetime,uniqueSiteID].  This file will also be 
#    generated from the master file in 1).


library(methods)   # always included for Rscripts
library(MazamaSpatialUtils) # for timezones



################################################################################
# Calculate the Distance between Points
#   http://www.r-bloggers.com/great-circle-distance-calculations-in-r/

distance <- function(targetLon, targetLat, lons, lats) {
  
  if (length(lons) != length(lats)) {
    stop(paste0('lons [',length(lons),'] and lats [',length(lats),'] are requried to be of the same length'))
  }
  
  # Algorithm copied directly from http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
    
  # Set up vector to be filled in
  distance <- rep(as.numeric(NA),length(lons))
  
  # Convert to radians
  targetLon <- targetLon*pi/180
  targetLat <- targetLat*pi/180
  
  # Haversine formula
  for (i in seq(length(lons))) {
    lon <- lons[i]*pi/180
    lat <- lats[i]*pi/180
    R <- 6371 # Earth mean radius [km]
    delta.lon <- (lon - targetLon)
    delta.lat <- (lat - targetLat)
    a <- sin(delta.lat/2)^2 + cos(targetLat) * cos(lat) * sin(delta.lon/2)^2
    c <- 2 * asin(min(1,sqrt(a)))
    distance[i] = R * c # distance in km
  }
  
  return(distance)
  
}




################################################################################
# createEBAMDataframes()
#
# Read in the lateset EBAM data files and use the contents to create metadata
# and data dataframes similar in structure to the AirNow dataframes.

createEBAMDataframes <- function(opt) {

  EBAM_PM25 <- suppressWarnings( readEBAMData(opt) ) # Don't want to hear about 'NAs introduced by coercion'
  
  if (opt$debug) print( paste0('All data read in.') )

  # ----- Update Master File --------------------------------------------------
  
  # One file per month
  absMasterFile <- paste0(opt$destDir,'/EBAM_Master_',format(lubridate::now("GMT"),"%Y%m"),".RData")
  
  if (file.exists(absMasterFile)) {    
    # Add DF to previous Master and check for duplicates.
    if (opt$debug) print( paste0('Reading in ',absMasterFile) )
    oldMaster <- get(load(absMasterFile))
    if (opt$debug) print( paste0('rbinding ',nrow(EBAM_PM25),' rows onto onto ',absMasterFile) )
    EBAM_PM2.5_Master <- rbind(oldMaster, EBAM_PM25)
    ###EBAM_PM2.5_Master <- dplyr::bind_rows(oldMaster, EBAM_PM25)
    EBAM_PM2.5_Master <- EBAM_PM2.5_Master[!duplicated(EBAM_PM2.5_Master),]
  } else {
    # No master found so use DF as Master
    assign('EBAM_PM2.5_Master',EBAM_PM25)
  }
  
  if (opt$debug) print( paste0('Saving ',absMasterFile,' with ',nrow(EBAM_PM2.5_Master),' rows.') )

  # Save result
  save(list='EBAM_PM2.5_Master',file=absMasterFile)
  
  
  # > names(EBAM_PM25)
  # [1] "Date.Time.GMT"     "COncRT"            "ConcHr"            "Flow"              "W.S"              
  # [6] "W.D"               "AT"                "RHx"               "RHi"               "BV"               
  # [11] "FT"                "Alarm"             "Type"              "Serial.Number"     "Version"          
  # [16] "Sys..Volts"        "UnitID"            "Alias"             "Latitude"          "Longitude"        
  # [21] "TimeStamp"         "timezone"          "GMTOffsetHours"    "GMTDSTOffsetHours" "countryCode"      
  # [26] "GMTTime"           "localTime"         "PWFSL_sourceFile"  "PWFSL_timestamp"  

  
  # Leland Tarnay QC -----------------------------------------------------------

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

  goodFlow <- EBAM_PM25$Flow >= 16.7*0.95 & EBAM_PM25$Flow <= 16.7*1.05
  goodAT <- EBAM_PM25$AT <= 45
  goodRHi <- EBAM_PM25$RHi <= 45
  goodConcHr <- EBAM_PM25$ConcHr <= 0.984
  goodGMTTime <- EBAM_PM25$GMTTime < lubridate::now("GMT") # saw a future date once
  
  if (opt$debug) {
    print(paste0('Flow has ',sum(is.na(EBAM_PM25$Flow)),' missing values and ',sum(!goodFlow,na.rm=TRUE),' out of range values.'))
    print(paste0('AT has ',sum(is.na(EBAM_PM25$AT)),' missing values and ',sum(!goodAT,na.rm=TRUE),' out of range values.'))
    print(paste0('RHi has ',sum(is.na(EBAM_PM25$RHi)),' missing values and ',sum(!goodRHi,na.rm=TRUE),' out of range values.'))
    print(paste0('ConcHr has ',sum(is.na(EBAM_PM25$ConcHr)),' missing values and ',sum(!goodConcHr,na.rm=TRUE),' out of range values.'))
    print(paste0('GMTTime has ',sum(is.na(EBAM_PM25$GMTTime)),' missing values.',sum(!goodGMTTime,na.rm=TRUE),' out of range values.'))
  }
  
  goodMask <- !is.na(EBAM_PM25$Flow) & goodFlow &
    !is.na(EBAM_PM25$AT) & goodAT &
    !is.na(EBAM_PM25$RHi) & goodRHi &
    !is.na(EBAM_PM25$ConcHr) & goodConcHr &
    !is.na(EBAM_PM25$GMTTime) & goodGMTTime

  if (opt$debug ) print(paste0(nrow(EBAM_PM25),' rows of data of which ',sum(is.na(goodMask)), ' are NA and ', sum(!goodMask,na.rm=TRUE),' are flagged as bad'))

  EBAM_PM25 <- EBAM_PM25[goodMask,]


  # END of QC -----------------------------------------------------------------

  # NOTE:  EBAM monitors do not have an official AQSID so we will assign a unique ID that can be used
  # NOTE:  in plotting code in the databrowser. Include everything up to the first '('.
  ID <- stringr::str_trim(stringr::str_extract(EBAM_PM25$Alias,'[^\\(]+'))
  EBAM_PM25$monitorID <- paste0(make.names(ID))
  
  # ----- Create PM2.5.RData file ----------------------------------------------
  
  if (opt$PM2.5) {
    
    if (opt$debug) print(paste0('Creating EBAM_PM2.5_Latest.RData'))

    # Read in the metadata and figure out which instruments collect PM2.5 data.
    absMetaFile <- paste(opt$destDir,'EBAM_SitesMetadata.RData',sep='/')
    load(absMetaFile)
    monitorIDs <- EBAM_SitesMetadata$monitorID
    
    # Load any pre-existing data and change the in-memory name
    thisYear <- format(Sys.time(),"%Y",tz="GMT")
    absDataFile <- paste(opt$destDir,'/EBAM_PM2.5_',thisYear,'.RData',sep='')   
    
    if ( file.exists(absDataFile) ) {
      
      if (opt$debug) print( paste0('Reading in ',absDataFile) )
      objName <- load(absDataFile)
      if (opt$debug) print( paste0('Finished reading in ',absDataFile) )
      hourlyDF <- get(objName)
      rm(list=c(objName))  
      
    } else {
      
      # Create an empty hourlyDF object covering 13 months of data
      lastYear <- as.numeric(thisYear) - 1
      start <- as.POSIXct(paste(lastYear,"-12-01 00:00:00",sep=""),tz="GMT")
      end <- as.POSIXct(paste(thisYear,"-12-31 23:00:00",sep=""),tz="GMT")
      datetime <- seq(start,end,by="hour")
      datetimeIds <- format(datetime,"%Y%m%d%H",tz="GMT")
      
      # Create an empty hourlyDF dataframe with dimensions datetimeId by monitorID+1
      # (The very first column will be "datetime", followed by all the monitorIDs)
      hourlyData <- array(data = NA,
                          dim = c(length(datetimeIds),length(monitorIDs)+1),
                          dimnames = list(datetimeIds, c("datetime",monitorIDs) ) )
      
      # Convert to dataframe
      hourlyDF <- as.data.frame(hourlyData)
      hourlyDF$datetime <- datetime
      
      # Clean up memory
      rm(hourlyData)
      
    }
     
    # TODO:  Here is the place to add any QA/QC based on other variables in the raw data
    # TODO:  Perhaps by creating 2D dataframes of the QA/QC variables
    
    # NOTE:  Request from Leland Tarnay on 2015-08-10 to remove all measurements made more than
    # NOTE:  1 km away from the latest location.
    badLocation <- rep(FALSE,nrow(EBAM_PM25))
    for (monitorID in rownames(EBAM_SitesMetadata)) {
      siteMask <- EBAM_PM25$monitorID == monitorID
      targetLon <- EBAM_SitesMetadata[monitorID,'longitude']
      targetLat <- EBAM_SitesMetadata[monitorID,'latitude']
      lon <- EBAM_PM25$Longitude
      lat <- EBAM_PM25$Latitude
      dist <- distance(targetLon,targetLat,lon,lat)
      badDist <- siteMask & (dist > 1.0)
      badLocation <- badLocation | badDist
    }
    EBAM_PM25 <- EBAM_PM25[!badLocation,]
    
    # Create 2D [datetime,instrumentID] dataframe of PM2.5 data with the 'reshape2' package:
    
    # NOTE:  COncRT is ...
    # NOTE:  ConcHr is the hourly average that should be used

    # NOTE:  reshape2::melt -- cannot melt data.frames with non-atomic columns
    # Create minimal subset with only atomic columns
    EBAM_PM25$YMDH <- strftime(EBAM_PM25$GMTTime,'%Y%m%d%H',tz='GMT')
    subDF <- EBAM_PM25[,c('monitorID','YMDH','ConcHr')]
    melted <- reshape2::melt(subDF, id.vars=c('monitorID','YMDH'), measure.vars='ConcHr')
    
    # Convert to UG/M3
    melted$value <- melted$value * 1000
    
    # Use median if multiple values are found
    # NOTE:  The resulting dataframe is [YMDH,monitorID] with an extra first column containing YMDH
    PM2.5_DF <- reshape2::dcast(melted, YMDH ~ monitorID, median)
    
    # NOTE:  It seems that a lot of data are being thrown away at the 11:00 and 23:00 hours
    # NOTE:  presumably due to the logic below.
    
    # Leland Tarnay suggests ignoring any cells with multiple values
    # Save YMDH as it will get wiped out with this logic
    savedYMDH <- PM2.5_DF$YMDH
    valueCountPerCell <- reshape2::dcast(melted, YMDH ~ monitorID, length)
    PM2.5_DF[valueCountPerCell > 1] <- NA
    # Restore YMDH
    PM2.5_DF$YMDH <- savedYMDH
    
    if (opt$debug) {
      # Create a logical matrix identifying cells with multiples
      multiples <- valueCountPerCell > 1
      # For each column, how many times did multiple cells occur
      multiplesCount <- apply(multiples,2,sum)
      names(multiplesCount) <- names(PM2.5_DF)
      # Print out the results, ignoring the 'datetime' column
      results <- multiplesCount[-1]
      if (any(results > 1)) {
        print(paste0('Some Instruments had multiple values per hour. Maximum values per hour are given below:'))
        print(multiplesCount[-1])
        print(paste0('The maximum number of values per hour was: ',max(valueCountPerCell[,-1],na.rm=TRUE)))
      }
    }
    
    # Finally, convert the YMDH column back into a column of POSIXct values named 'datetime'
    PM2.5_DF$YMDH <- lubridate::ymd_h(PM2.5_DF$YMDH)
    newNames <- names(PM2.5_DF)
    newNames[1] <- 'datetime'
    names(PM2.5_DF) <- newNames
    
    # Assign rownames
    rownames(PM2.5_DF) <- format(PM2.5_DF$datetime,"%Y%m%d%H",tz="GMT")
    
    # Overwrite data from hourlyDF with data from PM2.5_DF --------------------
    
    # First, add columns for any new monitors
    for (name in names(PM2.5_DF)) {
      if (!name %in% names(hourlyDF)) {
        hourlyDF[[name]] <- as.numeric(NA)
      }
    }
    # Now add the data
    hourlyDF[rownames(PM2.5_DF),colnames(PM2.5_DF)] <- PM2.5_DF
    PM2.5 <- hourlyDF
    
    # Save the 13-month file
    save(PM2.5, file=absDataFile)
    
    # Find latest 10 days
    endTime <- lubridate::now("GMT")
    startTime <- endTime - 10 * 24 * 3600
    timeline <- seq(round(startTime,"hour"),round(endTime,"hour"),by="hour")
    last_10_days <- format(timeline,"%Y%m%d%H")
    
    # Subset and save latest 10 days
    PM2.5 <- PM2.5[last_10_days,]
    
    # Save the file
    absDataFile <- paste0(opt$destDir,'/EBAM_PM2.5_Latest.RData')
    save(PM2.5,file=absDataFile)
        
  }
  
}

# END of createEBAMDataframes()
################################################################################
################################################################################
################################################################################

###############################################################################

airsis_getData <- function(files, debug=FALSE) {
  
  # NOTE:  All EBAM data files share a common core of attributes as seen in the header
  # NOTE:  line of ~ebam.csv files.:
  # NOTE:  
  # NOTE:    Date/Time/GMT,COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,UnitID,Alias,Latitude,Longitude,TimeStamp
  # NOTE:  
  # NOTE:  Files ending in ~ebam_bam.csv share these columns but include a few more which
  # NOTE:  typically contain missing values:
  # NOTE:  
  # NOTE:    Date/Time/GMT,Conc (?g/m3),Qtot (m3),COncRT,ConcHr,WS (KTS),Ozone (ppb),Flow,W/S,RTM09 (mg3),RH (%),W/D,AT,Ambient Temp (C),RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Start Date/Time (GMT),Sys. Volts,UnitID,Alias,Latitude,Longitude,TimeStamp
  # NOTE:
  # NOTE:  Becuase the dataframes will have different numbers of columns we will have to
  # NOTE:  merge them individually with dplyr::full_join rather than the faster
  # NOTE:  technique of merging them all at once with dplyr::bind_rows.
  
  firstTime <- TRUE
  DF = NULL
  for (file in files) {
    
    # Read in the data 
    # NOTE:  APCD_ebam_bam.csv and USFS_ebam_bam.csv both have 'latin1' encoding
    if (stringr::str_detect(file,'ebam_bam.csv') ||
        stringr::str_detect(file,'GBUAPCD.csv') ||   # TODO:  Remove this HACK
        stringr::str_detect(file,'ODEQ.csv') ||      # TODO:  Remove this HACK
        stringr::str_detect(file,'WASHOE.csv')) {    # TODO:  Remove this HACK
      lines <- readLines(file, encoding='latin1')
    } else {
      lines <- readLines(file)
    }

    # Remove malformed lines and collapse to a single text element
    commaCount <- stringr::str_count(lines,',')
    goodMask <- commaCount == commaCount[1]
    text <- paste0(lines[goodMask],collapse='\n')
    
    columnCount <- max(commaCount,na.rm=TRUE) + 1
    
    if (debug) print(paste0('Working on ',file,' -- ',columnCount,' columns'))

    if (columnCount == 21) {
      col_types <- 'cdddddddddddcccdccddc'
    } else if (columnCount == 29) {
      col_types <- 'cddddddddddddddddddccccdccddc'
    } else {
      if (debug) print(paste0('columnCount "',columnCount,'" not recognized'))
      next
    }
    
    # Read in the data
    result <- try( suppressWarnings(df <- readr::read_csv(text, col_types=col_types, progress=FALSE)),
                   silent=TRUE )
    
    # Skip to next file if errors were encountered
    if (class(result)[1] == "try-error") {
      err_msg <- geterrmessage()
      if (debug) print(paste0('Could not parse ',file,' as a dataframe.'))
      next
    }
    
    # Sanity check -- must have some data
    if (nrow(df) == 0) {
      if (debug) print(paste0('Skipping ',file,' which contains no data.'))
      next
    }
    
    # Add column identifying the file and processing date
    df$PWFSL_sourceFile <- file
    df$PWFSL_processingDateGMT <- lubridate::now('GMT')
    
    if (firstTime) {
      DF <- df
      firstTime <- FALSE
    } else {
      # TODO:  Is there a more efficient way to dplyr::full_join several dataframes?
      result <- try( DF <- suppressMessages( dplyr::full_join(DF,df)),
                     silent=TRUE)

      # Skip to next file if errors were encountered
      if (class(result)[1] == "try-error") {
        err_msg <- geterrmessage()
        if (debug) print(paste0(err_msg))
        next
      }
    }
    
  }
  
  return(DF)
  
}

###############################################################################

airsis_QCMask <- function(raw, reportFile=NULL) {
  
  # NOTE:  The incoming raw dataframe, generated by airsis_getData(), will have the following columns:
  # NOTE:  
  # NOTE:    Date/Time/GMT,Conc (µg/m3),Qtot (m3),COncRT,ConcHr,WS (KTS),Ozone (ppb),Flow,W/S,RTM09 (mg3),
  # NOTE:    RH (%),W/D,AT,Ambient Temp (C),RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Start Date/Time (GMT),
  # NOTE:    Sys. Volts,UnitID,Alias,Latitude,Longitude,TimeStamp,PWFSL_sourceFile,PWFSL_processingDateGMT

  # Create a set of masks that will be used to assess the quality of data
  bam1020 <- !is.na(raw$`Qtot (m3)`)
  ebam <- !bam1020
  
  # Saw a future date once
  future <- raw$`Date/Time/GMT` > lubridate::now('GMT')
  
  missingFlow <- is.na(raw$Flow)
  missingAT <- is.na(raw$AT)
  missingRHi <- is.na(raw$RHi)
  missingConcHr <- is.na(raw$ConcHr)
  missingConcHr <- is.na(raw$ConcHr)
  missingLongitude <- is.na(raw$Longitude)
  missingLatitude <- is.na(raw$Latitude)
  
  # EBAM QC
  ebam_goodFlow <- raw$Flow >= 16.7*0.95 & raw$Flow <= 16.7*1.05
  ebam_goodAT <- raw$AT <= 45
  ebam_goodRHi <- raw$RHi <= 45
  ebam_goodConcHr <- raw$ConcHr <= 0.984
  
  goodEBAM <- ebam &
    ebam_goodFlow & !missingFlow &
    ebam_goodAT & !missingAT &
    ebam_goodRHi & !missingRHi &
    ebam_goodConcHr & !missingConcHr &
    !missingLongitude & !missingLatitude &
    !future
  
  # BAM 1020 QC
  bam1020_goodFlow <- raw$Flow >= 0.834*0.95 & raw$Flow <= 0.834*1.05
  bam1020_goodAT <- raw$AT <= 45
  bam1020_goodRHi <- raw$RHi <= 45
  bam1020_goodConcHr <- raw$ConcHr <= 0.984
  
  goodBAM1020 <- bam1020 &
    bam1020_goodFlow & !missingFlow &
    bam1020_goodAT & !missingAT &
    bam1020_goodRHi &!missingRHi &
    bam1020_goodConcHr & !missingConcHr &
    !missingLongitude & !missingLatitude &
    !future
  
  # Generate a report if requested
  if (!is.null(reportFile)) {
    
    ebamCount <- sum(ebam,na.rm=TRUE)
    ebam_goodCount <- sum(goodEBAM,na.rm=TRUE)
    ebam_badCount <- ebamCount - ebam_goodCount
    ebam_missingFlowCount <- sum(missingFlow[ebam],na.rm=TRUE)
    ebam_missingATCount <- sum(missingAT[ebam],na.rm=TRUE)
    ebam_missingRHiCount <- sum(missingRHi[ebam],na.rm=TRUE)
    ebam_missingConcHrCount <- sum(missingConcHr[ebam],na.rm=TRUE)
    ebam_missingLongitudeCount <- sum(missingLongitude[ebam],na.rm=TRUE)
    ebam_missingLatitudeCount <- sum(missingLatitude[ebam],na.rm=TRUE)
    ebam_futureCount <- sum(future[ebam],na.rm=TRUE)
    ebam_badFlowCount <- sum(!ebam_goodFlow[ebam],na.rm=TRUE)
    ebam_badATCount <- sum(!ebam_goodAT[ebam],na.rm=TRUE)
    ebam_badRHiCount <- sum(!ebam_goodRHi[ebam],na.rm=TRUE)
    ebam_badConcHrCount <- sum(!ebam_goodConcHr[ebam],na.rm=TRUE)
    
    bam1020Count <- sum(bam1020,na.rm=TRUE)
    bam1020_goodCount <- sum(goodBAM1020,na.rm=TRUE)
    bam1020_badCount <- bam1020Count - bam1020_goodCount
    bam1020_missingFlowCount <- sum(missingFlow[bam1020],na.rm=TRUE)
    bam1020_missingATCount <- sum(missingAT[bam1020],na.rm=TRUE)
    bam1020_missingRHiCount <- sum(missingRHi[bam1020],na.rm=TRUE)
    bam1020_missingConcHrCount <- sum(missingConcHr[bam1020],na.rm=TRUE)
    bam1020_missingLongitudeCount <- sum(missingLongitude[bam1020],na.rm=TRUE)
    bam1020_missingLatitudeCount <- sum(missingLatitude[bam1020],na.rm=TRUE)
    bam1020_futureCount <- sum(future[bam1020],na.rm=TRUE)
    bam1020_badFlowCount <- sum(!bam1020_goodFlow[bam1020],na.rm=TRUE)
    bam1020_badATCount <- sum(!bam1020_goodAT[bam1020],na.rm=TRUE)
    bam1020_badRHiCount <- sum(!bam1020_goodRHi[bam1020],na.rm=TRUE)
    bam1020_badConcHrCount <- sum(!bam1020_goodConcHr[bam1020],na.rm=TRUE)
    
    reportText <- paste0(lubridate::now('GMT'),' ---------------------------------\n\n',
                         ebamCount,' EBAM records with ',ebam_goodCount,' good, ',ebam_badCount,' rejected:\n',
                         '\t',ebam_missingFlowCount,' missing "Flow"\n',
                         '\t',ebam_missingATCount,' missing "AT"\n',
                         '\t',ebam_missingRHiCount,' missing "RHi"\n',
                         '\t',ebam_missingConcHrCount,' missing "ConcHr"\n',
                         '\t',ebam_missingLongitudeCount,' missing "Longitude"\n',
                         '\t',ebam_missingLatitudeCount,' missing "Latitude"\n',
                         '\t',ebam_badFlowCount,' out of range "Flow"\n',
                         '\t',ebam_badATCount,' out of range "AT"\n',
                         '\t',ebam_badRHiCount,' out of range "RHi"\n',
                         '\t',ebam_badConcHrCount,' out of range "ConcHr"\n',
                         '\t',ebam_futureCount,' future times\n\n',
                         bam1020Count,' BAM 1020 records with ',bam1020_goodCount,' good, ',bam1020_badCount,' rejected:\n',
                         '\t',bam1020_missingFlowCount,' missing "Flow"\n',
                         '\t',bam1020_missingATCount,' missing "AT"\n',
                         '\t',bam1020_missingRHiCount,' missing "RHi"\n',
                         '\t',bam1020_missingConcHrCount,' missing "ConcHr"\n',
                         '\t',bam1020_missingLongitudeCount,' missing "Longitude"\n',
                         '\t',bam1020_missingLatitudeCount,' missing "Latitude"\n',
                         '\t',bam1020_badFlowCount,' out of range "Flow"\n',
                         '\t',bam1020_badATCount,' out of range "AT"\n',
                         '\t',bam1020_badRHiCount,' out of range "RHi"\n',
                         '\t',bam1020_badConcHrCount,' out of range "ConcHr"\n',
                         '\t',bam1020_futureCount,' future times\n\n')
    
    cat(reportText, file=reportFile, append=TRUE)
    
  }
  
  # Return the mask of good values
  goodMask <- goodEBAM | goodBAM1020
  return(goodMask)
  
}
  

###############################################################################

airsis_harmonize <- function(raw_qc) {
 
  # NOTE:  The AirNow_SitesMetadata.RData dataframe is organized [InstrumentID,MetadataParameter]
  # NOTE:
  # NOTE:  Here is the list of AirNow_SitesMetadata columns:
  # NOTE:  
  # NOTE:  [1] "AQSID"          "siteCode"       "siteName"       "status"        
  # NOTE:  [5] "agencyID"       "agencyName"     "EPARegion"      "latitude"      
  # NOTE:  [9] "longitude"      "elevation"      "GMTOffsetHours" "countryCode"   
  # NOTE:  [13] "FIPSMSACode"    "MSAName"        "FIPSStateCode"  "stateCode"     
  # NOTE:  [17] "GNISCountyCode" "countyName"     "has_BARPR"      "has_BC"        
  # NOTE:  [21] "has_CO"         "has_DEWPOINT"   "has_EC"         "has_H2S"       
  # NOTE:  [25] "has_NO"         "has_NO2"        "has_NO2Y"       "has_NOX"       
  # NOTE:  [29] "has_NOY"        "has_O3"         "has_OC"         "has_PM10"      
  # NOTE:  [33] "has_PM2.5"      "has_PM2.5_15"   "has_PMC"        "has_PRECIP"    
  # NOTE:  [37] "has_RHUM"       "has_SO2"        "has_SO2_15"     "has_SO4"       
  # NOTE:  [41] "has_SRAD"       "has_TEMP"       "has_UV_AETH"    "has_WD"        
  # NOTE:  [45] "has_WS"        
  
  # NOTE:  We will create a reduced version for non-AirNowTech data sources:
  # NOTE:
  # NOTE:  [1] "AQSID"          "siteCode"       "siteName"       "status"        
  # NOTE:  [5] "agencyID"       "agencyName"     "EPARegion"      "latitude"      
  # NOTE:  [9] "longitude"      "elevation"      "timezone"       "GMTOffsetHours"  
  # NOTE:  [13] "countryCode"   "FIPSMSACode"    "MSAName"        "FIPSStateCode"    
  # NOTE:  [17] "stateCode"     "GNISCountyCode" "countyName"     "monitorID"
  # NOTE:  [21] "has_PM2.5"
  # NOTE:  Many of these fields will be empty
  
  # NOTE:  raw_qc should have the following column names:
  # NOTE:
  # NOTE   [1] "Date/Time/GMT"           "Conc (µg/m3)"            "Qtot (m3)"               "COncRT"                 
  # NOTE   [5] "ConcHr"                  "WS (KTS)"                "Ozone (ppb)"             "Flow"                   
  # NOTE   [9] "W/S"                     "RTM09 (mg3)"             "RH (%)"                  "W/D"                    
  # NOTE   [13] "AT"                      "Ambient Temp (C)"        "RHx"                     "RHi"                    
  # NOTE   [17] "BV"                      "FT"                      "Alarm"                   "Type"                   
  # NOTE   [21] "Serial Number"           "Version"                 "Start Date/Time (GMT)"   "Sys. Volts"             
  # NOTE   [25] "UnitID"                  "Alias"                   "Latitude"                "Longitude"              
  # NOTE   [29] "TimeStamp"               "PWFSL_sourceFile"        "PWFSL_processingDateGMT"
  
  # TODO:  Sort out tricky monitorID/location uniqueness.
  # 
  # NOTE:  We really need to load in the existing metadata file at this point to get the canonical
  # NOTE:  location associated with each monitorID. Only then can we look at all the locations
  # NOTE:  associated with each Alias, calculate the distance from the canonical location and
  # NOTE:  decide whether this is a new monitor or not. This should be a new function called
  # NOTE:  from the top level rather than embedded in airsis_harmonize.
  
  # Create a unique monitor-location ID
  #
  # NOTE:  geosphere::distHaversine(c(0,30),c(0.01,30.01))
  # NOTE:  [1] 1472.587
  # NOTE:  So we can round to the second decimal place to create locations
  # NOTE:  unique to within about 1.5km of each other.
  # NOTE:
  # NOTE:  Be careful with rounding! Example:
  # NOTE:
  # NOTE:  > a <- c(1.25501,1.25499) # separated by .00002
  # NOTE:  > round(a,2)
  # NOTE:  [1] 1.26 1.25

  locationStamp <- paste0(round(raw_qc$Longitude,2),'E_',
                          round(raw_qc$Latitude,2),'N')
  monitorID <- paste0(raw_qc$Alias,'_',locationStamp)
  
  # Calculate distances from a known location (and later eachother)
#   PWFSL_location <- c(-122.353073, 47.650537)
#   monitor_location <- cbind(as.numeric(raw_qc$Longitude), as.numeric(raw_qc$Latitude))
#   distance <- rep(as.numeric(NA),nrow(raw_qc))
#   for (i in 1:nrow(monitor_location)) {
#     if (!anyNA(monitor_location[i,])) {
#       distance[i] <- geosphere::distHaversine(PWFSL_location,monitor_location[i,])
#     }
#   }
  
  # agencyName
  NPSMask <- stringr::str_detect(raw_qc$Alias,'^NPS ')
  USFSMask <- stringr::str_detect(raw_qc$Alias,'^USFS')
  APCDMask <- stringr::str_detect(raw_qc$Alias,'^AQMD ')
  agencyName <- rep('',nrow(raw_qc))
  agencyName[NPSMask] <- 'National Park Service' 
  agencyName[USFSMask] <- 'United States Forest Service'
  agencyName[APCDMask] <- 'local regulator' 
  
  # NOTE:  Sometimes a reasonable siteName is given in parens but this isn't reliable.
  # NOTE:  Instead, we'll rely on ggmap::revgeocode() to create siteNames when
  # NOTE:  creating the metadata.
  ###siteName <- stringr::str_match(raw_qc$Alias,'\\((.+)\\)')[,2]
  
  # NOTE:  This dataframe is in long format and will begin with the data
  # NOTE:  columns:  'datetime' and 'PM2.5'
  
  # Create a dataframe with the expected columns
  DF <- data.frame(datetime=raw_qc$`Date/Time/GMT`,
                   PM2.5=raw_qc$ConcHr*1000,                # EBAM reporting in milligrams
                   AQSID=as.character(NA),
                   siteCode=as.character(NA),
                   siteName=as.character(NA),
                   status=as.character(NA),
                   agencyID=as.character(NA),
                   agencyName=agencyName,
                   EPARegion=as.character(NA),
                   latitude=raw_qc$Latitude,
                   longitude=raw_qc$Longitude,
                   elevation=as.numeric(NA),
                   timezone=as.character(NA),
                   GMTOffsetHours=as.numeric(NA),
                   countryCode=as.character(NA),
                   FIPSMSACode=as.character(NA),
                   MSAName=as.character(NA),
                   FIPSStateCode=as.character(NA),
                   stateCode=as.character(NA),
                   GNISCountyCode=as.character(NA),
                   countyName=as.character(NA),
                   monitorID=monitorID,
                   has_PM2.5=raw_qc$Type == 'PM 2.5',
                   stringsAsFactors=FALSE)
  

  return(DF)
}


###############################################################################

airsis_createSitesMetadata <- function(harmonized, oldMeta=NULL, debug=FALSE) {
  
  # NOTE:  harmonized should have the following columns
  # NOTE:   [1] "datetime"        "PM2.5"          "AQSID"          "siteCode"       "siteName"       "status"        
  # NOTE:   [7] "agencyID"       "agencyName"     "EPARegion"      "latitude"       "longitude"      "elevation"     
  # NOTE:   [13] "timezone"       "GMTOffsetHours" "countryCode"    "FIPSMSACode"    "MSAName"        "FIPSStateCode" 
  # NOTE:   [19] "stateCode"      "GNISCountyCode" "countyName"     "monitorID"      "has_PM2.5"     
  
  # Remove the first two columns and use fromLast=TRUE to guarantee that we have the most recent records
  newMeta <- harmonized[!duplicated(harmonized$monitorID,fromLast=TRUE),c(-1,-2)]
  rownames(newMeta) <- newMeta$monitorID
  
  if (is.null(oldMeta)) {
    
    meta <- newMeta
    
  } else {
    
    # Guarantee that oldMeta has proper rownames
    rownames(oldMeta) <- oldMeta$monitorID
    
    # Preserve expensive-to-calculate, location-specific results: siteName, timezone, GMTOffsetHours
    newMeta$siteName <- oldMeta[newMeta$monitorID,'siteName']
    newMeta$EPARegion <- oldMeta[newMeta$monitorID,'EPARegion']
    newMeta$elevation <- oldMeta[newMeta$monitorID,'elevation']
    newMeta$timezone <- oldMeta[newMeta$monitorID,'timezone']
    newMeta$GMTOffsetHours <- oldMeta[newMeta$monitorID,'GMTOffsetHours']
    newMeta$countryCode <- oldMeta[newMeta$monitorID,'countryCode']
    newMeta$FIPSMSACode <- oldMeta[newMeta$monitorID,'FIPSMSACode']
    newMeta$MSAName <- oldMeta[newMeta$monitorID,'MSAName']
    newMeta$FIPSStateCode <- oldMeta[newMeta$monitorID,'FIPSStateCode']
    newMeta$stateCode <- oldMeta[newMeta$monitorID,'stateCode']
    newMeta$GNISCountyCode <- oldMeta[newMeta$monitorID,'GNISCountyCode']
    newMeta$countyName <- oldMeta[newMeta$monitorID,'countyName']
    
    # Now join them and remove duplicates
    meta <- dplyr::bind_rows(oldMeta,newMeta)
    meta <- meta[!duplicated(meta$monitorID),]
  }

  # Get siteName where missing
  
  # Use ggmap::revgeocode to return a dataframe with (address, street number, route, locality , ...)
  # (2500 queries allowed per day in August, 2015)
  if ('ggmap' %in% installed.packages()[,1]) {
    
    for (i in 1:nrow(meta)) {
      if (is.na(meta[i,'siteName'])) {
        location <- c(meta$longitude[i],meta$latitude[i])
        if (!anyNA(location)) {
          suppressMessages( address <- ggmap::revgeocode(location, output='more') )
          meta$siteName[i] <- paste0(address$locality,'-',address$route)
          countyName <- stringr::str_trim(stringr::str_replace(address$administrative_area_level_2,'County',''))
          meta$countyName[i] <- countyName
          # TODO:  Need to convert sateName to stateCode (requires enhancement of MazamaSpatialUtils)
          ###stateName <- address$administrative_area_level_1
          ### meta$stateCode <- MazamaSpatialUtils::stateToCode(stateName)
          ### if (is.null(meta$stateCode)) meta$stateCode <- NA
          
          if (debug) print(paste0('address = ',address$address))
        }
      }
    }
    
  }
  
  # Add timezone where missing
  for (i in 1:nrow(meta)) {
    if (is.na(meta[i,'timezone'])) {
      if ( !is.na(meta$longitude[i]) && !is.na(meta$latitude[i]) ) {
      timezoneDF <- MazamaSpatialUtils::getTimezone(meta$longitude[i],meta$latitude[i],allData=TRUE)
      meta$timezone[i] <- timezoneDF$timezone
      meta$GMTOffsetHours[i] <- timezoneDF$UTC_offset
      ###meta$GMTDSTOffsetHours <- timezoneDF$UTC_DST_offset # during Daylight Savings Time (DST)
      meta$countryCode[i] <- timezoneDF$countryCode
      }
    }
  }
  
  # Assign rownames
  rownames(meta) <- meta$monitorID
  
  return(meta)
}


###############################################################################

airsis_createData <- function(harmonized, oldData=NULL, removeMultiples=TRUE, debug=FALSE) {

  # Reshape the data ------------------
  
  # NOTE:  reshape2::melt -- cannot melt data.frames with non-atomic columns
  # Create minimal subset with only atomic columns
  subDF <- harmonized[,c('datetime','monitorID','PM2.5')]
  melted <- reshape2::melt(subDF, id.vars=c('datetime','monitorID'), measure.vars='PM2.5')
  
  # Use median if multiple values are found
  # NOTE:  The resulting dataframe is [YMDH,monitorID] with an extra first column containing YMDH
  PM2.5_DF <- reshape2::dcast(melted, datetime ~ monitorID, median)
  
  # NOTE:  It seems that a lot of data are being thrown away at the 11:00 and 23:00 hours
  # NOTE:  presumably due to the logic below.
  
  # Remove multiples ------------------

  if (removeMultiples) {
    
    # Leland Tarnay suggests ignoring any cells with multiple values
    # Note:  Don't include the first column (datetime) in this logic
    valueCountPerCell <- reshape2::dcast(melted, datetime ~ monitorID, length)
    PM2.5_DF[,-1][valueCountPerCell[,-1] > 1] <- NA
    
    if (opt$debug) {
      # Create a logical matrix identifying cells with multiples
      multiples <- valueCountPerCell > 1
      # For each column, how many times did multiple cells occur
      multiplesCount <- apply(multiples,2,sum)
      names(multiplesCount) <- names(PM2.5_DF)
      # Print out the results, ignoring the 'datetime' column
      results <- multiplesCount[-1]
      if (any(results > 1)) {
        print(paste0('Some Instruments had multiple values per hour. Maximum values per hour are given below:'))
        print(multiplesCount[-1])
        print(paste0('The maximum number of values per hour was: ',max(valueCountPerCell[,-1],na.rm=TRUE)))
      }
    }
    
  }
  
  # Assign rownames
  rownames(PM2.5_DF) <- strftime(PM2.5_DF$datetime,"%Y%m%d%H",tz="GMT")
  
  # TODO:  Merge PM25.5_DF with oldData
  
#   # Overwrite data from hourlyDF with data from PM2.5_DF --------------------
#   
#   # First, add columns for any new monitors
#   for (name in names(PM2.5_DF)) {
#     if (!name %in% names(hourlyDF)) {
#       hourlyDF[[name]] <- as.numeric(NA)
#     }
#   }
#   # Now add the data
#   hourlyDF[rownames(PM2.5_DF),colnames(PM2.5_DF)] <- PM2.5_DF
#   PM2.5 <- hourlyDF
  
}


###############################################################################

###############################################################################
###############################################################################
###############################################################################
# Main program

# For interactive debugging:
if (FALSE) {
  
  opt <- list(sourceDir=paste0('~/monitors'),
              destDir=paste0(getwd()),
              files <- c('APCD_ebam_bam.csv','ARB2.csv','GBUAPCD.csv','mrpsa_ebam.csv','ODEQ.csv',
                         'TCAPCD_ebam.csv','USFS_ebam_bam.csv','WASHOE.csv'),
              debug=TRUE, meta=TRUE, PM2.5=TRUE, PM2.5_raw=TRUE)  
  
}


# ----- Parse command line options --------------------------------------------

sourceDir <- '~/monitors'
files <- c('APCD_ebam_bam.csv','ARB2.csv','GBUAPCD.csv','mrpsa_ebam.csv','ODEQ.csv',
           'TCAPCD_ebam.csv','USFS_ebam_bam.csv','WASHOE.csv')
destDir <- paste0(getwd())

# Set up OptionParser
option_list <- list(
  optparse::make_option(c("--sourceDir"), default=sourceDir, help="Source directory containing EBAM data files [default \"%default\"]"),
  optparse::make_option(c("--destDir"), default=destDir, help="Destination directory [default \"%default\"]"),
  optparse::make_option(c("--debug"), action="store_true", default=FALSE, help="Print out additional debugging information [default %default]"),
  optparse::make_option(c("--meta"), action="store_true", default=TRUE, help="Create AirNow_SitesMetadata.RData [default %default]"),
  optparse::make_option(c("--PM2.5"), action="store_true", default=TRUE, help="Create PM2.5 .RData [default %default]"),
  optparse::make_option(c("--PM2.5_raw"), action="store_true", default=TRUE, help="Create PM2.5_raw .RData [default %default]")
)

# Parse arguments
opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list))

# Create needed variables
files <- paste0(opt$sourceDir,'/',files)
now <- lubridate::now('GMT')
lastMonth <- lubridate::`%m-%`(now,months(1)) # Remember, operators are actually functions
dayStamp <- strftime(now,"%Y%m%d",tz="GMT")
thisMonthStamp <- strftime(now,"%Y%m",tz="GMT")
lastMonthStamp <- strftime(lastMonth,"%Y%m",tz="GMT")


# ----- Read in Raw Data ------------------------------------------------------

# Read in the latest files as a single "raw" dataframe
raw <- airsis_getData(files, debug=opt$debug)

# If some data already exist, merge new data with old, discarding duplicated
rawFile <- paste0(opt$destDir,'/AIRSIS_raw_',dayStamp,'.RData')
if (file.exists(rawFile)) {
  if (opt$debug) print(paste0('Merging new and old raw data ...'))
  oldRaw <- get(load(rawFile))
  raw <- dplyr::bind_rows(oldRaw,raw)
  raw <- raw[!duplicated(raw),]
}

if (opt$debug) print(paste0('Writing raw data ...'))

# Write out the .RData file
save(raw,file=rawFile)

# NOTE:  Assumes this script is run every hour.
if (lubridate::hour(now) == 23) {
  # At the end of the day also write out a .csv file
  write.csv(raw,file=paste0(opt$destDir,'/AIRSIS_raw_',dayStamp,'.csv'))
}

# Convert times to POSIXct
raw$`Date/Time/GMT` <- strptime(raw$`Date/Time/GMT`,"%m/%d/%Y %H:%M:%S %p",tz="GMT")
raw$`Start Date/Time (GMT)` <- strptime(raw$`Start Date/Time (GMT)`,"%d-%b-%Y %H:%M:%S",tz='GMT') # Different. Ugh!
raw$`TimeStamp` <- strptime(raw$`TimeStamp`,"%m/%d/%Y %H:%M:%S %p",tz="GMT")


# ----- Create monitorID ------------------------------------------------------

sitesMetadataFile <- paste0(opt$destDir,'/AIRSIS_SitesMetdadata.RData')
if (file.exists(sitesMetadataFile)) {
  oldMeta <- get(load(sitesMetadataFile))
} else {
  oldMeta <- NULL
}
# TODO:  airsis_createMonitorID()
# monitorID <- airsis_createMonitorID(raw, oldMeta, opt$debug)


# ----- QC and Harmonization --------------------------------------------------

# Quality control the raw data and convert times to POSIXct
reportFile <- paste0(opt$destDir,'/QC_report_',dayStamp,'.txt')
goodMask <- airsis_QCMask(raw, reportFile)
raw_qc <- raw[goodMask,]

# Harmonize the dataframe with internal standards
harmonized_raw <- airsis_harmonize(raw)
harmonized_qc <- airsis_harmonize(raw_qc)


# ----- Create SitesMetadata --------------------------------------------------

if (opt$meta) {
  # Generate metadata from _raw so we catch all possible monitors
  AIRSIS_SitesMetadata <- airsis_createSitesMetadata(harmonized_raw, oldMeta, debug=opt$debug)
  save(AIRSIS_SitesMetadata,file=sitesMetadataFile)
}


# ----- Create QC Data --------------------------------------------------------

if (opt$PM2.5) {
  
  # TODO:  How to backfill data in the previous month?
  #
  # Read in current and previous month and dplyr::bind_rows them together
  # Use this as oldPM2.5
  # Then airsis_createData to reshape/merge
  # Then split into lastMonth and thisMonth and save
  
  # NOTE:  We will use the current data to backfill any missing values. For this
  # NOTE:  we need to open the current and previous month's data files.
  lastMonthFile <- paste0(opt$destDir,'/AIRSIS_PM2.5_',lastMonthStamp,'.RData')
  thisMonthFile <- paste0(opt$destDir,'/AIRSIS_PM2.5_',thisMonthStamp,'.RData')
  
  if ( file.exists(lastMonthFile) && file.exists(thisMonthFile) ) {
    lastMonthPM2.5 <- get(load(lastMonthFile))
    thisMonthPM2.5 <- get(load(thisMonthFile))
    oldPM2.5 <- dplyr::bind_rows(lastMonthPM2.5,thisMonthPM2.5)
  } else if ( file.exists(lastMonthFile) ) {
    oldPM2.5 <- get(load(lastMonthFile))
  } else if ( file.exists(thisMonthFile) ) {
    oldPM2.5 <- get(load(thisMonthFile))
  } else {
    oldPM2.5 <- NULL
  }
  
  newPM2.5 <- airsis_createData(harmonized_qc, oldPM2.5, opt$debug)
  
  # NOTE:  The returned dataframe may have up to two months worth of data.
  # NOTE:  Split at the month boundary and save them separately.
  
  monthBegin <- lubridate::ymd_hms(strftime(now, "%Y-%m-01 00:00:00"), tz="UTC")
  
  # Last Month
  PM2.5 <- newPM2.5[datetime < monthBegin,]
  save(PM2.5,file=lastMonthFile)
  
  # This Month
  PM2.5 <- newPM2.5[datetime >= monthBegin,]
  save(PM2.5,file=lastMonthFile)
  
}

# ----- Create Raw Data --------------------------------------------------------

if (opt$PM2.5_raw) {
  
  # TODO:  How to backfill data in the previous month?
  #
  # Read in current and previous month and dplyr::bind_rows them together
  # Use this as oldPM2.5
  # Then airsis_createData to reshape/merge
  # Then split into lastMonth and thisMonth and save
  
  # NOTE:  We will use the current data to backfill any missing values. For this
  # NOTE:  we need to open the current and previous month's data files.
  lastMonthFile <- paste0(opt$destDir,'/AIRSIS_PM2.5_raw_',lastMonthStamp,'.RData')
  thisMonthFile <- paste0(opt$destDir,'/AIRSIS_PM2.5_raw_',thisMonthStamp,'.RData')
  
  if ( file.exists(lastMonthFile) && file.exists(thisMonthFile) ) {
    lastMonthPM2.5 <- get(load(lastMonthFile))
    thisMonthPM2.5 <- get(load(thisMonthFile))
    oldPM2.5 <- dplyr::bind_rows(lastMonthPM2.5,thisMonthPM2.5)
  } else if ( file.exists(lastMonthFile) ) {
    oldPM2.5 <- get(load(lastMonthFile))
  } else if ( file.exists(thisMonthFile) ) {
    oldPM2.5 <- get(load(thisMonthFile))
  } else {
    oldPM2.5 <- NULL
  }
  
  newPM2.5 <- airsis_createData(harmonized_raw, oldPM2.5)
  
  # NOTE:  The returned dataframe may have up to two months worth of data.
  # NOTE:  Split at the month boundary and save them separately.
  
  monthBegin <- lubridate::ymd_hms(strftime(now, "%Y-%m-01 00:00:00"), tz="UTC")
  
  # Last Month
  PM2.5 <- newPM2.5[datetime < monthBegin,]
  save(PM2.5,file=lastMonthFile)
  
  # This Month
  PM2.5 <- newPM2.5[datetime >= monthBegin,]
  save(PM2.5,file=lastMonthFile)
  
}

###############################################################################
# END OF SCRIPT
###############################################################################


