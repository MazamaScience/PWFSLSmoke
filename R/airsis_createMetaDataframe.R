#' @keywords AIRSIS
#' @export
#' @title Create Sites Metadata Dataframe
#' @param df single site AIRSIS dataframe after clustering
#' @param verbose logical flag to generate verbose output
#' @description Explain this function
#' 
#' @seealso \link{airsis_downloadData}
#' @seealso \link{addClustering}
#' @return ws_monitor object with a unique `monitorID` for each unique location


airsis_createMetaDataframe <- function(df, verbose=FALSE) {
  
  if ( !'deploymentID' %in% names(df) ) stop(paste0("The 'df' dataframe does not have a 'deploymentID' column.  Have you run addClustering()?"))
  
  # Pull out unique deployments
  df <- df[!duplicated(df$deploymentID),]
  
  # Our dataframe now contains the following columns:
  #
  #   > names(df)
  #    [1] "MasterTable_ID" "Alias"          "Latitude"       "Longitude"      "Date.Time.GMT"  "COncRT"        
  #    [7] "ConcHr"         "Flow"           "W.S"            "W.D"            "AT"             "RHx"           
  #   [13] "RHi"            "BV"             "FT"             "Alarm"          "Type"           "Serial.Number" 
  #   [19] "Version"        "Sys..Volts"     "TimeStamp"      "PDate"          "monitorName"    "datetime"      
  #   [25] "deploymentID"   "medoidLon"      "medoidLat"     
  #
  # On 2016-01-31, the following columns were found in:
  #   http://smoke.airfire.org/RData/AirNowTech/AirNowTech_PM2.5_SitesMetadata.RData
  #
  #   > meta <- get(load('~/Downloads/AirNowTech_PM2.5_SitesMetadata.RData'))
  #   > str(meta)
  #   'data.frame':	1106 obs. of  24 variables:
  #     $ AQSID         : chr  "000020301" "000030701" "000040801" "000040203" ...
  #   $ siteCode      : chr  "0301" "0701" "0801" "0203" ...
  #   $ siteName      : chr  "WELLINGTON" "AYLESFORD MOUNTAIN" "CANTERBURY" "FOREST HILLS" ...
  #   $ status        : Factor w/ 2 levels "Active","Inactive": 1 1 1 1 1 1 1 1 1 1 ...
  #   $ agencyID      : Factor w/ 125 levels "AB1","AK1","AL1",..: 27 27 27 27 27 27 27 27 30 30 ...
  #   $ agencyName    : Factor w/ 125 levels "Alabama Department of Environmental Management",..: 27 27 27 27 27 27 27 27 61 61 ...
  #   $ EPARegion     : Factor w/ 13 levels "CA","MX","R1",..: 1 1 1 1 1 1 1 1 1 1 ...
  #   $ latitude      : num  46.5 45 46 45.3 46 ...
  #   $ longitude     : num  -64 -65 -67.5 -66 -66.6 ...
  #   $ elevation     : num  33.9 230 0 57 0 ...
  #   $ GMTOffsetHours: num  -4 -4 -4 -4 -4 -4 -4 -4 -5 -5 ...
  #   $ countryCode   : Factor w/ 3 levels "CA","MX","US": 1 1 1 1 1 1 1 1 1 1 ...
  #   $ FIPSCMSACode  : Factor w/ 1 level "": 1 1 1 1 1 1 1 1 1 1 ...
  #   $ CMSAName      : Factor w/ 1 level "": 1 1 1 1 1 1 1 1 1 1 ...
  #   $ FIPSMSACode   : Factor w/ 467 levels "","10140","10180",..: 1 1 1 1 1 1 1 1 1 1 ...
  #   $ MSAName       : Factor w/ 467 levels "","Aberdeen, WA",..: 1 1 1 1 1 1 1 1 1 1 ...
  #   $ FIPSStateCode : Factor w/ 56 levels "00","01","02",..: 1 1 1 1 1 1 1 1 1 1 ...
  #   $ stateCode     : Factor w/ 55 levels "AK","AL","AR",..: 6 6 6 6 6 6 6 6 6 6 ...
  #   $ GNISCountyCode: Factor w/ 1027 levels "00001","00002",..: 2 3 4 4 4 4 4 4 5 5 ...
  #   $ countyName    : Factor w/ 789 levels "ABBEVILLE","ADA",..: 578 514 499 499 499 499 499 499 585 585 ...
  #   $ GNISCityCode  : Factor w/ 1 level "": 1 1 1 1 1 1 1 1 1 1 ...
  #   $ cityName      : Factor w/ 1 level "": 1 1 1 1 1 1 1 1 1 1 ...
  #   $ timezone      : chr  "America/Halifax" "America/Halifax" "America/Moncton" "America/Moncton" ...
  #   $ monitorID     : chr  "000020301" "000030701" "000040801" "000040203" ...
  #
  #     
  # We will create a reduced version for the AIRSIS data with at least:
  # 
  #  [1] "AQSID"          "siteCode"       "siteName"       "status"        
  #  [5] "agencyID"       "agencyName"     "EPARegion"      "latitude"      
  #  [9] "longitude"      "elevation"      "timezone"       "GMTOffsetHours" 
  # [13] "countryCode"    "FIPSMSACode"    "MSAName"        "FIPSStateCode"   
  # [17] "stateCode"      "GNISCountyCode" "countyName"     "monitorID"
  # 
  # Many of these fields will be empty
  
  # TODO:  Further reduce metadata columns to omit FIPS and GNIS codes, etc.
  
  # Create empty dataframe
  meta <- as.data.frame(matrix(nrow=nrow(df),ncol=20))
  
  colNames <- c('AQSID','siteCode','siteName','status',
                'agencyID','agencyName','EPARegion','latitude',
                'longitude','elevation','timezone','GMTOffsetHours',
                'countryCode','FIPSMSACode','MSAName','FIPSStateCode',
                'stateCode','GNISCountyCode','countyName','monitorID')
  
  names(meta) <- colNames
  
  # Assign data where we have it
  # NOTE:  We use monitorID as a unique identifier instead of AQSID so that we can be
  # NOTE:  when working with non-AirNow datasets.
  meta$longitude <- df$medoidLon
  meta$latitude <- df$medoidLat
  meta$monitorID <- paste0(make.names(df$monitorName),'__',sprintf("%03d",df$deploymentID))
  
  # Assign rownames
  rownames(meta) <- meta$monitorID
  
  # Use the cluster medoids to assign timezones, state and country codes
  ###library(MazamaSpatialUtils)
  setSpatialDataDir('~/Data/Spatial')
  loadSpatialData('NaturalEarthAdm1')
  meta$timezone <- MazamaSpatialUtils::getTimezone(meta$longitude, meta$latitude, useBuffering=TRUE)
  meta$countryCode <- MazamaSpatialUtils::getCountryCode(meta$longitude, meta$latitude, useBuffering=TRUE)
  meta$stateCode <- MazamaSpatialUtils::getStateCode(meta$longitude, meta$latitude, useBuffering=TRUE)
  
  # TODO:  Could assign other spatial identifiers like EPARegion, etc.
  
  # agencyName
  NPSMask <- stringr::str_detect(df$Alias,'^NPS ')
  USFSMask <- stringr::str_detect(df$Alias,'^USFS')
  meta$agencyName[NPSMask] <- 'National Park Service'
  meta$agencyName[USFSMask] <- 'United States Forest Service'
  
  
  # Assign column classes
  
  # Convert some columns to character even if they have all NA
  characterColumns <- c('AQSID','siteCode','siteName','timezone','monitorID')
  for (colName in characterColumns) {
    meta[[colName]] <- as.character(meta[[colName]])
  }
  
#   # Convert some columns to factor ONLY IF some values are found
#   factorColumns <- c('status','agencyID','agencyName','EPARegion',
#                      'countryCode','FIPSMSACode','MSAName','FIPSStateCode','stateCode',
#                      'GNISCountyCode','countyName')
#   
#   for (colName in factorColumns) {
#     if ( any(!is.na(meta[[colName]])) ) {
#       meta[[colName]] <- as.factor(meta[[colName]])
#     }
#   }
  
  # ----- Add elevation data (meters) from Google API ---------------------------
  
  # Create url
  urlBase <- 'https://maps.googleapis.com/maps/api/elevation/json?locations='
  locations <- paste(meta$latitude,meta$longitude,sep=',',collapse='|')
  url <- paste0(urlBase,locations)
  
  # Get and parse the return which has elements 'results' and 'status'
  googleReturn <- httr::content(httr::GET(url))
  
  # Check results
  if (googleReturn$status != 'OK') {
    
    cat(paste0('Google status was "',googleReturn$status,'" for URL:\n\t',url))
    
  } else {
    
    # Convert list of lists to list of dataframes
    tempList <- lapply(googleReturn$results,as.data.frame)
    # Combine individual dataframes
    elevationDF <- dplyr::bind_rows(tempList)
    
    # Sanity check that things came back in the same order
    if ( !all(meta$lat == elevationDF$location.lat) || !all(meta$lon == elevationDF$location.lon) ) {
      cat(paste0('Something is wrong with station elevation ordering.'))
    } else {
      meta$elevation <- elevationDF$elevation
    }
    
  }
  
  # ----- Add siteName from Google API ---------------------------
  
  # When siteName is missing, create one similar to AirNow with "locality-route"
  
  # Use ggmap::revgeocode to return a dataframe with (address, street number, route, locality , ...)
  # (2500 queries allowed per day in August, 2015)
  if ('ggmap' %in% installed.packages()[,1]) {
    
    for (i in 1:nrow(meta)) {
      if (is.na(meta[i,'siteName'])) {
        location <- c(meta$longitude[i],meta$latitude[i])
        if (verbose) cat(paste('Google address request for location = ',location[1],',',location[2],'.\n'))
        if (!anyNA(location)) {
          address <- ggmap::revgeocode(location, output='more')
          cat(paste0(address$address,'\n'))
          meta$siteName[i] <- paste(address$locality,address$route,sep='-')
          # NOTE:  administrative_area_level_2 is not always present
          try( meta$countyName[i] <- stringr::str_replace(address$administrative_area_level_2,' County',''),
               silent=FALSE )
        }
      }
    }
    
  }
  
  return(meta)
  
}
