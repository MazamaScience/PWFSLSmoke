openaq_createMetaDataframe <- function(df){
  # Sanity check -- deploymentID must exist
  if ( !'deploymentID' %in% names(df) ) {
    logger.error("The 'df' dataframe does not have a 'deploymnetID' column")
    stop(paste0("The 'df' dataframe does not have a 'deploymentID' column. Have you run addClustering()?"))
  }
  
  ## right now the data frame has column names:
  ## [1] "location"     "city"         "country"      "utc"          "local"        "parameter"   
  ## [7] "value"        "unit"         "latitude"     "longitude"    "attribution"  "deploymentID"
  ## create 21 columns for meta as a standard
  uniqueDeploymentID <- unique(df$deploymentID)
  meta <- data.frame(matrix(nrow = length(uniqueDeploymentID), ncol = 21),
                     row.names = uniqueDeploymentID)
  names(meta) <- c('AQSID','siteCode','siteName','status',
                'agencyID','agencyName','EPARegion','latitude',
                'longitude','elevation','timezone','GMTOffsetHours',
                'countryCode','FIPSMSACode','MSAName','FIPSStateCode',
                'stateCode','GNISCountyCode','countyName','monitorID',
                'monitorType')
  
  #assign distinct values to meta accordingly
  indexes <- !duplicated(df$deploymentID)
  meta$siteName <- df$location[indexes]
  meta$latitude <- df$latitude[indexes]
  meta$longitude <- df$longitude[indexes]
  

  
  # ################
  
  # This used to be in openaq_createMonitorObject
  
  # # add stateCode, countryCode and deploymentID column where deploymentID is a unique identifier
  # # composed of latitude and longitude
  # 
  # ## NOTE: stateCode includes Mexican codes TM and CH, Canadian code BC
  # ## TODO: need to manually change foreign codes to US, add sanity check, probably a new function
  # df$stateCode <- getStateCode(df$longitude,df$latitude,useBuffering = T)
  # 
  # #check to see if there is any NA
  # #sum(is.na(df$stateCode))
  # 
  # #check to see if there is non-US states
  # USstateCode <- NaturalEarthAdm1$stateCode[which(NaturalEarthAdm1$countryCode=='US')]
  # nonUSstateCode <- dplyr::setdiff(df$stateCode,USstateCode)
  # # UScities <- stringr::str_replace(us.cities[,1], us.cities[,2], '')
  # # UScities <- stringr::str_trim(UScities)
  # stateCodeChange <- c('TX','PR','TX','ID') ### TODO improve, not assuming order
  # for (i in 1:4) {
  #   df$stateCode[which(df$stateCode==nonUSstateCode[i])] <- stateCodeChange[i]
  # }
  # 
  # # replace SimpleCountries with TMWorldBorders to get more accurate results 
  # # df$countryCode <- getCountryCode(df$longitude,df$latitude,dataset = 'TMWorldBorders',useBuffering = T)
  # df$deploymentID <- with(df, paste(latitude, longitude,sep = ', '))
  # ################
  
  # ################
  
  # This is from airsis_createMetaDataframe
  
  # # Create empty dataframe
  # meta <- as.data.frame(matrix(nrow=nrow(df),ncol=21))
  # 
  # colNames <- c('AQSID','siteCode','siteName','status',
  #               'agencyID','agencyName','EPARegion','latitude',
  #               'longitude','elevation','timezone','GMTOffsetHours',
  #               'countryCode','FIPSMSACode','MSAName','FIPSStateCode',
  #               'stateCode','GNISCountyCode','countyName','monitorID',
  #               'monitorType')
  # 
  # names(meta) <- colNames
  # 
  # # Assign data where we have it
  # # NOTE:  We use monitorID as a unique identifier instead of AQSID so that we can be
  # # NOTE:  consistent when working with non-AirNow datasets.
  # meta$longitude <- df$medoidLon
  # meta$latitude <- df$medoidLat
  # meta$monitorID <- paste0(make.names(df$monitorName),'__',sprintf("%03d",df$deploymentID))
  # meta$monitorType <- df$monitorType
  # 
  # # Assign rownames
  # rownames(meta) <- meta$monitorID
  # 
  # # Add timezones, state and country codes
  # meta <- addMazamaMetadata(meta)
  # 
  ### Fix up state and country codes
  #
  # # Add elevation, siteName and countyName
  # meta <- addGoogleMetadata(meta)
  # 
  # # Convert some columns to character even if they have all NA
  # characterColumns <- c('AQSID','siteCode','siteName','countyName','timezone','monitorID','monitorType')
  # for (colName in characterColumns) {
  #   meta[[colName]] <- as.character(meta[[colName]])
  # }
  # 
  # logger.debug("'meta' dataframe has %d rows and %d columns", nrow(meta), ncol(meta))
  
  # ################
  
  

  
  return(meta)
}

openaq_createDataDataframe <- function(df){
  subDF <- df[,c("datetime","value","deploymentID")]
  melted <- reshape2::melt(subDF,id.vars=c("datetime","deploymentID"))
  
  # check if it's exactly one measurement per hour at each location
  countValuePerHr <- reshape2::dcast(melted,datetime~deploymentID,length)
  maxCount <- max(countValuePerHr[,-1])
  if (maxCount>1) logger.warn('Up to %s measurements per hour -- median used',maxCount)
  
  # create a dataframe for values
  pm25DF <- reshape2::dcast(melted,datetime~deploymentID,median)
  
  # create a dataframe for hours
  hourlyDF <- data.frame(seq(min(melted$datetime),max(melted$datetime),by="hours"))
  names(hourlyDF) <- "datetime"
  
  # combine the two dataframes together by doing a left join
  data <- dplyr::left_join(hourlyDF,pm25DF,by="datetime")
  
  return(data)
}

openaq_createMonitorObject <- function(startdate='') {
  
  # download the openaq data as a dataframe
  df <- openaq_downloadData(startdate = '20161001')
  
  # add datetime and monitorID column
  df$datetime <- lubridate::ymd_hms(df$local)
  uniqueLatLon <- unique(paste(df$latitude, df$longitude))
  uniqueLatLon <- stringr::str_split_fixed(uniqueLatLon, ' ', 2)
  colnames(uniqueLatLon) <- c("latitude","longitude")
  stateCodes <- getStateCode(uniqueLatLon[,"longitude"],uniqueLatLon[,"latitude"]) ### TODO: to correct there is error
  
  # create metadata for the data frame
  meta <- openaq_createMetaDataframe(df)
  
  # create datadata for the data frame
  data <- openaq_createDataDataframe(df)
  
  # create the ws_monitor object
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class=c("ws_monitor", "list"))
  
  return(ws_monitor)
}