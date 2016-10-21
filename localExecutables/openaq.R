openaq_createMetaDataframe <- function(df){
  df <- df[!duplicated(df$monitorID),]
  meta <- data.frame(matrix(nrow = nrow(df), ncol = 7), row.names = df$monitorID)
  
  names(meta) <- c('siteName', 'latitude', 'longitude', 'timezone',
                'countryCode', 'stateCode', 'monitorID')
  
  for (name in names(meta)[-c(1,4,5)]) {
    meta[[name]] <- df[[name]]
  }
  meta$siteName <- df$location
  meta$countryCode <- 'US'
  meta$timezone <- MazamaSpatialUtils::getTimezone(meta$longitude,meta$latitude,useBuffering = T)

  return(meta)
}

openaq_createDataDataframe <- function(df){
  subDF <- df[,c("datetime","value","monitorID")]
  melted <- reshape2::melt(subDF,id.vars=c("datetime","monitorID"))
  
  # check if it's exactly one measurement per hour at each location
  countValuePerHr <- reshape2::dcast(melted,datetime~monitorID,length)
  maxCount <- max(countValuePerHr[,-1])
  if (maxCount>1) logger.warn('Up to %s measurements per hour -- median used',maxCount)
  
  # create a dataframe for values
  pm25DF <- reshape2::dcast(melted,datetime~monitorID,median)
  
  # create a dataframe for hours
  hourlyDF <- data.frame(seq(min(melted$datetime),max(melted$datetime),by="hours"))
  names(hourlyDF) <- "datetime"
  
  # combine the two dataframes together by doing a left join
  data <- dplyr::left_join(hourlyDF,pm25DF,by="datetime")
  rownames(data) <- format(data$datetime,"%Y%m%d%H",tz="GMT")
  
  return(data)
}

openaq_createMonitorObject <- function(startdate='') {
  
  # download the openaq data as a dataframe
  df <- openaq_downloadData(startdate = startdate)
  
  # add datetime and monitorID column
  df$datetime <- lubridate::ymd_hms(df$local)
  
  # extract unique combinations of latitudes and longitudes for faster buffering process
  uniqueLatLon <- unique(paste(df$latitude, df$longitude))
  uniqueLatLon <- stringr::str_split_fixed(uniqueLatLon, ' ', 2)
  colnames(uniqueLatLon) <- c("latitude", "longitude")
  uniqueLatLon <- apply(uniqueLatLon,2,as.numeric)
  stateCodes <- getStateCode(uniqueLatLon[,"longitude"], uniqueLatLon[,"latitude"], useBuffering = T) 
  
  # correct non-US state codes  
  stateCodes[which(stateCodes == '')] <- 'PR'
  stateCodes[which(stateCodes == 'TM' | stateCodes == 'CH')] <- 'TX'
  stateCodes[which(stateCodes == 'BC')] <- 'ID'
  
  # assign state codes accordingly
  df$stateCode <- NA
  for (i in 1:nrow(df)) {
    latIndex <- which(uniqueLatLon[, 1] == df$latitude[i])
    lonIndex <- which(uniqueLatLon[, 2] == df$longitude[i])
    df$stateCode[i] <- stateCodes[intersect(latIndex,lonIndex)]
  }
  
  # create a monitorID column as unique identifier 
  df$monitorID <- with(df,paste(location,city,stateCode,sep=', '))
  
  # create metadata for the data frame
  meta <- openaq_createMetaDataframe(df)
  
  # create datadata for the data frame
  data <- openaq_createDataDataframe(df)
  
  # create the ws_monitor object
  ws_monitor <- list(meta=meta, data=data)
  ws_monitor <- structure(ws_monitor, class=c("ws_monitor", "list"))
  
  return(ws_monitor)
}