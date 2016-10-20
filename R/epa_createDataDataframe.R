epa_createDataDataframe <- function(df, verbose){
  if (verbose) cat(paste0('   Creating data dataframe ...\n'))
  
  subDF <- df[,c("monitorID","datetime","Sample Measurement")]
  # The data file will refer to any of the original instrument's readings, the bulk of the information.
  
  # "melt" the data frame into long-format data
  # The "melt" function will turn the column names into their own column and the rest of the data into a second.
  melted <- reshape2::melt(data=subDF, id.vars = c("datetime","monitorID"), measure.vars="Sample Measurement")
  
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
  
  logger.debug("'data' dataframe has %d rows and %d columns", nrow(data), ncol(data))
  
  return(data)
  
  # # "cast" the data frame into wide-format data with country code column names
  # # The "cast" function will take the melted version of the data and and move desired columns out of their 
  # # Original column and into their own columns.
  # data <- reshape2::dcast(molten, datetime ~ monitorID)
  # 
  # # Add rownames
  # # TODO:  Figure out what is up with non-unique %Y%m%d%H
  # # TODO:  rownames(data) <- strftime(data$datetime,"%Y%m%d%H") ### ERROR:  non-unique row names 2013110301 (daylight savings issue?)
}