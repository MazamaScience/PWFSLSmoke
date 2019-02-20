########################################################################
# updateNotifyMonitors.R
#
# Function to update tables in the 'notify_monitors' schema based on analysis
# of recent monitoring data.
# 
# updateNotifyMonitors()    -- process latest data and update 'notify_monitors' schema
#
# Author: Jonathan Callahan jonathan@mazamascience.com
########################################################################

# -----------------------------------------------------------------------------
#' @export
#' @title Update Tables in the notify_monitors Schema
#' @param db_conn DBIConnection from the DBI \pkg{package}
#' @param baseUrl parameter passed on to the \code{PWFSLSmoke::~_loadLatest()} functions
#' @param outputDir directory in which to write a current_event_deployments.csv file
#' @return Invisibly returns 0 on success and 1 on error.
#' @description Latest AirNow, AIRSIS and WRCC data are loaded and processed into a suite
#' of \code{status} parameters from which specific \code{events} are generated.
#' 
#' Events are defined in the \code{monitor_event_types} table:
#' \enumerate{
#' \item{\code{USG6} --'NowCast level increased to Unhealthy for Sensitive Groups in last 6 hours'}
#' \item{\code{U6} --'NowCast level increased to Unhealthy in last 6 hours'}
#' \item{\code{VU6} --'NowCast level increased to Very Unhealthy in last 6 hours'}
#' \item{\code{HAZ6} --'NowCast level increased to Hazardous in last 6 hours'}
#' \item{\code{MOD6} --'NowCast level decreased to Moderate or Good in last 6 hours'}
#' \item{\code{NR6} --'Monitor not reporting for 6 or more hours'}
#' \item{\code{MAL6} --'Monitor malfunctioning 6 or more hours'}
#' \item{\code{NEW6} --'New monitor reporting in last 6 hours'}
#' }
#' 
#' The final step is the replacement of the \code{monitor_event_deployments} table
#' with new event-deployments that can then be joined with spatial areas and used to
#' generate notifications for subscribed users.
updateNotifyMonitors <- function(db_conn,
                                 outputDir=NULL) {
  
  logger.debug("----- updateNotifyMonitors -----")
  
  # ----- Load Latest Data -----
  
  logger.info("Loading latest data ...")
  ws_monitor <- loadLatest()
  
  # subset to last 7 days (to speed up but also guarantee proper calculation of NowCast and AQI)
  endtime <- lubridate::now()
  starttime <- lubridate::now() - lubridate::ddays(7)
  ws_monitor <- monitor_subset(ws_monitor, tlim=c(starttime, endtime))
  
  logger.info("Calculating nowcast ...")  
  # calculate nowcast
  mon_nowcast <- monitor_nowcast(ws_monitor)
  
  logger.info("Calculating aqi ...")  
  # and aqi
  mon_aqi <- monitor_aqi(ws_monitor)
  
  
  # ----- Load data from database ---------------------------------------------
  
  # TODO:  Should we update latency metrics for monitors which aren't in ws_monitor?
  # TODO:  Or do we just delete them from monitor_current_status?
  # TODO:  Or do nothing as is the case right now?
  
  # ----- Update tables in notify_monitors schema -----------------------------  
  
  # NOTE:  Use a single time for this processing cycle so we can easily find all
  # NOTE:  events associated with the latest cycle of processing
  
  processingUpdateTime <- lubridate::now(tz="UTC")
  
  # find last valid and prev valid indices
  dataBrick <- ws_monitor$data[,-1] # omit 'datetime'
  lastValidIndex <- apply(dataBrick, 2, function(x) { return(max(which(!is.na(x)))) })
  prevValidIndex <- apply(dataBrick, 2, function(x) { return( if( sum(!is.na(x))>1 ) { ( max( which(!is.na(x))[-sum(!is.na(x))] ) ) } else { NA } ) })
  
  logger.debug("Looping over %d monitors", nrow(ws_monitor$meta))
  # Loop over each monitor in turn
  for ( pwfsl_id in ws_monitor$meta$monitorID ) { 
    
    logger.trace("--- starting %s ---", pwfsl_id)
    
    statusList <- list()
    
    # ----- Update monitor_current_status table -------------------------------
    
    result <- try({
      
      currentIndex <- lastValidIndex[pwfsl_id]
      previousIndex <- prevValidIndex[pwfsl_id]
      
      
      # metadata
      statusList$pwfsl_id <- pwfsl_id
      statusList$airnow_id <- ws_monitor$meta[pwfsl_id,'AQSID']
      statusList$airsis_epa_id <- as.character(NA)
      statusList$airsis_apcd_id <- as.character(NA)
      statusList$airsis_arb2_id <- as.character(NA)
      statusList$airsis_usfs_id <- as.character(NA)
      statusList$wrcc_id <- as.character(NA)
      statusList$monitor_type <- ws_monitor$meta[pwfsl_id, 'monitorType']
      statusList$reported_status_flag <- ws_monitor$meta[pwfsl_id, 'status']
      statusList$site_name <- ws_monitor$meta[pwfsl_id, 'siteName']
      statusList$ara_name <- ws_monitor$meta[pwfsl_id, 'siteCode']
      statusList$state_code <- ws_monitor$meta[pwfsl_id, 'stateCode']
      statusList$country_code <- ws_monitor$meta[pwfsl_id, 'countryCode']
      statusList$timezone <- ws_monitor$meta[pwfsl_id, '']
      statusList$data_ingest_source <- as.character(NA)
      statusList$responsible_agency <- ws_monitor$meta[pwfsl_id, 'agencyName']
      statusList$longitude <- ws_monitor$meta[pwfsl_id, 'longitude']
      statusList$latitude <- ws_monitor$meta[pwfsl_id, 'latitude']
      statusList$elevation <- ws_monitor$meta[pwfsl_id, 'elevation']
      
      # times
      currentUpdateTime <- ws_monitor$data$datetime[currentIndex]
      previousUpdateTime <- ws_monitor$data$datetime[previousIndex]
      currentLatencyHourCount <- trunc( as.numeric( difftime(processingUpdateTime, currentUpdateTime, units="hours") ) )
      previousLatencyHourCount <- trunc( as.numeric( difftime(processingUpdateTime, previousUpdateTime, units="hours") ) )
      
      statusList$pm25_processing_update_time <- processingUpdateTime
      statusList$pm25_current_update_time <- currentUpdateTime
      statusList$pm25_previous_update_time <- previousUpdateTime
      statusList$pm25_current_latency_hour_count <- currentLatencyHourCount
      statusList$pm25_previous_latency_hour_count <- previousLatencyHourCount
      
      # current status
      statusList$pm25_current_1hr_value <- ws_monitor$data[currentIndex, pwfsl_id]
      statusList$pm25_current_1hr_nowcast <- mon_nowcast$data[currentIndex, pwfsl_id]
      statusList$pm25_current_1hr_aqi <- mon_aqi$data[currentIndex, pwfsl_id]
      statusList$pm25_current_1hr_category <- .bincode(mon_nowcast$data[currentIndex, pwfsl_id], AQI$breaks_24, include.lowest = TRUE)
      if (currentIndex > 3) {
        statusList$pm25_current_3hr_value <- mean(ws_monitor$data[(currentIndex-3):currentIndex, pwfsl_id], na.rm = TRUE)
      } else {
        logger.trace("Insufficient data for current 3hr mean")
        statusList$pm25_current_3hr_value <- as.numeric(NA)
      }
      if ( currentIndex > 23 ) {
        statusList$pm25_current_24hr_valid_data_count <- sum( !is.na( ws_monitor$data[(currentIndex-23):currentIndex, pwfsl_id] ) )
      } else {
        logger.trace("Insufficient data for current 24hr mean")
        statusList$pm25_current_24hr_valid_data_count <- as.integer(NA)
      }
      
      # previous status
      if( !is.na(previousIndex) ) {
        statusList$pm25_previous_1hr_value <- ws_monitor$data[previousIndex, pwfsl_id]
        statusList$pm25_previous_1hr_nowcast <- mon_nowcast$data[previousIndex, pwfsl_id]
        statusList$pm25_previous_1hr_aqi <- mon_aqi$data[previousIndex, pwfsl_id]
        statusList$pm25_previous_1hr_category <- .bincode(mon_nowcast$data[previousIndex, pwfsl_id], AQI$breaks_24, include.lowest = TRUE)
        if ( previousIndex > 3 ) {
          statusList$pm25_previous_3hr_value <- mean(ws_monitor$data[(previousIndex-3):currentIndex, pwfsl_id], na.rm = TRUE)
        } else {
          statusList$pm25_previous_3hr_value <- as.numeric(NA)
        }
      } else {
        statusList$pm25_previous_1hr_value <- as.numeric(NA)
        statusList$pm25_previous_1hr_nowcast <- as.numeric(NA)
        statusList$pm25_previous_1hr_aqi <- as.numeric(NA)
        statusList$pm25_previous_1hr_category <- as.integer(NA)
        statusList$pm25_previous_3hr_value <- as.numeric(NA)
      }
      
      # yesterday indices
      # NOTE:  Shenanigans required to get UTC yesterdayIndices
      # NOTE:  Note that lubridate::now() returns a 'POSIXct' while lubridate::today() returns a 'Date' object
      timezone <- ws_monitor$meta[pwfsl_id, 'timezone']
      localTodayStart <- lubridate::floor_date(lubridate::now(tz=timezone), unit="day")
      utcTodayStart <- lubridate::with_tz(localTodayStart, tz = "UTC")
      utcYesterdayStart <- utcTodayStart - lubridate::hours(24)
      yesterdayIndices <- which(ws_monitor$data$datetime >= utcYesterdayStart & ws_monitor$data$datetime < utcTodayStart)
      
      statusList$pm25_yesterday_24hr_valid_data_count <- sum( !is.na( ws_monitor$data[yesterdayIndices, pwfsl_id] ) )
      
      if ( statusList$pm25_yesterday_24hr_valid_data_count >= 18) {
        yesterdayAverage <- mean(ws_monitor$data[yesterdayIndices, pwfsl_id], na.rm = TRUE)
        statusList$pm25_yesterday_24hr_value <- yesterdayAverage
        statusList$pm25_yesterday_24hr_aqi <- mean(mon_aqi$data[yesterdayIndices, pwfsl_id], na.rm = TRUE)
        statusList$pm25_yesterday_24hr_category <- .bincode(yesterdayAverage, AQI$breaks_24, include.lowest = TRUE)
      } else {
        logger.trace("Insufficient data for yesterday 24hr mean")
        statusList$pm25_yesterday_24hr_value <- as.numeric(NA)
        statusList$pm25_yesterday_24hr_category <- as.integer(NA)
      }
      
      # Update the monitor_current_status table
      updateMonitorCurrentStatus(db_conn, pwfsl_id, statusList) 
      
      
    }, silent = TRUE ) # end of 'monitor_current_status' try block
    
    # handle errors
    if ("try-error" %in% class(result)) {
      err_msg <- geterrmessage()
      logger.debug("Failed to update monitor_current_status with pwfsl_id '%s'", pwfsl_id)
      logger.debug(err_msg)
    }
    
    logger.trace("Done updating monitor_current_status for %s", pwfsl_id)
    
    # ----- Update monitor_events table ---------------------------------------
    
    # USG6 -- NowCast level increased to Unhealthy for Sensitive Groups in the last 6 hours
    # U6   -- NowCast level increased to Unhealthy in the last 6 hours
    # VU6  -- NowCast level increased to Very Unhealthy in the last 6 hours
    # HAZ6 -- NowCast level increased to Hazardous in the last 6 hours
    # MOD6 -- NowCast level decreased to Moderate or Good in the last 6 hours
    # NR6  -- Monitor not reporting for more than 6 hours
    # MAL6 -- Monitor malfunctioning the last 6 hours
    # NEW6 -- New monitor reporting in the last 6 hours
    
    
    
    if (FALSE) { # DON'T USE monitor_current_status. Instead, find all the events from scratch
      # NOTE:  With this setup, I don't think we are ever using the monitor_current_status table. 
      # NOTE:  Is there any point in creating it every time????
      return <- try({
        
        
        logger.trace('current latency: %s', statusList$pm25_current_latency_hour_count)
        if ( statusList$pm25_current_latency_hour_count <= 6 ) {
          
          if ( currentIndex > 5 ) { # Shouldn't need this but it doesn't hurt
            
            # NOTE:  The pm25 'value' for the currentIndex should always be non-NA.
            # NOTE:  However, the pm25 'nowcast' for the currentIndex will be NA if the previous two hourly values were NA.
            # NOTE:  In that case we use the 'value' instead of the 'nowcast'.
            if ( is.na(mon_nowcast$data[currentIndex, pwfsl_id]) ) {
              pm25_values <- ws_monitor$data[(currentIndex-5):currentIndex, pwfsl_id]
            } else {
              pm25_values <- mon_nowcast$data[(currentIndex-5):currentIndex, pwfsl_id]
            }
            pm25_categories <- .bincode(pm25_values, AQI$breaks_24, include.lowest = TRUE)
            
            # Increased to a category in the last six hours
            if ( pm25_categories[6] == 6 && all(pm25_categories[1:5] < 6, na.rm=TRUE) ) {
              addMonitorEvent(db_conn, pwfsl_id, 'HAZ6', '', statusList$pm25_current_update_time, processingUpdateTime)
            } else if ( pm25_categories[6] == 5 && all(pm25_categories[1:5] < 5, na.rm=TRUE) ) {
              addMonitorEvent(db_conn, pwfsl_id, 'VU6', '', statusList$pm25_current_update_time, processingUpdateTime)
            } else if ( pm25_categories[6] == 4 && all(pm25_categories[1:5] < 4, na.rm=TRUE) ) {
              addMonitorEvent(db_conn, pwfsl_id, 'U6', '', statusList$pm25_current_update_time, processingUpdateTime)
            } else if ( pm25_categories[6] == 3 && all(pm25_categories[1:5] < 3, na.rm=TRUE) ) {
              addMonitorEvent(db_conn, pwfsl_id, 'USG6', '', statusList$pm25_current_update_time, processingUpdateTime)
            }
            
            # Decreased to a category in the last six hours
            if ( pm25_categories[6] <= 2 && all(pm25_categories[1:5] > 2, na.rm=TRUE) && sum(!is.na(pm25_values)>1)) {
              addMonitorEvent(db_conn, pwfsl_id, 'MOD6', '', statusList$pm25_current_update_time, processingUpdateTime)
            }
            
          }
          
          # New monitor reporting in the last 6 hours
          if ( sum(!is.na(mon_nowcast$data[,pwfsl_id])) == 1 ) {
            addMonitorEvent(db_conn, pwfsl_id, 'USG6', '', statusList$pm25_current_update_time, processingUpdateTime)
          }
          
          
        } else if ( statusList$pm25_current_latency_hour_count <= 12 ) {
          
          # SIX CONSECUTIVE ALERTS for Monitor not reporting for more than 6 hours
          addMonitorEvent(db_conn, pwfsl_id, 'NR6', '', statusList$pm25_current_update_time, processingUpdateTime)
          
        } else {
          
          # CONTINUOUS ALERT for Monitor not reporting for more than 6 hours
          ###addMonitorEvent(db_conn, pwfsl_id, 'NR6', '', statusList$pm25_current_update_time, processingUpdateTime)
          
        }
        
      }, silent=FALSE) # end of 'monitor_events' try block
      
      # handle errors
      if ("try-error" %in% class(result)) {
        err_msg <- geterrmessage()
        logger.debug("Failed to add to monitor_events with pwfsl_id '%s'", pwfsl_id)
        logger.debug(err_msg)
      }
    }
    
    logger.trace("Finished with %s", pwfsl_id)
    
  } # end of pwfsl_id loop
  
  logger.debug("Finished updating monitor_current_status table")
  
  if (TRUE) { # Use the database and some tidy magic
    # USG6 -- NowCast level increased to Unhealthy for Sensitive Groups in the last 6 hours
    # U6   -- NowCast level increased to Unhealthy in the last 6 hours
    # VU6  -- NowCast level increased to Very Unhealthy in the last 6 hours
    # HAZ6 -- NowCast level increased to Hazardous in the last 6 hours
    # MOD6 -- NowCast level decreased to Moderate or Good in the last 6 hours
    # NR6  -- Monitor not reporting for more than 6 hours
    # MAL6 -- Monitor malfunctioning the last 6 hours
    # NEW6 -- New monitor reporting in the last 6 hours
    
    current_events <- tbl(db_conn, 'monitor_current_status') %>%
      filter(pm25_current_latency_hour_count <= 6,
             pm25_processing_update_time == processingUpdateTime)
    
    usg6 <- current_events %>% 
      filter(pm25_previous_latency_hour_count - pm25_current_latency_hour_count < 6,
             pm25_current_1hr_category >=3,
             pm25_previous_1hr_category < 3) %>% 
      select(pwfsl_id, pm25_current_update_time) %>% collect() %>%
      mutate(code = "USG6")
    u6 <- current_events %>%
      filter(pm25_previous_latency_hour_count - pm25_current_latency_hour_count < 6,
             pm25_current_1hr_category >=4,
             pm25_previous_1hr_category < 4) %>% 
      select(pwfsl_id, pm25_current_update_time) %>% collect()%>%
      mutate(code = "U6")
    vu6 <- current_events %>%
      filter(pm25_previous_latency_hour_count - pm25_current_latency_hour_count < 6,
             pm25_current_1hr_category >= 5,
             pm25_previous_1hr_category < 5) %>% 
      select(pwfsl_id, pm25_current_update_time) %>% collect()%>%
      mutate(code = "VU6")
    haz6 <- current_events %>%
      filter(pm25_previous_latency_hour_count - pm25_current_latency_hour_count < 6,
             pm25_current_1hr_category >= 6,
             pm25_previous_1hr_category < 6) %>% 
      select(pwfsl_id, pm25_current_update_time) %>% collect()%>%
      mutate(code = "HAZ6")
    mod6 <- current_events %>%
      filter(pm25_previous_latency_hour_count - pm25_current_latency_hour_count < 6,
             pm25_current_1hr_category <= 2,
             pm25_previous_1hr_category > 2) %>% 
      select(pwfsl_id, pm25_current_update_time) %>% collect()%>%
      mutate(code = "MOD6")
    mal6 <- current_events %>%
      filter(pm25_current_latency_hour_count == 6) %>% 
      select(pwfsl_id, pm25_current_update_time) %>% collect()%>%
      mutate(code = "MAL6")
    events <- bind_rows(usg6,
                        u6,
                        vu6,
                        haz6,
                        mod6,
                        mal6)
    # Add All Events
    for (row in seq_len(nrow(events)) ) {
      event <- events[row,]
      addMonitorEvent(db_conn, event$pwfsl_id, event$code, '', event$pm25_current_update_time, processingUpdateTime)
    }
    
    
    
  }
  
  # ----- Update monitor_current_event_deployments table ----------------------
  
  logger.info('Updating current event deployments ...')
  updateMonitorCurrentEventDeploymentsTable(db_conn)
  
  # Now write a csv version of this file into outputDir
  currentEventDeployments <- dbReadTable(db_conn, 'monitor_current_event_deployments_vw')
  filename <- paste0('current_event_deployments_', strftime(lubridate::now('UTC'), "%Y%m%d%H", tz='UTC'), '.csv')
  filepath <- file.path(outputDir,filename)
  readr::write_csv(currentEventDeployments, filepath)
  
}