###############################################################################
# DNR Pilot Burn Pojrect (2928)
#
# The R code for the DNR pilot burn project data analysis is broken down into 
# the following steps, each associated with a separate file:
#
# * DNR_downloadData.R   -- download, QC, reshape and convert into .RData format
# * DNR_ingestData.R     -- ingest previously converted data and peroform any cleanup
#                           (e.g. convert negative values of PM2.5 to 0.0)
#
# Once all of this work is done, we are ready for the plotting scripts:
#
# * DNR_timeseriesPlot.R -- timeseries plot for a monitor of interest
# * DNR_burnMap.R        -- map of all burns near a monitoring location
###############################################################################

# This DNR_downloadData.R script downloads all of the data sources needed and
# creates local version of the data in the localData/ directory.

library(PWFSLSmoke)

# Set up MazamaSpatialUtils for metadata creation
library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

# Set up logging and log to the console all DEBUG and higher logging statements
logger.setup()
logger.setLevel(INFO)

start <- 20160901
end <- 20161015

# ----- AIRSIS raw monitoring data --------------------------------------------

if ( !file.exists('localData/airsis_rawList.RData') ) {
  
  # Susan O'Neill provided these data URLs:
  #
  #   http://www.wrcc.dri.edu/cgi-bin/rawMAIN4.pl?idsmf1 
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1012 -- EBAM
  #   http://apcd.airsis.com/vision/UnitHistory.aspx?uid=1013 -- EBAM
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1031 -- EBAM
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1032 -- EBAM
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1033 -- EBAM
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1034 -- EBAM
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1049 -- ESAM
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1050 -- ESAM
  
  airsis_rawList <- list()
  
  airsis_rawList[['FishHatchery']] <- airsis_createRawDataframe('USFS', '1012', start, end)  # Kettle Falls-Sherman Pass Scenic Byway
  airsis_rawList[['KennedMeadows']] <- airsis_createRawDataframe('APCD', '1013', start, end) # Manson-Totem Pole Road
  airsis_rawList[['Naches']] <- airsis_createRawDataframe('EPA', '1014', start, end)         # Naches-U.S. 12
  airsis_rawList[['Liberty']] <- airsis_createRawDataframe('USFS', '1031', start, end)       # Cle Elum-Liberty Road
  airsis_rawList[['Usk']] <- wrcc_createRawDataframe('smf1', start, end)                     # Cusick-Le Clerc Road North
  airsis_rawList[['Plain']] <- airsis_createRawDataframe('USFS', '1033', start, end)         # Leavenworth-Beaver Valley Road
  airsis_rawList[['Curlew']] <- airsis_createRawDataframe('USFS', '1034', start, end)        # Curlew-Customs Road
  airsis_rawList[['Nile']] <- airsis_createRawDataframe('USFS', '1049', start, end)          # Naches-Washington 410
  airsis_rawList[['KettleFalls']] <- airsis_createRawDataframe('USFS', '1050', start, end)   # Kettle Falls-West 11th Avenue
  
  save(airsis_rawList, file='localData/airsis_rawList.RData')
  
}


# ----- AIRSIS "ws_monitor" objects -------------------------------------------

if ( !file.exists('localData/airsis_monitorList.RData') ) {
  
  # Susan O'Neill provided these data URLs:
  #
  #   http://www.wrcc.dri.edu/cgi-bin/rawMAIN4.pl?idsmf1 
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1012 -- EBAM
  #   http://apcd.airsis.com/vision/UnitHistory.aspx?uid=1013 -- EBAM
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1031 -- EBAM
  #XXXhttp://usfs.airsis.com/vision/UnitHistory.aspx?uid=1032 -- EBAM
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1033 -- EBAM
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1034 -- EBAM
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1049 -- ESAM
  #   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1050 -- ESAM
  
  airsis_monitorList <- list()
  
  airsis_monitorList[['FishHatchery']] <- airsis_createMonitorObject('USFS', '1012', start, end)  # Kettle Falls-Sherman Pass Scenic Byway
  airsis_monitorList[['KennedMeadows']] <- airsis_createMonitorObject('APCD', '1013', start, end) # Manson-Totem Pole Road
  airsis_monitorList[['Naches']] <- airsis_createMonitorObject('EPA', '1014', start, end)         # Naches-U.S. 12
  airsis_monitorList[['Liberty']] <- airsis_createMonitorObject('USFS', '1031', start, end)       # Cle Elum-Liberty Road
  airsis_monitorList[['Usk']] <- wrcc_createMonitorObject('smf1', start, end)                     # Cusick-Le Clerc Road North
  airsis_monitorList[['Plain']] <- airsis_createMonitorObject('USFS', '1033', start, end)         # Leavenworth-Beaver Valley Road
  airsis_monitorList[['Curlew']] <- airsis_createMonitorObject('USFS', '1034', start, end)        # Curlew-Customs Road
  airsis_monitorList[['Nile']] <- airsis_createMonitorObject('USFS', '1049', start, end)          # Naches-Washington 410
  airsis_monitorList[['KettleFalls']] <- airsis_createMonitorObject('USFS', '1050', start, end)   # Kettle Falls-West 11th Avenue
  
  # WRCC monitor 'smf1' had three deployments, the last of which was in Usk
  airsis_monitorList[['Usk']] <- monitor_subset(airsis_monitorList[['Usk']], monitorIDs='FWS.Smoke..1__003')
  
  # Combine airsis_monitorList into a single ws_monitor object
  airsis_monitors <- monitor_combine(airsis_monitorList)
  
  save(airsis_monitors, file='localData/airsis_monitors.RData')
  
}

# ----- AirNow monitors of interest -------------------------------------------

if ( !file.exists('localData/airnow_monitors.RData') ) {
  
  us <- airnow_load(start, end)
  
  monitorIDs <- c('530770015', # Toppenish-Yakima Tribe
                  '530770016', # White Swan-Yakima Tribe
                  '530770009', # Yakima-4th Ave
                  '530370002', # Ellensburg-Ruby St
                  '530070011', # Wenatchee-Fifth St
                  '530070010', # Leavenworth-Evans St
                  '530070007', # Chelan-Woodin Ave
                  '530470009', # Twisp-Glover St
                  '530470010', # Winthrop-Chewuch Rd
                  '530470013'  # Omak-Colville Tribe
  )
  
  airnow_monitors <- monitor_subset(us, monitorIDs=monitorIDs)
  
  save(airnow_monitors, file='localData/airnow_monitors.RData')
  
}

# ----- DNR Smoke Management Approvals database -------------------------------

if ( !file.exists('localData/dnr_smokeManagementApprovalsRaw.RData') ) {
  
  # Screen-scrape DNR website to build dataframe of Smoke Management Approvals
  #
  # URLs of the form:
  #   "https://fortress.wa.gov/dnr/protection/BurnRequests/View/05-11-2016"
  
  # Configurable base URL
  baseUrl <- "https://fortress.wa.gov/dnr/protection/BurnRequests/View/"
  
  # Create a daily time axis and associated datestamps
  startdate <- parseDatetime(start)
  enddate <- parseDatetime(end)
  timeAxis <- seq(startdate, enddate, by="day")
  datestamps <- strftime(timeAxis, "%m-%d-%Y", tz="GMT")
  
  # List to store daily dataframes
  dfList <- list()
  
  # Screen-scrape daily tables --------------------------------------------------
  
  for (datestamp in datestamps) {
    url <- paste0(baseUrl,datestamp)
    print(paste0("Downloading ",url))
    wikiDoc <- xml2::read_html(url)
    tables <- rvest::html_nodes(wikiDoc, "table")
    df <- rvest::html_table(tables[[1]])
    # Replace "empty" tables with NA except for the comments column
    if ( nrow(df) == 1 && stringr::str_detect(df$Permit[1], "No smoke management request") ) {
      for ( name in setdiff(names(df), c('Comments')) ) df[[name]] <- NA
    }
    df$datestamp <- datestamp
    dfList[[datestamp]] <- df
  }
  
  # Combine daily tables into a single dataframe
  df <- dplyr::bind_rows(dfList)
  
  # Clean up dataframe ----------------------------------------------------------
  
  # Fix doubled Permit column, matching initial non-whitespace characters
  df$Permit <- stringr::str_match(df$Permit,'^\\S+')[,1]
  
  # Separate Longitude and Latitude
  location <- stringr::str_split_fixed(df$`Lat/Lon`,' ',2)
  df$Latitude <- location[,1]
  df$Longitude <- location[,2]
  df$`Lat/Lon` <- NULL
  
  # Create DNR_Pilot column
  pattern <- "24 hour|24hr|24 hr|2928|pilot|Pilot"
  df$DNR_Pilot <- stringr::str_detect(df$Comments,pattern)
  
  df$datetime <- lubridate::mdy(df$datestamp, tz="America/Los_Angeles")
  
  dnr_smokeManagementApprovalsRaw <- df
  
  # Save output as CSV to share with other folks
  readr::write_csv(dnr_smokeManagementApprovalsRaw, path="localData/dnr_smokeManagementApprovalsRaw.csv")
  
}

# ----- BlueSky fire locations ------------------------------------------------

if ( !file.exists('localData/bluesky_eventsList.RData') ) {
  
  # Configurable elements
  dailyOutputDir <- 'standard'
  model <- 'PNW-1.33km'
  baseUrl <- "http://smoke.airfire.org/bluesky-daily/output"
  
  # ---- Download and return Bluesky Events -----------------------------------
  
  # Create an empty list for model data
  bluesky_eventsList <- list()
  
  # Create a daily time axis and associated datestamps
  startdate <- parseDatetime(start)
  enddate <- parseDatetime(end)
  timeAxis <- seq(startdate, enddate, by="day")
  datestamps <- strftime(timeAxis, "%Y%m%d", tz="GMT")
  
  # Download the model runs. If the model doesn't exist then skip to the next one. 
  for (datestamp in datestamps) {
    
    # Turn the datestamp into a modelRun by appending '00' for hours
    modelRun <- paste0(datestamp,"00")
    
    fire_locations_csv <- paste0(baseUrl, "/", dailyOutputDir, "/", model, "/", modelRun, "/forecast/data/fire_locations.csv")
    fire_events_csv <- paste0(baseUrl, "/", dailyOutputDir, "/", model, "/", modelRun, "/forecast/data/fire_events.csv")
    
    lines <- readr::read_lines(fire_locations_csv)
    
    if ( length(lines) == 1 ) {
      
      # No fires
      bluesky_eventsList[[datestamp]] <- NULL
      
    } else {
      
      # Read in the raw data files
      locations <- readr::read_csv(fire_locations_csv, col_types='ccddcdcddccccdddddddddddddddddddddddddddddddddddddddddddddddcccccccc')
      events <- readr::read_csv(fire_events_csv, col_types='ccddddddddddddddddddddd')
      
      # modify the event$id name so we can "join"
      names(events)[1] <- 'event_id'
      
      df <- dplyr::left_join(locations, events, by="event_id")
      
      # Create a YYYYMMDD 'datestamp' column and a POSIXct 'datetime'
      # NOTE:  'date_time' currently contains something like "201509090000-07:00" (GMT datestamp plus GMT offset)
      df$datestamp <- stringr::str_sub(df$date_time,1,8)
      df$datetime <- lubridate::ymd(df$datestamp)
      
      # NOTE:  In this combined dataframe, data are repeated four times, once for each of
      # NOTE:  four concecutive dates. We subset by grabbing the data associated with the
      # NOTE:  datestamp of interest.
      
      bluesky_eventsList[[datestamp]] <- df[df$datestamp == datestamp,]
    
    }
    
  }
  
  # Now combine all event-days into a single dataframe
  bluesky_events <- dplyr::bind_rows(bluesky_eventsList)
  
  # Save output as CSV to share with other folks
  save(bluesky_events, file="localData/bluesky_events.RData")
  
} 
