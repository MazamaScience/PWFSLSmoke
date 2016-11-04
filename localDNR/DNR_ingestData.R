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

# This DNR_ingestData.R script ingests and cleans up previously downloaded data.

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

# ----- Create a dictionary of monitor names and IDs --------------------------

monitorDict <- list(
  # AIRSIS monitors
  FishHatchery="Fish.Hatchery..1012.__001",
  Manson="Manson.WA.apcd1013__001",
  Naches="CRO...Naches..WA__001",
  Liberty="Liberty.WA..1031.__001",
  Plain="Plain.WA..1033.__001",
  Curlew="Curlew.WA..1034.__001",
  Nile="Nile.Pinecliff.1049__001",
  KettleFalls="Kettle.Falls..1050.__001",
  # AirNow monitors
  Twisp="530470009",
  Ellensburg="530370002",
  Leavenworth="530070010",
  Toppenish="530770015",
  Winthrop="530470010",
  Yakima="530770009",
  WhiteSwan="530770016",
  Omak="530470013",
  Wenatchee="530070011",
  Chelan="530070007"
  )

# ----- Load data created by DNR_downloadData.R -------------------------------

load('localData/airnow_monitors.RData')
load('localData/airsis_rawList.RData')
load('localData/airsis_monitorList.RData')
load('localData/bluesky_eventsList.RData')

# Assign negative PM2.5 to zero for airnow_monitors, airsis_monitorList
airnow_monitors <- monitor_replaceData(airnow_monitors, data < 0, 0)

for (i in 1:length(airsis_monitorList)) {
  airsis_monitorList[[i]] <- monitor_replaceData(airsis_monitorList[[i]], data < 0, 0)
}

# Combine airsis_monitorList and bluesky_eventsList for convenience
airsis_monitors <- monitor_combine(airsis_monitorList)

bluesky_events <- dplyr::bind_rows(bluesky_eventsList)

# ----- Load hand-edited Excel Spreadsheet from Janice Peterson ---------------

# > names(janice_SMA)
# [1] "Permit"                               "Region"                               "Land.Owner"                          
# [4] "Unit"                                 "Legal"                                "Acres"                               
# [7] "Proposed.Tons"                        "Accomplished.Tons"                    "Approval"                            
# [10] "Acres.Burned"                         "Accomplished.Tons.from.FS.fireportal" "Ignition.time"                       
# [13] "Near.AQ.monitor."                     "Comments"                             "datestamp"                           
# [16] "Latitude"                             "Longitude"                            "DNR_Pilot.24.Hr.Advance"             
# [19] "datetime"                            

col_types <- c('text','text','text',
               'text','text','numeric',
               'numeric','numeric','text',
               'numeric','numeric','text',
               'text','text','date',
               'numeric','numeric','text',
               'text')

filepath=paste0(getwd(),'/localData/DNR\ SM\ eastside\ accomplishments\ fall\ 2016.xlsx')
janice_SMA <- readxl::read_excel(filepath, na="NA",
                                 col_types=col_types)

# remove rows with all missing
allMissingMask <- apply( janice_SMA, 1, function(x) { all(is.na(x)) } )
janice_SMA <- janice_SMA[!allMissingMask,]

names(janice_SMA) <- make.names(names(janice_SMA))

# TODO:  Check this:  On 2016-11-03, rows 57 and 58 have bogus values in DNR_Pilot.24.Hr.Advance
# TODO:  Check this:  and need to be removed explicitly

janice_SMA <- janice_SMA[1:56,]

# Convert 'Approval' and 'DNR_Pilot.24.Hr.Advance' to logicals
approval <- stringr::str_replace( stringr::str_replace(janice_SMA$Approval,"Y","T"), "N","F")
pilot <- stringr::str_replace( stringr::str_replace(janice_SMA$DNR_Pilot.24.Hr.Advance,"1","T"), "0","F")
janice_SMA$Approval <- as.logical(approval)
janice_SMA$DNR_Pilot.24.Hr.Advance <- as.logical(pilot)

# Converting time columns to PWFSLSmoke internal standards: remove 'datestamp', 'datetime' should have POSIXct
janice_SMA$datestamp <- NULL
janice_SMA$datetime <- lubridate::ymd_hms(janice_SMA$datetime, tz="UTC")

# Make Ignition.Time a POSIXct in the "America/Los_Angeles" timezone
datestamp <- as.character(lubridate::date(janice_SMA$datetime))
janice_SMA$Ignition.time <- toupper(janice_SMA$Ignition.time)   # convert "na" to "NA"
timestamp <- paste0('0',janice_SMA$Ignition.time)               # guarantee 4 digits
timestamp <- ifelse(stringr::str_detect(timestamp,"NA"), NA, timestamp)
h <- stringr::str_sub(timestamp, -4, -3)
m <- stringr::str_sub(timestamp, -2,-1)
ignition_timestamp <- paste0(datestamp,' ',h,':',m,':00')
ignition_timestamp <- ifelse(stringr::str_detect(ignition_timestamp,"NA"), NA, ignition_timestamp)
janice_SMA$Ignition.time <- lubridate::ymd_hms(ignition_timestamp, tz="America/Los_Angeles")

# Clean up
rm(allMissingMask)
rm(approval)
rm(col_types)
rm(datestamp)
rm(end)
rm(filepath)
rm(h)
rm(i)
rm(ignition_timestamp)
rm(m)
rm(pilot)
rm(start)
rm(timestamp)



