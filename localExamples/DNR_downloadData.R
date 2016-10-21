###############################################################################
# Obtain and plot monitoring data for the DNR pilot project
#
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
#
###############################################################################

library(PWFSLSmoke)

# Set up MazamaSpatialUtils for metadata creation
library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

# Set up logging and log to the console all DEBUG and higher logging statements
logger.setup()
logger.setLevel(INFO)

# "Raw" data ------------------------------------------------------------------

rawList <- list()
rawList[['FishHatchery']] <- airsis_createRawDataframe('USFS', '1012', 20160901, 20161015)  # Kettle Falls-Sherman Pass Scenic Byway
rawList[['KennedMeadows']] <- airsis_createRawDataframe('APCD', '1013', 20160901, 20161015) # Manson-Totem Pole Road
rawList[['Naches']] <- airsis_createRawDataframe('EPA', '1014', 20160901, 20161015)         # Naches-U.S. 12
rawList[['Liberty']] <- airsis_createRawDataframe('USFS', '1031', 20160901, 20161015)       # Cle Elum-Liberty Road
###rawList[['Usk']] <- airsis_createRawDataframe('USFS', '1032', 20160901, 20161015)        # NO DATA SURVIVES QC!
rawList[['Plain']] <- airsis_createRawDataframe('USFS', '1033', 20160901, 20161015)         # Leavenworth-Beaver Valley Road
rawList[['Curlew']] <- airsis_createRawDataframe('USFS', '1034', 20160901, 20161015)        # Curlew-Customs Road
rawList[['Nile']] <- airsis_createRawDataframe('USFS', '1049', 20160901, 20161015)          # Naches-Washington 410
rawList[['KettleFalls']] <- airsis_createRawDataframe('USFS', '1050', 20160901, 20161015)   # Kettle Falls-West 11th Avenue

save(rawList, file='DNR_rawList.RData')

# "monitor" objects -----------------------------------------------------------

monitorList <- list()
monitorList[['FishHatchery']] <- airsis_createMonitorObject('USFS', '1012', 20160901, 20161015)  # Kettle Falls-Sherman Pass Scenic Byway
monitorList[['KennedMeadows']] <- airsis_createMonitorObject('APCD', '1013', 20160901, 20161015) # Manson-Totem Pole Road
monitorList[['Naches']] <- airsis_createMonitorObject('EPA', '1014', 20160901, 20161015)         # Naches-U.S. 12
monitorList[['Liberty']] <- airsis_createMonitorObject('USFS', '1031', 20160901, 20161015)       # Cle Elum-Liberty Road
###monitorList[['Usk']] <- airsis_createMonitorObject('USFS', '1032', 20160901, 20161015)        # NO DATA SURVIVES QC!
monitorList[['Plain']] <- airsis_createMonitorObject('USFS', '1033', 20160901, 20161015)         # Leavenworth-Beaver Valley Road
monitorList[['Curlew']] <- airsis_createMonitorObject('USFS', '1034', 20160901, 20161015)        # Curlew-Customs Road
monitorList[['Nile']] <- airsis_createMonitorObject('USFS', '1049', 20160901, 20161015)          # Naches-Washington 410
monitorList[['KettleFalls']] <- airsis_createMonitorObject('USFS', '1050', 20160901, 20161015)   # Kettle Falls-West 11th Avenue

save(monitorList, file='DNR_monitorList.RData')

