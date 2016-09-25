###############################################################################
# Create merged dataframe from AIRSIS dump file
#
###############################################################################

library(PWFSLSmoke)

# Set up MazamaSpatialUtils for metadata creation
library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

# Set up logging and log to the console all DEBUG and higher logging statements
logger.setup()
logger.setLevel(DEBUG)

USFS_ebam_file <- '~/Data/monitors/USFS_ebam.csv'

fileString <- readr::read_file(USFS_ebam_file)

df <- airsisDump_parseData(fileString)

# At this point df has multiple monitors in it.

# TODO:  Loop through df$Alias and run each chunk through further data processing



