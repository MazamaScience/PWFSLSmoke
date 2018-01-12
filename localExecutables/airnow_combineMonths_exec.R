#!/usr/bin/env Rscript

# This script will combine airnow monthly data into a yearly data file and save it as an RData file
# 
# # Example
# airsis_getYearlyData_exec.R --startdate=20150101 --enddate=2016010223 --outputDir="~/Data/airsis" --fileName="airsis_2015"

VERSION <- "0.0.1"

suppressPackageStartupMessages({
  library(methods)
  library(optparse)
  library(PWFSLSmoke)
})

##########################################

# For debugging
if ( FALSE ) {
  
  opt <- list(parameterName = "PM2.5",
              year = "2017",
              inputDir = "/Users/helen/Data/airnow/2017/PM25",
              outputDir = "/Users/helen/Data/airnow/2017/PM25",
              logDir = "/Users/helen/Data/airnow/2017/PM25")  
  
}

combineAirnowMonths <- function(opt) {
  
  firstMonth <- TRUE
  for (month in 1:12){
    
    # Build the filepath
    fileName <- paste0("airnow_", opt$parameterName, "_", opt$year, "_", stringr::str_pad(month, 2, pad = 0), ".RData")
    filePath <- file.path(opt$inputDir, fileName)
    logger.debug(paste0("loading ", filePath))
    print(paste0("loading ", filePath))
    if ( !file.exists(filePath) ) {
      logger.debug(paste0("File ", filePath, " does not exist."))
      print(paste0("File ", filePath, " does not exist."))
    } else {
      
      # Load the correct ws_monitor object
      airnowMonth <- load(filePath)
      airnowMonth <- eval(parse(text = airnowMonth))
      
      if ( firstMonth ) {
        airnowYear <- airnowMonth
        firstMonth <- FALSE
      } else {
        # NOTE:  The way we grow the ws_monitor object is an example of what NOT to do in R but
        # NOTE:  we are limited by the fact that monitor_join() can only join 2 monitors.
        print("combining months...")
        airnowYear <- monitor_combine(list(airnowYear, airnowMonth))
      }
      logger.debug(paste0("successfully combined month ", month))
      print(paste0("successfully combined month ", month))
    }
  }
  
  # Set the name of airnowYear to fileName
  if (is.null(opt$fileName)) {
    fileName <- paste0('airnow_', opt$year)
  } else {
    fileName <- opt$fileName
  }
  
  assign(fileName, airnowYear)
  filePath <- file.path(opt$outputDir, fileName)
  
  # Save the RData file
  save(list = fileName, file = filePath)
  
}

################################################################################
# Main program

# ----- Parse command line options ---------------------------------------------

# Set up option parser
option_list <- list(
  make_option(c("-n","--parameterName"),default='PM2.5', help="parameter name"),
  make_option(c("-y","--year"), default=2016, help="Specify a single year to combine months for"),
  make_option(c("-d","--inputDir"), default=getwd(), help="Directory containing airnow monthly .RData files [default=\"%default\"]"),
  make_option(c("-o","--outputDir"), default=getwd(), help="Output directory for generated .RData files [default=\"%default\"]"),
  make_option(c("-f","--fileName"), default="airnow_[year]", help="Name for the resultant .RData file"),
  make_option(c("-l","--logDir"), default=getwd(), help="Output directory for generated .log file [default=\"%default\"]"),
  # make_option(c("-s","--spatialDataDir"), default='~/Data/Spatial', 
  # help="Directory containing spatial datasets used by MazamaSpatialUtils [default=\"%default\"]"),
  make_option(c("-V","--version"), action="store_true", default=FALSE, help="Print out version number [default=\"%default\"]")
)

# Parse arguments
opt <- parse_args(OptionParser(option_list=option_list))

# Print out version and quit
if ( opt$version ) {
  cat(paste0('createCSV_exec.R ',VERSION,'\n'))
  quit()
}

# Sanity checks
if ( !dir.exists(opt$inputDir) ) stop(paste0("inputDir not found:  ",opt$downloadDir))
if ( !dir.exists(opt$outputDir) ) stop(paste0("outputDir not found:  ",opt$outputDir))
if ( !dir.exists(opt$logDir) ) stop(paste0("logDir not found:  ",opt$logDir))
# 
# # Add year subdirectories
# opt$downloadDir <- file.path(opt$downloadDir,opt$year)
# opt$outputDir <- file.path(opt$outputDir,opt$year)
# opt$logDir <- file.path(opt$logDir,opt$year)
# 
# # Make sure the year subdirectories exist
# dir.create(opt$downloadDir, showWarnings=FALSE)
# dir.create(opt$outputDir, showWarnings=FALSE)
# dir.create(opt$logDir, showWarnings=FALSE)

# Assign log file names
debugLog <- file.path(opt$logDir, paste0('airnow_combineMonths', '_DEBUG.log'))
infoLog  <- file.path(opt$logDir, paste0('airnow_combineMonths', '_INFO.log'))
errorLog <- file.path(opt$logDir, paste0('airnow_combineMonths', '_ERROR.log'))

# Set up logging
logger.setup(debugLog=debugLog, infoLog=infoLog, errorLog=errorLog)

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error


# ----- Save airnow ws_monitor object as a RData file ------

result <- try( combineAirnowMonths(opt) )

if ( "try-error" %in% class(result) ) {
  msg <- paste("Error combining airnow months: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info("Completed successfully!")
}




