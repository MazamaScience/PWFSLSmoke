#!/usr/bin/env Rscript

defaultProvider <- 'USFS'
defaultUnitIDs <- NULL 
defaultYears <- '2000:2015'
defaultLogLevel <- futile.logger::INFO 
defaultTranscript <- 'AIRSIS_TRANSCRIPT.txt'
defaultOutputDir <- getwd()
defaultBaseUrl <- "http://xxxx.airsis.com/vision/common/CSVExport.aspx?" 


# Set up OptionParser
option_list <- list(
  optparse::make_option(c("--provider"), default=defaultProvider, help="Identifier used to modify baseURL [default \"%default\"]"),
  optparse::make_option(c("--unitIDs"), default=defaultUnitIDs, help="Unit identifiers, seperated by commas ('4,16,64') [default \"%default\"]"),
  optparse::make_option(c("--years"), default=defaultYears, help="Years to download data from, seperated by a colon ('2000:2015') [default \"%default\"]"),
  optparse::make_option(c("--log"), default=defaultLogLevel, help="The level to log at [default \"%default\"]"),
  optparse::make_option(c("--logpath"), default=defaultTranscript, help="File name for the transcript log [default \"%default\"]"),
  optparse::make_option(c("--output"), default=defaultOutputDir, help="Directory to save data and transcripts to [default \"%default\"]"),
  optparse::make_option(c("--url"), default=defaultBaseUrl, help="base URL for data queries [default \"%default\"]")
)

# Parse arguments
opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list))
# Parse arguments
if (length(opt$unitIDs) == 1) {
  opt$unitIDs <- as.numeric(unlist(stringr::str_split(opt$unitIDs, ',')))
}
years <- unlist(stringr::str_split(opt$years, ':'))
opt$startYear <- as.numeric(years[1])
opt$endYear <- as.numeric(years[2])

silence <- PWFSLSmoke::airsis_buildLibrary(provider=opt$provider, unitIDs=opt$unitIDs, 
                                years=seq(opt$startYear, opt$endYear), logLevel=opt$log,
                                transcript=opt$transcript, outputDir=opt$output, 
                                baseUrl = opt$url)