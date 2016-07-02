#!/usr/bin/env Rscript

defaultDataDir <- '~/Data/WRCC'
defaultOutputDir <- '~/Data/WRCC'
defaultLogLevel <- futile.logger::INFO 
defaultTranscript <- 'WRCC_TRANSCRIPT.txt'

# Set up OptionParser
option_list <- list(
  optparse::make_option(c("--data"), default=defaultOutputDir, help="Directory where raw data files are located [default \"%default\"]"),
  optparse::make_option(c("--output"), default=defaultOutputDir, help="Directory where data files and transcript will be written [default \"%default\"]"),
  optparse::make_option(c("--log"), default=defaultLogLevel, help="The level to log at [default \"%default\"]"),
  optparse::make_option(c("--logpath"), default=defaultTranscript, help="File name for the transcript log [default \"%default\"]")
)

# Parse arguments
opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list))

silence <- PWFSLSmoke::wrcc_buildLibrary(dataDir=opt$data, outputDir=opt$output, logLevel=opt$log,
                                         transcript=opt$logpath)