# NOTE: this is a placeholder for now.  Basic concept is captured, and will develop further at a later date.

# IDEAS
# Subset by time?
# Move around title, labels, etc.
# change colors?

# use the following to set up some dummy raw_enhanced data to play with
if (FALSE) {
  
  library(PWFSLSmoke)
  library(openair)
  setwd("~/Projects/PWFSLSmoke/")
  
  load("localData/airsis_rawList.RData")
  source('~/Projects/PWFSLSmoke/R/raw_enhanced.R', echo=FALSE)
  
  raw <-   raw <- airsis_rawList$Plain; rawSource <- "AIRSIS" #EBAM AIRSIS
  ebam_airsis <- raw_enhanced(raw, rawSource = rawSource)
  
  raw <- airsis_rawList$Naches; rawSource <- "AIRSIS" #ESAM AIRSIS
  esam_airsis <- raw_enhanced(raw, rawSource = rawSource)
  
  raw <- airsis_rawList$Usk; rawSource <- "WRCC" #ESAM WRCC
  esam_wrcc <- raw_enhanced(raw, rawSource = rawSource)
  
  rm(raw)
  rm(rawSource)
  
}

rawPlot_windRose <- function(raw, ...) {
  
  openair::windRose(raw,ws="windSpeed",wd="windDir",angle=15, ...)

}

