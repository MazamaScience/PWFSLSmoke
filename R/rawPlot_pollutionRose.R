# NOTE: this is a placeholder for now.  Basic concept is captured, and will develop further at a later date.

# IDEAS
# Subset data by time?
# Move around title, labels, etc.
# change colors?

# NOTE: Can use this function to plot various parameters against wind direction
# NOTE: As an interesting check on the binning by wind direction, run the function below with parameter="windDir".  It
# NOTE: is interesting to note that the boundary bins do not fully capture; so the direction bounds must be offset!

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

rawPlot_pollutionRose <- function(raw, parameter="pm25", normalise=TRUE, ...) {
  
  openair::pollutionRose(raw, ws="windSpeed", wd="windDir",pollutant=parameter, normalise=normalise, angle=15, ...)
  # NOTE: unclear why we need to pass in a wind speed argument since not used in pollution rose...?!?

}

#as an alternative, the following may be useful...perhaps as small multiples by day?
#polarPlot(ebam_airsis,pollutant="pm25",x="windSpeed",wd="windDir")

#here's another type of openair plot that may prove handy, or at least inspire a similar plot
summaryPlot(ebam_airsis[c("datetime","pm25","windSpeed","windDir")],period = "months")

percentileRose(esam_wrcc,pollutant = "pm25",wd="windDir")

#polarAnnulus(ebam_airsis,pollutant="pm25") #can't get this one to work, but looks cool

names(ebam_airsis)
ebam_airsis$ws <- NULL
