% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raw_getHighlightDates.R
\name{raw_getHighlightDates}
\alias{raw_getHighlightDates}
\title{Return Day Stamps for Values Above a Threshold}
\usage{
raw_getHighlightDates(
  df,
  dataVar,
  tzone = NULL,
  highlightRange = c(1e+12, Inf)
)
}
\arguments{
\item{df}{dataframe with \code{datetime} column in UTC}

\item{dataVar}{variable to be evaluated}

\item{tzone}{timezone where data were collected}

\item{highlightRange}{range of values of to be highlighted}
}
\description{
Returna list of dates in YYYYMMDD format where
the dataVar is within \code{highlightRange}.
}
\examples{
\dontrun{
# Fail gracefully if any resources are not available
try({

raw <- airsis_createRawDataframe(startdate = 20160901, provider = 'USFS',unitID = '1033')
raw <- raw_enhance(raw)
highlightRange <- c(50,Inf)
dataVar <- 'pm25'
tzone <- "America/Los_Angeles"
highlightDates <- raw_getHighlightDates(raw,dataVar,tzone,highlightRange)
rawPlot_timeOfDaySpaghetti(df=raw,highlightDates = highlightDates)

}, silent = FALSE)
}
}
