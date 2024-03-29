% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/airsis_downloadData.R
\name{airsis_downloadData}
\alias{airsis_downloadData}
\title{Download AIRSIS data}
\usage{
airsis_downloadData(
  startdate = strftime(lubridate::now(tzone = "UTC"), "\%Y0101", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"), "\%Y\%m\%d", tz = "UTC"),
  provider = "USFS",
  unitID = NULL,
  baseUrl = "http://xxxx.airsis.com/vision/common/CSVExport.aspx?"
)
}
\arguments{
\item{startdate}{desired start date (integer or character representing YYYYMMDD[HH])}

\item{enddate}{desired end date (integer or character representing YYYYMMDD[HH])}

\item{provider}{identifier used to modify baseURL \code{['APCD'|'USFS']}}

\item{unitID}{unit identifier}

\item{baseUrl}{base URL for data queries}
}
\value{
String containing AIRSIS output.
}
\description{
Request data from a particular station for the desired time period.
Data are returned as a single character string containing the AIRIS output.
}
\examples{
\dontrun{
# Fail gracefully if any resources are not available
try({

fileString <- airsis_downloadData( 20150701, 20151231, provider='USFS', unitID='1026')
df <- airsis_parseData(fileString)

}, silent = FALSE)
}
}
\references{
\href{http://usfs.airsis.com}{Interagency Real Time Smoke Monitoring}
}
\keyword{AIRSIS}
