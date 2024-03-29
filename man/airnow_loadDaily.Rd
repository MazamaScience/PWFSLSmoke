% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/airnow_loadDaily.R
\name{airnow_loadDaily}
\alias{airnow_loadDaily}
\title{Load recent AirNow monitoring data}
\usage{
airnow_loadDaily(
  parameter = "PM2.5",
  baseUrl = "https://haze.airfire.org/monitoring/latest/RData",
  dataDir = NULL
)
}
\arguments{
\item{parameter}{Parameter of interest.}

\item{baseUrl}{Base URL for 'daily' AirNow data files.}

\item{dataDir}{Local directory containing 'daily' data files.}
}
\value{
A \emph{ws_monitor} object with AirNow data.
}
\description{
Loads pre-generated .RData files containing recent
AirNow data.

If \code{dataDir} is defined, data will be loaded from this local
dirctory. Otherwise, data will be loaded from the monitoring data repository
maintained by PWFSL.

The daily files loaded by this function are updated once a day, shortly
after midnight and contain data for the previous 45 days.

For the most recent data, use \code{airnow_loadLatest()}.

For data extended more than 45 days into the past, use \code{airnow_loadAnnual()}.

AirNow parameters include the following:
\enumerate{
\item{PM2.5}
}

Available AirNow RData and associated log files can be seen at:
\href{https://haze.airfire.org/monitoring/AirNow/RData/latest/}{https://haze.airfire.org/monitoring/AirNow/RData/latest/}
}
\examples{
\dontrun{
# Fail gracefully if any resources are not available
try({

airnow_loadDaily() \%>\%
  monitor_subset(stateCodes=CONUS) \%>\%
  monitor_map()

}, silent = FALSE)
}
}
\seealso{
\code{\link{airnow_loadAnnual}}

\code{\link{airnow_loadLatest}}
}
\keyword{AirNow}
