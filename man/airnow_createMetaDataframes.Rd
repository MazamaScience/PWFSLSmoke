% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/airnow_createMetaDataframes.R
\name{airnow_createMetaDataframes}
\alias{airnow_createMetaDataframes}
\title{Create dataframes of AirNow site location metadata}
\usage{
airnow_createMetaDataframes(
  parameters = NULL,
  pwfslDataIngestSource = "AIRNOW",
  addGoogleMeta = TRUE
)
}
\arguments{
\item{parameters}{vector of names of desired pollutants or NULL for all pollutants}

\item{pwfslDataIngestSource}{identifier for the source of monitoring data, e.g. \code{'AIRNOW'}}

\item{addGoogleMeta}{logicial specifying wheter to use Google elevation and reverse geocoding services}
}
\value{
List of 'meta' dataframes with site metadata for unique parameters (e.g: "PM2.5", "NOX").
}
\description{
The \code{airnow_createMetaDataframes()} function uses the \code{airnow_downloadSites()} function
to download site metadata from AirNow and restructures that data into a format that is compatible
with the PWFSLSmoke package \emph{ws_monitor} data model.

The \code{meta} dataframe in the \emph{ws_monitor} data model has metadata associated with monitoring
site locations for a specific parameter and must contain at least the following columns:
\itemize{
  \item{monitorID -- per deployment unique ID}
  \item{longitude -- decimal degrees E}
  \item{latitude -- decimal degrees N}
  \item{elevation -- height above sea level in meters}
  \item{timezone -- olson timezone}
  \item{countryCode -- ISO 3166-1 alpha-2}
  \item{stateCode -- ISO 3166-2 alpha-2}
}

The \code{meta} dataframe will have rownames matching \code{monitorID}.

This function takes a dataframe obtained from AirNowTech's
\code{monitoring_site_locations.dat} file, splits it up into separate dataframes,
one for each parameter, and performs the following cleanup:
\itemize{
  \item{convert incorrect values to \code{NA} e.g. longitude=0 & latitude=0}
  \item{add timezone information}
}

Parameters included in AirNow data include at least the following list:
\enumerate{
\item{BARPR}
\item{BC}
\item{CO}
\item{NO}
\item{NO2}
\item{NO2Y}
\item{NO2X}
\item{NOX}
\item{NOOY}
\item{OC}
\item{OZONE}
\item{PM10}
\item{PM2.5}
\item{PRECIP}
\item{RHUM}
\item{SO2}
\item{SRAD}
\item{TEMP}
\item{UV-AETH}
\item{WD}
\item{WS}
}

Setting \code{parameters=NULL} will generate a separate dataframe for each of the above parameters.
}
\examples{
\dontrun{
# Fail gracefully if any resources are not available
try({

metaList <- airnow_createMetaDataframes(parameters = "PM2.5")

}, silent = FALSE)
}
}
\seealso{
\link{airnow_downloadSites}
}
\keyword{AirNow}
