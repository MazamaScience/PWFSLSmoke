% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epa_parseData.R
\name{epa_parseData}
\alias{epa_parseData}
\title{Parse EPA data}
\usage{
epa_parseData(zipFile = NULL)
}
\arguments{
\item{zipFile}{absolute path to monitoring data .zip file}
}
\value{
Tibble of EPA data.
}
\description{
This function uncompress previously downloaded air quality .zip files from the EPA and
reads it into a tibble.

Available parameters include:
\enumerate{
\item{Ozone}
\item{SO2}
\item{CO}
\item{NO2}
\item{PM2.5}
\item{PM10}
\item{Wind}
\item{Temperature}
\item{Barometric_Pressure}
\item{RH_and_Dewpoint}
\item{HAPs}
\item{VOCs}
\item{NONOxNOy}
}

Associated parameter codes include:
\enumerate{
\item{44201}{ -- Ozone}
\item{42401}{ -- SO2}
\item{42101}{ -- CO}
\item{42602}{ -- NO2}
\item{88101}{ -- PM2.5}
\item{88502}{ -- PM2.5}
\item{81102}{ -- PM10}
\item{SPEC}{ -- PM2.5}
\item{WIND}{ -- Wind}
\item{TEMP}{ -- Temperature}
\item{PRESS}{ -- Barometric Pressure}
\item{RH_DP}{ -- RH and dewpoint}
\item{HAPS}{ -- HAPs}
\item{VOCS}{ -- VOCs}
\item{NONOxNOy}
}
}
\note{
Unzipped CSV files are almost 100X larger than the compressed .zip files.
CSV files are removed after data are read into a dataframe.
}
\examples{
\dontrun{
# Fail gracefully if any resources are not available
try({

zipFile <- epa_downloadData(2016, "88101", '~/Data/EPA')
tbl <- epa_parseData(zipFile, "PM2.5")

}, silent = FALSE)
}
}
\references{
\href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}

\href{https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_format_3}{file format description}
}
\keyword{EPA}
