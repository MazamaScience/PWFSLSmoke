#' @keywords AirNow
#' @import dplyr
#' @import MazamaSpatialUtils
#' @export
#' @title Create Sites Metadata Dataframe
#' @param sites dataframe obtained from airnow_downloadSites()
#' @param parameter parameter name used to subset sites
#' @param verbose logical flag to generate verbose output
#' @description The \pkg{PWFSLSmoke} package's data model for monitoring
#' data is a list with \code{data} and \code{meta} dataframes. The \code{meta}
#' dataframe has metadata associated with monitoring site locations for a specific
#' parameter and must contain at least the following columns:
#' \itemize{
#'   \item{monitorID -- per deployment unique ID}
#'   \item{longitude -- decimal degrees E}
#'   \item{latitude -- decimal degrees N}
#'   \item{elevation -- height above sea level in meters}
#'   \item{timezone -- olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#' }
#' 
#' The \code{meta} dataframe will have rownames matching \code{monitorID}.
#' 
#' The airnow_downloadSites() function takes a dataframe obtained from AirNowTech's
#' \code{monitoring_site_locations.dat} file and performs the following cleanup:
#' \itemize{
#'   \item{remove duplicate entries associated with different parameters}
#'   \item{convert incorrect values to \code{NA} e.g. lon=0 & lat=0}
#'   \item{add timezone information}
#' }
#' 
#' Metadata subsets can be generated for sites monitoring specific pollutants
#' by specifying, for example \code{parameter='O3'}.
#' 
#' @return Dataframe of site location metadata appropriate for use in the
#' creation of a \code{ws_monitor} object.
#' @seealso \link{airnow_downloadSites}
#' @examples
#' \dontrun{
#' sites <- airnow_downloadSites('USER','PASS')
#' meta <- airnow_createMetaDataframe(sites, parameter='PM2.5')
#' }

airnow_createMetaDataframe <- function(sites, parameter='PM2.5', verbose=FALSE) {

  # Subset metadata to only include sites measuring 'parameter'
  if (parameter %in% levels(sites$parameterName)) {
    sites <- sites[sites$parameterName == parameter,]
  } else {
    parameterNames <- paste0(levels(sites$parameterName),collapse=',')
    err_msg <- paste0('parameter "',parameter,'" not found in "', parameterNames,'".\nUsing all parameters.')
    stop(err_msg)
  }
  
  # Ignore records that differ only by 'parameterName'
  columnNames <- setdiff(names(sites),c('parameterName'))
  meta <- sites[!duplicated(sites),columnNames]
  
  # Handle records with bad location information
  badLocationMask <- (meta$longitude == 0) & (meta$latitude == 0)
  meta$longitude[badLocationMask] <- NA
  meta$latitude[badLocationMask] <- NA
  meta$elevation[badLocationMask] <- NA

  if (verbose) cat(paste0(sum(badLocationMask),' locations found with lon/lat = 0.\n'))
  
  if (verbose) cat(paste0('Determining timezones for ',nrow(meta),' locations ...\n'))

  # Add timezone
  # TODO:  Remove supressWarnings() when getTimezone() stops generating them.
  meta$timezone <- suppressWarnings( MazamaSpatialUtils::getTimezone(meta$longitude, meta$latitude, useBuffering=TRUE) )

  # TODO:  Could check/correct/add state codes  
  
  # TODO:  Could add HUC12

  # Add monitorID column
  # NOTE:  AirNow appears to do a good job of creating per dployment unique IDs
  meta$monitorID <- meta$AQSID
  
  # Assign rownames
  rownames(meta) <- meta$monitorID
  
  return(meta)
}

