#' @keywords ws_monitor
#' @export
#' @title Write Current Monitor Data to GeoJSON
#' @param ws_monitor \emph{ws_monitor} object
#' @param filename filename where geojson file will be saved
#' @param properties optional character vector of properties to include for each monitor in geoJSON. 
#' If NULL all are included. May include any ws_monitor metadata and additional columns generated in \code{\link{monitor_currentData}}
#' @param propertyNames optional character vector supplying custom names for properties in geoJSON. If NULL or different length 
#' than \code{properties} defaults will be used.
#' @return invisibly returns geoJSON string. 
#' @description Writes a geoJSON file containing current monitor data. For details on what is included, see 
#' \code{\link{monitor_currentData}}
#' @examples
#' \dontrun{
#' wa <- loadLatest() %>% monitor_subset(stateCodes = 'WA')
#' wa_current_geojson <- monitor_writeCurrentGeoJSON(wa, 'wa_monitors.geojson')
#' wa_current_list <- jsonlite::fromJSON(wa_current)
#' wa_spdf <- rgdal::readOGR(dsn='wa_monitors.geojson', layer='currentTbl')
#' wa_spdf <- rgdal::readOGR(dsn='wa_monitors.geojson', layer="OGRGeoJSON")
#' plot(wa_spdf)
#' }

monitor_writeCurrentGeoJSON <- function(ws_monitor,
                                        filename,
                                        properties = NULL,
                                        propertyNames = NULL) {
  
  # Sanity check
  if ( monitor_isEmpty(ws_monitor) ) stop("ws_monitor object contains zero monitors")
  
  result <- try(
    currentTbl <- monitor_currentData(ws_monitor)
  )
  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    stop(paste0("error getting current data: ", err_msg))
  }
  
  
  
  # NOTE:  For highest values to be plotted last on Leaflet Maps we want monitors with NA's
  # NOTE:  to come first and then everything in increasing order.
  myOrder <- rev( order(currentTbl$PM2.5_yesterday, decreasing=TRUE) )
  currentTbl <- currentTbl[myOrder,]
  
  # ----- Get coords from meta and create SPDF --------------------------------
  
  # NOTE:  To save space we always round to 5 decimal places (~1 meter)
  coords <- cbind( round(currentTbl$longitude,5), round(currentTbl$latitude,5) ) 
  
  # Use monitorID as siteName if siteName is missing
  currentTbl$siteName <- ifelse(is.na(currentTbl$siteName), currentTbl$monitorID, currentTbl$siteName)
  
  # Subset meta to desired properties
  if (!is.null(properties)) {
    currentTbl <- currentTbl[,properties]
    if (!is.null(propertyNames) & length(propertyNames) == length(properties))
      result <- try(names(currentTbl) <- propertyNames)
      if ( "try-error" %in% class(result) ) {
        err_msg <- geterrmessage()
        warning(paste0("could not change propertyNames - using defaults: ", err_msg))
      }
  }

  
  
  # Create the SpatialPointsDataFrame
  # NOTE:  We use as.data.frame because use of dplyr:: functions adds the "tbl_df" class
  sites <- sp::SpatialPointsDataFrame(coords=coords,
                                      proj4string=sp::CRS("+proj=longlat +ellps=WGS84"),
                                      data=as.data.frame(currentTbl))

  
  # Convert to geoJSON using 'rgdal' package
  # NOTE:  rgdal::writeOGR creates .geojson files where numeric properties are not quoted.
  # NOTE:  rgdal::writeOGR complains if filename already exists.
  # NOTE:  rgdal::writeOGR does not automatically append '.geojson'
  # filename <- paste0(opt$outputDir,'/',opt$dataSource,'_PM2.5_latest10.geojson')
  # 
  # logger.info('Using rgdal::writeOGR to create geojson file %s', filename)
  
  rgdal::writeOGR(sites, dsn=filename, layer='currentTbl', driver='GeoJSON')
    
  # Add top level metadata as Foreign Members:  https://tools.ietf.org/html/rfc7946#section-6.1
  # and rewrite the geojson file
  geojsonList <- as.list(jsonlite::fromJSON(filename,
                                            simplifyVector=TRUE,
                                            simplifyDataFrame=FALSE,
                                            simplifyMatrix=FALSE,
                                            flatten=FALSE))
  
  geojsonList$lastUpdateTimestampUTC <- strftime(lubridate::now('UTC'), "%Y-%m-%d %H:%M:%S %z", tz='UTC')
  geojsonText <- jsonlite::toJSON(geojsonList, auto_unbox=TRUE, null='null', na='null')
  cat(geojsonText, file=filename)
  
  return(invisible(geojsonText))
  
}
