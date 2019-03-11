#' @export
#' @title Calculate distances between points
#' @param targetLon longitude (decimal degrees) of the point from which
#' distances are calculated
#' @param targetLat latitude (decimal degrees) of the point from which
#' distances are calculated
#' @param longitude vector of longitudes for which a distance is calculated
#' @param latitude vector of latitudes for which a distance is calculated
#' @description This function uses the Haversine forumula for calculating great
#' circle distances between points. This formula is purpoted to work better
#' than the spherical law of cosines for very short distances.
#' @references \url{https://www.r-bloggers.com/great-circle-distance-calculations-in-r/}
#' @return Vector of distances in km.
#' @examples
#' # Seattle to Portland airports
#' SEA_lon <- -122.3088
#' SEA_lat <- 47.4502
#' PDX_lon <- -122.5951
#' PDX_lat <- 45.5898
#' distance(SEA_lon, SEA_lat, PDX_lon, PDX_lat)

distance <- function(
  targetLon,
  targetLat,
  longitude,
  latitude
) {

  if (length(longitude) != length(latitude)) {
    stop(paste0('longitude [',
                length(longitude),
                '] and latitude [',
                length(latitude),
                '] are requried to be of the same length'))
  }

  targetLocation <- cbind(targetLon, targetLat)
  location <- cbind(longitude, latitude)
  distance <- geosphere::distHaversine(targetLocation, location) / 1000

  return(distance)

}
