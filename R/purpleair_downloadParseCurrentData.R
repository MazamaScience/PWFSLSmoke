#' @keywords PurpleAir
#' @export
#' @importFrom utils read.table
#' @title Download Current Data from PurpleAir
#' @param baseUrl base URL for current data
#' @return Dataframe of current PurpleAir data.
#' @description The \url{https://www.purpleair.com/json} site provides current particulate
#' particulate monitoring data for PurpleAir monitors.
#' 
#' @examples
#' \dontrun{
#' df <- purpleair_downloadParseCurrentData()
#' }

purpleair_downloadParseCurrentData <- function(baseUrl='https://www.purpleair.com/json') {
  
  # Strip off any final '/'
  baseUrl <- stringr::str_replace(baseUrl,'/$','')
  
  # Placeholder in case things get more complicated
  url <- baseUrl

  # Convert data to a ???
  PAList <- jsonlite::fromJSON(url,
                               simplifyVector=TRUE,
                               simplifyDataFrame=TRUE,
                               simplifyMatrix=TRUE,
                               flatten=FALSE)

  # > names(PAList)
  # [1] "mapVersion"       "baseVersion"      "mapVersionString" "results"  
  
  # TODO:  Opportunity to check versions
  
  # Pull out the dataframe of results
  resultsDF <- PAList$results

  # ----- BEGIN convert results$Stats -----------------------------------------
  
  # NOTE:  Stats for current, 10 min, 30 min, 1 hr, 6 hr, 1 day, 1 week are stored in df$Stats

  # NOTE:  Some Stats are NA and we need to fill in a blank stats array for those
  emptyStatsList <- list(
    v = -999.9,
    v1 = -999.9,
    v2 = -999.9,
    v3 = -999.9,
    v4 = -999.9,
    v5 = -999.9,
    v6 = -999.9,
    pm = -999.9,
    lastModified = -999.9,
    timeSinceModified = -999 # int
  )

  emptyStatsJSON <- jsonlite::toJSON(emptyStatsList, auto_unbox=TRUE, null='null', na='null', pretty=FALSE)

  # Add empty JSON string where needed
  missingStatsMask <- is.na(resultsDF$Stats)
  resultsDF$Stats[missingStatsMask] <- emptyStatsJSON
  
  statsList <- lapply(resultsDF$Stats, function(x) { jsonlite::fromJSON(x) } )
  
  # NOTE:  At this point we have a statsList where every element is a list
  # NOTE:  Some Stats are missing 'pm', 'lastModified' and 'timeSinceModified'
  # NOTE:  but bind_rows() will take care of this by filling in those columns
  # NOTE:  with NA.
  
  statsTbl <- dplyr::bind_rows(statsList)
  
  # Now convert -999.9 and -999 back to NA
  missingMask <- statsTbl <= -999
  statsTbl[missingMask] <- as.numeric(NA)
  
  # ----- END convert results$Stats -------------------------------------------
  
  # Now create a new dataframe using the important columsn from results and stats
  
  # > names(resultsDF)
  # [1] "ID"                               "ParentID"                         "Label"                           
  # [4] "DEVICE_LOCATIONTYPE"              "THINGSPEAK_PRIMARY_ID"            "THINGSPEAK_PRIMARY_ID_READ_KEY"  
  # [7] "THINGSPEAK_SECONDARY_ID"          "THINGSPEAK_SECONDARY_ID_READ_KEY" "Lat"                             
  # [10] "Lon"                              "PM2_5Value"                       "LastSeen"                        
  # [13] "State"                            "Type"                             "Hidden"                          
  # [16] "Flag"                             "isOwner"                          "A_H"                             
  # [19] "temp_f"                           "humidity"                         "pressure"                        
  # [22] "AGE"                              "Stats"                           

  # > names(statsTbl)
  # [1] "v"                 "v1"                "v2"                "v3"                "v4"               
  # [6] "v5"                "v6"                "pm"                "lastModified"      "timeSinceModified"  

  tbl <- dplyr::bind_cols(resultsDF, statsTbl)
  tbl$Stats <- NULL

  return(tbl)
  
}
