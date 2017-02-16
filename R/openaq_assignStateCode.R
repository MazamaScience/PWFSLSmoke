#' @keywords OpenAQ
#' @export
#' @title Assign State Codes to the Available Longitudes and Latitudes
#' @param df a dataframe with longitude and latitude columns
#' @description Uses function getStateCode() from MazamaSpatialUtils package 
#' to assign state codes to the longitudes and latitudes in the dataframe.
#' @return Input dataframe with additional columns: \code{stateCode}.
#' @examples
#' \dontrun{
#' df <- openaq_downloadData(startdate = 20160901, days=2)
#' df <- openaq_assignStateCode(df)
#' }

# ----- Assign stateCode ----------------------------------------------------

# NOTE:  Normally we would assign stateCode inside createMetaDataframe.
# NOTE:  However, we ran into problems with non-unique combinations of
# NOTE:  location-city-country. I belive Rochester Maine/Minnesota was the problem.
# NOTE:  So we need to assign the stateCode here. This is tricky becuase 
# NOTE:  the spatial assignment is imperfect with some monitors close to borders
# NOTE:  being assigned to the wrong country or even state.
# NOTE:  These mistakes will need to be corrected on a per-monitor basis.

openaq_assignStateCode <- function(df) {

  # create a column as the combination of latitude and longitude for later use of left_join
  # which will be discarded afterwards
  df$latlon <- paste(df$latitude, df$longitude, sep=',')
  
  # pull out unique combinations of latitudes and longitudes for faster processing of getStateCode
  latlonUnique <- unique(df$latlon)
  latlonStateCode <- stringr::str_split_fixed(latlonUnique, ',', 2)
  latlonStateCode <- data.frame( apply(latlonStateCode, 2, as.numeric) )

  # get state codes for these unique combinations of latitudes and logitudes
  latlonStateCode$stateCode <- suppressMessages(
    getStateCode( latlonStateCode[,2], latlonStateCode[,1], useBuffering=TRUE ) )
  latlonStateCode$latlon <- latlonUnique

  # append stateCode column to the data frame
  df <- dplyr::left_join(df, latlonStateCode, by="latlon")

  # get rid of extra columns
  df$latlon <- NULL
  df$X1 <- NULL
  df$X2 <- NULL

  df$stateCode[which(df$stateCode=='')] <- NA
  
  ## NOTE: This is for further checking if OpenAQ adds more monitors that are located at ambiguous places
  # us <- df[which(df$country=='US'),]
  # ca <- df[which(df$country=='CA'),]
  # 
  # data(maps::state.fips)
  # usStateCodes <- as.character( unique(state.fips$abb) )
  # usStateCodes <- c(usStateCodes, c("AK", "HI"))
  # 
  # checkUSStateCodes <- unique( us$stateCode[! us$stateCode %in% usStateCodes] )
  # 
  # # Canadian state code NF has been changed to NL
  # caStateCodes <- c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT", "NF")
  #
  # checkCAStateCodes <- unique( ca$stateCode[! ca$stateCode %in% caStateCodes])
  
  # correct non-US state codes for US only  
  df$stateCode[which(df$stateCode == 'TM' & df$country == 'US')] <- 'TX'
  df$stateCode[which(df$stateCode == 'CH' & df$country == 'US')] <- 'TX'
  df$stateCode[which(df$stateCode == 'BC' & df$country == 'US')] <- 'ID'
 
  return(df)
}
