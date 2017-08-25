#' @export
#' @title Load Current Status from Airsis Site
#' @description Scrape current monitor status from Airsis website (http://xxxx.airsis.com
#' where xxxx %in% c("usfs", "apcd", "arb2", "epa").
#' @param provider data provider, e.g. usfs, arb2, etc.
#' @param username username for login credentials
#' @param password password for login credentials
#' @return A dataframe with unit ID, alias, location, and latest update time, in UTC.
#' @examples
#' \dontrun{
#' usfs <- airsis_currentStatus(provider="usfs")
#' View(usfs)
#' }

airsis_currentStatus <- function(provider, username, password) {
  
  # TODO: develop code to get response via request, cookies, etc...
  ### response <- httr::GET("http://usfs.airsis.com")  #, authenticate(username, password))
  ### response <- httr::GET("http://usfs.airsis.com/vision/Login.aspx?ReturnUrl=%2fvision", authenticate(username, password))
  
  ### ---- temporary for development/testing purposes ------------------
  # providers <- c("usfs", "apcd", "arb2", "epa")
  # provider <- providers[4]
  response <- readr::read_file(paste0("~/Downloads/", provider, ".htm"))
  ### ------------------------------------------------------------------
  
  provider <- stringr::str_to_lower(provider)
    
  # parse raw html and pull out fields of interest
  currentStatusDoc <- xml2::read_html(response)
  currentStatusNode <- rvest::html_nodes(currentStatusDoc, xpath='//*[@id="DataGrid1"]')
  
  # convert node to table
  currentStatusTable <- rvest::html_table(currentStatusNode, header=TRUE)[[1]]
  
  # get unitIDs from node
  if ( provider=="usfs") {
    linkNode <- "#DataGrid1 td:nth-child(4) a"
  } else if ( provider=="apcd") {
    linkNode <- "td:nth-child(21) a"
  } else if ( provider=="arb2") {
    linkNode <- "td:nth-child(21) a"
  } else if ( provider=="epa") {
    linkNode <- "td:nth-child(20) a"
  } else {
    stop('provider not supported')
  }
  urls <- rvest::html_nodes(currentStatusDoc, linkNode)
  urls <- rvest::html_attr(urls, 'href')
  unitIDs <- stringr::str_replace(urls, stringr::fixed("UnitHistory.aspx?uid="), "")
  
  # get times
  # TODO: verify airsis data is in local time
  times <- currentStatusTable[["Date/Time"]]
  times <- as.POSIXct(times, format="%m/%d/%y %I:%M%p")
  times <- lubridate::with_tz(times, "UTC")
  times <- format(times, "%Y-%m-%d %H:%M %Z")
  
  
  # create dataframe
  df <- data.frame(unitID=unitIDs)
  df$Alias <- currentStatusTable[["Alias"]]
  df$Location <- currentStatusTable[["Location"]]
  df$datetimeString <- times
  
  return(df)
  
}
