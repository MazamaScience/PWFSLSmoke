
# #' @param provider data provider, e.g. usfs, arb2, etc.
# #' 

# This function will use the rvest package to parse the table found at URLs like  http://usfs.airsis.com.
# It should accept the following arguments:
#   provider (usfs, arb2, etc.)
# It should return a data frame with the following columns:
#   unitID
#   alias
#   location
#   datetimeString

arisis_currentStatus <- function(provider, username, password) {
  
  # TODO: develop code to get response via request, cookies, etc...
  ### response <- httr::GET("http://usfs.airsis.com")  #, authenticate(username, password))
  ### response <- httr::GET("http://usfs.airsis.com/vision/Login.aspx?ReturnUrl=%2fvision", authenticate(username, password))
  
  ### ---- temporary for development/testing purposes ------------------
  providers <- c("USFS", "APCD", "ARB2", "EPA")
  provider <- providers[4]
  response <- readr::read_file(paste0("~/Downloads/", provider, ".htm"))
  ### ------------------------------------------------------------------
  
  # provider-specific settings
  if ( provider=="USFS") {
    linkNode <- "#DataGrid1 td:nth-child(4) a"
  } else if ( provider=="APCD") {
    linkNode <- "td:nth-child(21) a"
  } else if ( provider=="ARB2") {
    linkNode <- "td:nth-child(21) a"
  } else if ( provider=="EPA") {
    linkNode <- "td:nth-child(20) a"
  }
  
  # parse raw html and pull out fields of interest
  currentStatusDoc <- xml2::read_html(response)
  currentStatusNode <- rvest::html_nodes(currentStatusDoc, xpath='//*[@id="DataGrid1"]')
  
  # convert node to table
  currentStatusTable <- rvest::html_table(currentStatusNode, header=TRUE)[[1]]
  
  # get unitIDs from node
  urls <- currentStatusDoc %>%
    html_nodes(linkNode) %>%
    html_attr('href')
  
  unitIDs <- stringr::str_replace(urls, stringr::fixed("UnitHistory.aspx?uid="), "")
  
  # create dataframe
  
  dataTable <- data.frame(unitID=unitIDs)
  dataTable$Alias <- currentStatusTable[["Alias"]]
  dataTable$Location <- currentStatusTable[["Location"]]
  # TODO: clean up date time, including convert to string
  # TODO: account for UTC when possible (APCD, ARB2)
  dataTable$datetimeString <- currentStatusTable[["Date/Time"]]
  View(dataTable)
  
  

}

# tables <- rvest::html_nodes(currentStatusDoc, "table")
# currentStatusTable <- rvest::html_table(tables[[4]], fill=TRUE)


# if ( provider=="USFS") {
#   xpath <- '//*[@id="DataGrid1"]'
# } else if ( provider=="APCD") {
#   xpath <- '//*[@id="DataGrid1"]'
# } else if ( provider=="ARB2") {
#   xpath <- '//*[@id="DataGrid1"]'
# } else if ( provider=="EPA") {
#   xpath <- '//*[@id="DataGrid1"]'
# }

