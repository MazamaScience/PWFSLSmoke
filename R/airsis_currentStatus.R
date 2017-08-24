
#' @param provider data provider, e.g. usfs, arb2, etc.
#' 

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
  
  # temporary for development/testing purposes
  providers <- c("USFS", "APCD", "ARB2", "EPA")
  provider <- providers[4]
  response <- readr::read_file(paste0("~/Downloads/", provider, ".htm"))
  
  
  # parse raw html into tables
  currentStatusDoc <- xml2::read_html(response)
  
  # pull out fields of interest
  currentStatusNode <- rvest::html_nodes(currentStatusDoc, xpath='//*[@id="DataGrid1"]')
  
  # convert node to table
  currentStatusTable <- rvest::html_table(currentStatusNode, header=TRUE)[[1]]
  
  if ( provider=="USFS") {
    linkNode <- "#DataGrid1 td:nth-child(4) a"
  } else if ( provider=="APCD") {
    linkNode <- "td:nth-child(21) a"
  } else if ( provider=="ARB2") {
    linkNode <- "td:nth-child(21) a"
  } else if ( provider=="EPA") {
    linkNode <- "td:nth-child(20) a"
  }
  
  # get links from node
  urls <- currentStatusDoc %>%
    html_nodes(linkNode) %>%
    html_attr('href')

  unitIDs <- stringr::str_replace(urls, stringr::fixed("UnitHistory.aspx?uid="), "")
  unitIDs
  
  currentStatusTable$unitID <- unitIDs
  View(currentStatusTable)
  
  

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


### ---- initial code -----

# info <- xml2::read_html("http://usfs.airsis.com")
# tab <- rvest::html_table(info)
# tab

# from https://stackoverflow.com/questions/24723606/scrape-password-protected-website-in-r

# handle <- httr::handle("http://usfs.airsis.com")
# # path   <- "amember/login.php"
# 
# # # fields found in the login form.
# # login <- list(
# #   amember_login = "",
# #   amember_pass  = ""#,
# # )
# 
# login <- list(
#   txtUserName = "",
#   txtPassword  = ""#,
# )
# 
# response <- httr::POST(handle = handle, body = login)


# from https://stackoverflow.com/questions/32434478/scraping-password-protected-forum-in-r

# library(rvest)
# url <- "http://usfs.airsis.com"
# pgsession <- html_session(url)
# 
# pgform <- html_form(pgsession)[[1]]
# 
# filled_form <- set_values(pgform,
#                           "txtUserName" = "username", 
#                           "txtPassword" = "password")
# 
# submit_form(pgsession, filled_form)
# memberlist <- jump_to(pgsession, "http://usfs.airsis.com/vision/stattable.aspx")
# 
# page <- read_html(memberlist)
# 
# html_text(page)
# 
# usernames <- html_nodes(x = page, css = "#memberlist .Alias")
# 
# data_usernames <- html_text(usernames, trim = TRUE)
