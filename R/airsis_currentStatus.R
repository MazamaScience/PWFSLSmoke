#' #' ###@export
#' #' @title Load Current Status from Airsis Site
#' #' @description Scrape current monitor status from Airsis website (http://xxxx.airsis.com
#' #' where xxxx %in% c("usfs", "apcd", "arb2", "epa").
#' #' @param provider data provider, e.g. usfs, arb2, etc.
#' #' @param username username for login credentials
#' #' @param password password for login credentials
#' #' @return A dataframe with unit ID, alias, location, and latest update time, in UTC.
#' #' @examples
#' #' \dontrun{
#' #' epa <- airsis_currentStatus(provider="epa")
#' #' View(epa)
#' #' }
#' 
#' airsis_currentStatus <- function(provider, username, password) {
#' 
#'   # TODO: develop code to get response via request, cookies, etc...
#'   ### response <- httr::GET("http://usfs.airsis.com")  #, authenticate(username, password))
#'   ### response <- httr::GET("http://usfs.airsis.com/vision/Login.aspx?ReturnUrl=%2fvision", authenticate(username, password))
#' 
#'   if ( FALSE ) {
#' 
#'     # ----- GET login form ----------------------------------------------------
#'     #   http://usfs.airsis.com/vision/Login.aspx
#' 
#'     # REQUEST:
#'     #
#'     # GET /vision/Login.aspx HTTP/1.1
#'     # Host: usfs.airsis.com
#'     # Connection: keep-alive
#'     # User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.91 Safari/537.36
#'     # Upgrade-Insecure-Requests: 1
#'     # Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8
#'     # Accept-Encoding: gzip, deflate
#'     # Accept-Language: en-US,en;q=0.8
#' 
#'     # RESPONSE:
#'     #
#'     # HTTP/1.1 200 OK
#'     # Cache-Control: private
#'     # Content-Type: text/html; charset=utf-8
#'     # Server: Microsoft-IIS/8.5
#'     # X-AspNet-Version: 4.0.30319
#'     # X-Powered-By: ASP.NET
#'     # Date: Thu, 21 Sep 2017 18:50:10 GMT
#'     # Content-Length: 2245
#'     # loginUrl <- paste0("http://", provider, ".airsis.com/vision/Login.aspx")
#'     # r <- httr::GET(loginUrl)
#'     # loginDoc <- httr::content(r, as='parsed') # automatic conversion to "xml_document"
#' 
#'     loginUrl <- paste0("http://", provider, ".airsis.com/vision/Login.aspx")
#'     r <- httr::GET(loginUrl)
#'     loginDoc <- httr::content(r, as='parsed') # automatic conversion to "xml_document"
#' 
#'     # Extract "hidden" input parameters
#'     viewState <- rvest::html_attr(rvest::html_node(loginDoc, css="input#__VIEWSTATE"), "value")
#'     viewStateGenerator <- rvest::html_attr(rvest::html_node(loginDoc, css="input#__VIEWSTATEGENERATOR"), "value")
#'     eventValidation <- rvest::html_attr(rvest::html_node(loginDoc, css="input#__EVENTVALIDATION"), "value")
#' 
#'     # ----- POST login form ---------------------------------------------------
#' 
#'     # REQUEST:
#'     #
#'     # POST /vision/Login.aspx HTTP/1.1
#'     # Host: usfs.airsis.com
#'     # Connection: keep-alive
#'     # Content-Length: 403
#'     # Pragma: no-cache
#'     # Cache-Control: no-cache
#'     # Origin: http://usfs.airsis.com
#'     # Upgrade-Insecure-Requests: 1
#'     # Content-Type: application/x-www-form-urlencoded
#'     # User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.91 Safari/537.36
#'     # Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8
#'     # Referer: http://usfs.airsis.com/vision/Login.aspx
#'     # Accept-Encoding: gzip, deflate
#'     # Accept-Language: en-US,en;q=0.8
#' 
#'     # RESPONSE:
#'     #
#'     # HTTP/1.1 302 Found
#'     # Cache-Control: private
#'     # Content-Type: text/html; charset=utf-8
#'     # Location: /vision/default.aspx
#'     # Server: Microsoft-IIS/8.5
#'     # Set-Cookie: ASP.NET_SessionId=owboccet52glkq4uv1hziycn; path=/; HttpOnly
#'     # X-AspNet-Version: 4.0.30319
#'     # Set-Cookie: AIRSIS_Vision=3D86BF175248A6A95F24443F0F578433B6DB932194869D088EC66A337107BF8B13F9212D5C69376D489EADEFC60D0EB2BD4081637227111544A08E4FD4BF5AD4BBAF07E6B8658020821E2C1871B7D995ECC8A89678D33C228A39B9A2993A0E62; path=/; HttpOnly
#'     # X-Powered-By: ASP.NET
#'     # Date: Thu, 21 Sep 2017 18:48:37 GMT
#'     # Content-Length: 2395
#' 
#'     # POST login form
#'     loginUrl <- paste0("http://", provider, ".airsis.com/vision/Login.aspx")
#'     body <- list("__VIEWSTATE"=viewState,
#'                  "__VIEWSTATEGENERATOR"=viewStateGenerator,
#'                  "__EVENTVALIDATION"=eventValidation,
#'                  "tzo"="7",
#'                  "txtUserName"=provider,
#'                  "txtPassword"=provider)
#'     s <- httr::POST(loginUrl,
#'                     body=body,
#'                     encode="form")
#' 
#'     # cookiesDF <- httr::cookies(r)
#'     # sessionId <- cookiesDF$value[cookiesDF$name == "ASP.NET_SessionId"]
#' 
#' 
#'     # GET /vision/stattable.aspx HTTP/1.1
#'     # Host: usfs.airsis.com
#'     # Connection: keep-alive
#'     # Pragma: no-cache
#'     # Cache-Control: no-cache
#'     # Upgrade-Insecure-Requests: 1
#'     # User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.91 Safari/537.36
#'     # Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8
#'     # Referer: http://usfs.airsis.com/vision/stattable.aspx
#'     # Accept-Encoding: gzip, deflate
#'     # Accept-Language: en-US,en;q=0.8
#'     # Cookie: ASP.NET_SessionId=bxj4bda1lbu0vqrrjjv5fc1k; AIRSIS_Vision=FF8256F6854E7FED9F504445AF5689D12A760B0F8AD5D0902454B2DFECD718EFE0DAB18FC449119C4302CB35540E8A0C381A0DEBB3AA58872541BE6AE9BD3598C9D08EB704F81A1CC45F7F55A15CF15D979F10234BC823BB63CAD3338A80AC7D
#' 
#'     # airsisStattableUrl <- "http://usfs.airsis.com/vision/stattable.aspx"
#'     # r <- httr::POST(airsisStattableUrl,
#'     #                 body=body,
#'     #                 httr::set_cookies(ASP.NET_SessionId=sessionId),
#'     #                 httr::verbose(info=TRUE))
#' 
#'     # GET station table
#'     #   http://usfs.airsis.com/vision/stattable.aspx
#'     bopUrl <- "http://usfs.airsis.com/vision/stattable.aspx"
#'     r <- httr::GET(bopUrl,
#'                    httr::set_cookies(ASP.NET_SessionId="lzsdznssvmz5jsw4xrvnbbk2",
#'                                      AIRSIS_Vision="B6311DC82CDA36DC51D0232F6BE9339F669B496942058A81B5DE6DE74D6A194B5BAAE9B6A5CD4A80A099641FCA79F9EDE00C7FF05985787B55C51D6C182C8ACED6C45C8BAAB56B96C47C26DC97BCE059B20E54F53F61084D55B9321993B30C10"))
#' 
#'   }
#' 
#'   ### ---- temporary for development/testing purposes ------------------
#'   # # providers <- c("usfs", "apcd", "arb2", "epa")
#'   # # provider <- providers[4]
#'   # response <- readr::read_file(paste0("~/Downloads/", provider, ".htm"))
#'   ### ------------------------------------------------------------------
#' 
#'   # NOTE: It appears AIRSIS reports last update time in their computer's time zone, rather than UTC.
#'   AIRSIS_COMPUTATIONAL_TIMEZONE <- "America/Los_Angeles"
#' 
#'   provider <- stringr::str_to_lower(provider)
#' 
#'   ### testing
#'   content <- httr::content(r)
#'   html <- xml2::read_html(r)
#'   ###
#' 
#' 
#'   # parse raw html and pull out fields of interest
#'   logger.debug("Parsing html file")
#'   currentStatusDoc <- xml2::read_html(response)
#'   currentStatusNode <- rvest::html_nodes(currentStatusDoc, css="#DataGrid1")
#' 
#'   # convert node to table
#'   logger.debug("Extracting current status table from html document")
#'   currentStatusTable <- rvest::html_table(currentStatusNode, header=TRUE)[[1]]
#' 
#'   # get unitIDs from node
#'   if ( provider=="usfs") {
#'     linkNode <- "#DataGrid1 td:nth-child(4) a"
#'   } else if ( provider=="apcd") {
#'     linkNode <- "td:nth-child(21) a"
#'   } else if ( provider=="arb2") {
#'     linkNode <- "td:nth-child(21) a"
#'   } else if ( provider=="epa") {
#'     linkNode <- "td:nth-child(20) a"
#'   } else {
#'     stop('provider not supported')
#'   }
#'   logger.debug("Extracting unit IDs via URLs from html document")
#'   urls <- rvest::html_nodes(currentStatusDoc, linkNode)
#'   urls <- rvest::html_attr(urls, 'href')
#'   unitIDs <- stringr::str_replace(urls, stringr::fixed("UnitHistory.aspx?uid="), "")
#' 
#'   # get times
#'   # TODO: verify data is in local time
#'   # TODO: output format OK?
#'   times <- currentStatusTable[["Date/Time"]]
#'   times <- as.POSIXct(times, format="%m/%d/%y %I:%M%p", tz=AIRSIS_COMPUTATIONAL_TIMEZONE)
#'   times <- lubridate::with_tz(times, "UTC")
#'   # times <- format(times, "%Y-%m-%d %H:%M %Z")
#' 
#'   # create dataframe
#'   logger.debug("Combining unit status into a single dataframe")
#'   df <- data.frame(unitID=unitIDs, stringsAsFactors = FALSE)
#'   df$alias <- currentStatusTable[["Alias"]]
#'   df$location <- currentStatusTable[["Location"]]
#'   df$lastUpdateTime <- times
#'   df$provider <- provider
#' 
#'   return(df)
#' 
#' }
