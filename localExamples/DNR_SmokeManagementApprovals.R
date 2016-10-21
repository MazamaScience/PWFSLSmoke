###############################################################################
# Screen-scrape DNR website to build dataframe of Smoke Management Approvals
#
# URLs of the form:
#   "https://fortress.wa.gov/dnr/protection/BurnRequests/View/05-11-2016"
#
###############################################################################

# Configurable base URL
baseUrl <- "https://fortress.wa.gov/dnr/protection/BurnRequests/View/"

# Create a daily time axis and associated datestamps
startdate <- lubridate::ymd("2016-09-01")
###enddate <- lubridate::ymd("2016-09-30")
enddate <- lubridate::today()
timeAxis <- seq(startdate, enddate, by="day")
datestamps <- strftime(timeAxis, "%m-%d-%Y")

# List to store daily dataframes
dfList <- list()

# Screen-scrape daily tables --------------------------------------------------

for (datestamp in datestamps) {
  url <- paste0(baseUrl,datestamp)
  print(paste0("Downloading ",url))
  wikiDoc <- xml2::read_html(url)
  tables <- rvest::html_nodes(wikiDoc, "table")
  df <- rvest::html_table(tables[[1]])
  # Replace "empty" tables with NA except for the comments column
  if ( nrow(df) == 1 && stringr::str_detect(df$Permit[1], "No smoke management request") ) {
    for ( name in setdiff(names(df), c('Comments')) ) df[[name]] <- NA
  }
  df$datestamp <- datestamp
  dfList[[datestamp]] <- df
}

# Combine daily tables into a single dataframe
df <- dplyr::bind_rows(dfList)

# Clean up dataframe ----------------------------------------------------------

# Fix doubled Permit column, matching initial non-whitespace characters
df$Permit <- stringr::str_match(df$Permit,'^\\S+')[,1]

# Separate Longitude and Latitude
location <- stringr::str_split_fixed(df$`Lat/Lon`,' ',2)
df$Latitude <- location[,1]
df$Longitude <- location[,2]
df$`Lat/Lon` <- NULL

# Create DNR_Pilot column
pattern <- "24 hour|24hr|24 hr|2928|pilot|Pilot"
df$DNR_Pilot <- stringr::str_detect(df$Comments,pattern)

df$datetime <- lubridate::mdy(df$datestamp, tz="America/Los_Angeles")

# Save output
readr::write_csv(df, path="SmokeManagementApprovals.csv")

