Overall outline might look like this:
## Welcome to the data archive
* temporary monitors (WRCC, AIRSIS) vs. permanent monitors (AirNow)
* three types of archive files — hourly, daily, monthly
* structure of the archive
* loading files directly from the archive with get(load(url(…)))
## loading functions
* ~loadLatest, ~loadDaily, ~loadAnnual
* monitor_load() — the preferred function for general data loading
* note — no ability to cross year boundaries (OK because there are few wildfires on New Year’s Eve)
## examples
* most current example -- load and create plot the using Latest data for Washington
* archival example — load and create plot using data from OR-ID-WA during 2015 (this is the package dataset Northwest Megafires but obtain the data from the archive)
