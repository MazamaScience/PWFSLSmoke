airnow <- airnow_load(startdate = 20150601, enddate = 20151031)
airnow <- monitor_subset(airnow, stateCodes = c("WA", "OR", "ID"), countryCodes = "US")
airnowID <- monitor_subset(airnow, stateCodes = "ID", countryCodes = "US")
airnowWA <- monitor_subset(airnow, stateCodes = "WA", countryCodes = "US")
airnowOR <- monitor_subset(airnow, stateCodes = "OR", countryCodes = "US")

# worst acute
monitorInteractiveMap(airnow)
worstPm25 <- max(airnow$data[,-1], na.rm=TRUE)
dataArray <- as.matrix(airnow$data[,-1])
worstMonitor <- which(dataArray==worstPm25, arr.ind=TRUE)
worstAcuteMonitor <- colnames(airnow$data)[worstMonitor[2]]

# find meta info for the monitor that has the worst pm
# airnow$meta[which(rownames(airnow$meta)==worstMonitor),]
# AQSID siteCode             siteName status agencyID                                agencyName EPARegion latitude longitude elevation GMTOffsetHours
# 160491012 160491012     1012 Cottonwood E-sampler Active      ID1 Idaho Department of Environmental Quality       R10 46.06132 -116.3467    1091.6             -8
# countryCode FIPSCMSACode CMSAName FIPSMSACode MSAName FIPSStateCode stateCode GNISCountyCode countyName GNISCityCode cityName            timezone monitorID
# 160491012          US                                                      16        ID          16049      IDAHO                       America/Los_Angeles 160491012

monitorPlot_dailyBarplot(ws_monitor=airnow, monitorID=worstAcuteMonitor)

# worst chronic
airnowAvg <- monitor_dailyStatistic(airnow)
numBadDays <- apply(airnowAvg$data[,-1], 2, function(x){ sum(x > AQI$breaks_24[4], na.rm=TRUE) }) #unhealthy level
worstChronicMonitor <- colnames(airnowAvg$data)[which(numBadDays==max(numBadDays))+1]

# this is the same monitor by looking at averages
# pm25Means <- apply(dataArray, 2, function(x){mean(x, na.rm=TRUE)})
# worstChronicMonitor <- names( which(pm25Means==max(pm25Means)) ) # 160490003 

# find meta info for the monitor that has the worst chronic pm
# airnow$meta[which(rownames(airnow$meta)==worstChronicMonitor),]
# AQSID siteCode  siteName status agencyID                            agencyName EPARegion latitude longitude elevation GMTOffsetHours countryCode FIPSCMSACode
# 160490003 160490003     0003 Kamiah-ID Active      TRX Tribal Environmental Exchange Network        R8  46.2094 -116.0275     460.2             -8          US             
# CMSAName FIPSMSACode MSAName FIPSStateCode stateCode GNISCountyCode countyName GNISCityCode cityName            timezone monitorID
# 160490003                                         16        ID          16049      IDAHO                       America/Los_Angeles 160490003
# 

monitorPlot_dailyBarplot(ws_monitor=airnow, monitorID=worstChronicMonitor)

# to see where the two monitors are on the map
monitorMap(airnow)
points(-116.3467, 46.06132,cex=1.8,pch=1,lwd=2,col="blue")
points(-116.0275, 46.2094, cex=1.8,pch=1,lwd=2,col="darkgreen")

# find out the day with the worst pm25
monitorPlot_dailyBarplot(ws_monitor=airnow, monitorID=worstAcuteMonitor, tlim = c(20150809, 20150829)) #0822 0825
monitorPlot_dailyBarplot(ws_monitor=airnow, monitorID=worstChronicMonitor, tlim = c(20150809, 20150829)) #0822 0827

# look at hourly pm at specific bad days
monitorPlot_hourlyBarplot(ws_monitor=airnow, monitorID=worstAcuteMonitor, tlim=c(20150822, 20150825))
monitorPlot_hourlyBarplot(ws_monitor=airnow, monitorID=worstChronicMonitor, tlim=c(20150822, 20150827))

monitorPlot_timeOfDaySpaghetti(ws_monitor=airnow, monitorID=worstAcuteMonitor, tlim=c(20150822, 20150825))
monitorPlot_timeOfDaySpaghetti(ws_monitor=airnow, monitorID=worstChronicMonitor, tlim=c(20150822, 20150827))


# color counties by number of days/hours for AQI levels

numBadDaysCounty <- aggregate(data.frame(numBadDays), list(airnow$meta$GNISCountyCode), max) # mean or max
numBadDaysCounty[,2] <- ceiling(numBadDaysCounty[,2])
colnames(numBadDaysCounty) <- c("fips", "days")
countyFIPs <- maps::county.fips
countyFIPs <- countyFIPs[duplicated(countyFIPs[,1])]
countyFIPs$polyname <- sapply(countyFIPs$polyname,function(x){ return( stringr::str_split_fixed(x,":",2)[1] ) } )
numBadDaysCounty <- data.frame(apply(numBadDaysCounty,2,as.integer))
numBadDaysCounty <- dplyr::left_join(numBadDaysCounty, countyFIPs, by="fips")
colnames(numBadDaysCounty)[3] <- "county"
# 0, 1-5, 6-10, 11-15, 16-20
numBadDaysCounty$colIndex <- .bincode(numBadDaysCounty$days, c(0,1,5,10,15,20), right=FALSE)
cols <- RColorBrewer::brewer.pal(5, "Blues")

map("county",c("WA","OR","ID"))
map("county",numBadDaysCounty$county, col=cols[numBadDaysCounty$colIndex], fill=TRUE, add=TRUE)

if (FALSE) {
  

  
  
    
}
