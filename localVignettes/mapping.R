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
worstMonitor <- colnames(airnow$data)[worstMonitor[2]]

# find meta info for the monitor that has the worst pm
# airnow$meta[which(rownames(airnow$meta)==worstMonitor),]
# AQSID siteCode             siteName status agencyID                                agencyName EPARegion latitude longitude elevation GMTOffsetHours
# 160491012 160491012     1012 Cottonwood E-sampler Active      ID1 Idaho Department of Environmental Quality       R10 46.06132 -116.3467    1091.6             -8
# countryCode FIPSCMSACode CMSAName FIPSMSACode MSAName FIPSStateCode stateCode GNISCountyCode countyName GNISCityCode cityName            timezone monitorID
# 160491012          US                                                      16        ID          16049      IDAHO                       America/Los_Angeles 160491012

monitorPlot_dailyBarplot(ws_monitor=airnow, monitorID=worstMonitor)

#boxplot(dataArray)

# worst chronic
pm25Means <- apply(dataArray, 2, function(x){mean(x, na.rm=TRUE)})
worstChronicMonitor <- names( which(pm25Means==max(pm25Means)) ) # 160490003 

# find meta info for the monitor that has the worst chronic pm
# airnow$meta[which(rownames(airnow$meta)==worstChronicMonitor),]
# AQSID siteCode  siteName status agencyID                            agencyName EPARegion latitude longitude elevation GMTOffsetHours countryCode FIPSCMSACode
# 160490003 160490003     0003 Kamiah-ID Active      TRX Tribal Environmental Exchange Network        R8  46.2094 -116.0275     460.2             -8          US             
# CMSAName FIPSMSACode MSAName FIPSStateCode stateCode GNISCountyCode countyName GNISCityCode cityName            timezone monitorID
# 160490003                                         16        ID          16049      IDAHO                       America/Los_Angeles 160490003
# 

monitorMap(airnow)
points(-116.3467, 46.06132,cex=1.8,pch=1,lwd=2,col="blue")
points(-116.0275, 46.2094, cex=1.8,pch=1,lwd=2,col="darkgreen")

