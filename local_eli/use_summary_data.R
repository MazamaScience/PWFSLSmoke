# ------ FUNCTION USAGE ------

summary <- monitor_generateSummary(
  agency = "airsis",
  filePath = "~/test_monitor_generateSummary.csv"
)

# ------ SAMPLE STATISTICS ------

agenciesReporting <-
  summary$agency %>%
  table()

statesReporting <-
  summary$stateCode %>%
  table()

latencyBreaks <-
  cut(summary$last_latency, breaks = c(0,1,3,6,12,Inf), right = FALSE) %>%
  table()

# AQI levels
aqiLevels <-
  cut(summary$last_nowcast_1hr, breaks = AQI$breaks_24, right = FALSE) %>%
  table() %>%
  as.list()

aqiLevels$missing <-
  summary$last_nowcast_1hr[is.na(summary$last_nowcast_1hr)] %>%
  length()

# ------ EXAMPLE PLOTS ------

library(ggplot2)

# agency bar chart
agenciesReporting_latest <-
  summary %>%
  dplyr::filter(preparedAt == max(preparedAt)) %>%
  dplyr::select(agency) %>%
  table() %>%
  as.data.frame()

ggplot(data = agenciesReporting_latest, mapping = aes(x = ., y = Freq, fill = Freq)) +
  geom_bar(stat = "identity") +
  ylab("Amount reporting") +
  xlab("Agency") +
  theme(legend.position = "none")

# latency histograms
latency_latest <-
  summary %>%
  dplyr::filter(preparedAt == max(preparedAt)) %>%
  dplyr::select(last_latency)

ggplot(data = latency_latest, mapping = aes(x = last_latency)) +
  geom_histogram(bins = 10) +
  ylab("Amount reporting") +
  xlab("Latency (hrs)") +
  theme(legend.position = "none")

latency_table <-
  latency_latest %>%
  dplyr::pull() %>%
  cut(breaks = c(0,1,3,6,12,Inf), right = FALSE) %>%
  table() %>%
  as.data.frame()

ggplot(data = latency_table, mapping = aes(x = ., y = Freq, fill = Freq)) +
  geom_bar(stat = "identity") +
  ylab("Amount reporting") +
  xlab("Latency (hrs)") +
  theme(legend.position = "none")

# AQI histogram

# there's probably a better way to do this....
aqi_latest <-
  summary %>%
  dplyr::filter(preparedAt == max(preparedAt)) %>%
  dplyr::pull(last_nowcastLevel)

aqi_freq_list <-
  aqi_latest %>%
  table() %>%
  as.list()

aqi_data <- data.frame(names = AQI$names, freq = as.numeric(NA), stringsAsFactors = FALSE)
for(i in seq(1,6,1)) {
  aqi_data$freq[[i]] <- ifelse(is.null(aqi_freq_list[[as.character(i)]]), 0, aqi_freq_list[[as.character(i)]])
}
aqi_data[7, ] <- c("Missing", aqi_latest[is.na(aqi_latest)] %>% length())

ggplot(data = aqi_data, mapping = aes(x = names, y = freq, fill = freq)) +
  geom_bar(stat = "identity") +
  ylab("Amount reporting") +
  xlab("AQI Level") +
  theme(legend.position = "none")

