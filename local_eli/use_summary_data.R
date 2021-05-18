# ------ FUNCTION USAGE ------

summary <- monitor_generateSummary(
  agency = "airsis",
  filePath = "./local_eli/test_monitor_generateSummary.csv"
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

# use the factor(names, ...) to re-force the order of the bars

ggplot(data = aqi_data, mapping = aes(x = factor(names, c(AQI$names, "Missing")), y = freq, fill = freq)) +
  geom_bar(stat = "identity") +
  ylab("Amount reporting") +
  xlab("AQI Level") +
  theme(legend.position = "none")


# timeseries plots

summary <- readr::read_csv("./local_eli/test_monitor_generateSummary.csv", col_types = readr::cols())

# Number of states reporting. Time series
statesReporting_ts <- summary %>%
  group_by(stateCode, preparedAt) %>%
  summarize(n = n()) %>%
  # round time to nearest hour
  mutate(preparedAt = format(round(preparedAt, units = "hours"), format = "%m-%d %H"))

# NOTE: after 5/16 I started getting data from airsis and WRCC
library(ggplot2)
ggplot(data = statesReporting_ts, aes(x = preparedAt, y = n, fill = stateCode)) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Time prepared (month-day hour UTC)") +
  ylab("Number of stations reporting")

# Stations in each latency group. Time series
latency_ts <- summary %>%
  mutate(latencyBin = cut(last_latency, breaks = c(0,1,3,6,12,Inf), right = FALSE)) %>%
  group_by(latencyBin, preparedAt) %>%
  summarize(n = n()) %>%
  mutate(preparedAt = format(round(preparedAt, units = "hours"), format = "%m-%d %H"))

ggplot(data = latency_ts, aes(x = preparedAt, y = n, fill = latencyBin)) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Time prepared (month-day hour UTC)") +
  ylab("Number of stations reporting")


# Agencies. Time series
# This is a better way to plot it. But with so few data points, the bars are really thin
agency_ts <- summary %>%
  group_by(agency, preparedAt) %>%
  summarize(n = n()) %>%
  mutate(preparedAt = lubridate::floor_date(preparedAt, unit = "hours"))

ggplot(data = agency_ts, aes(x = preparedAt, y = n, fill = agency)) +
  geom_col() + # geom_bar(stat = "identity", position = "stack") also works. I just tried something different
  xlab("Time prepared (UTC)") +
  ylab("Number of agencies reporting")

# average latency by state
state_avg_latency <- summary %>%
  group_by(stateCode, preparedAt) %>%
  summarize(avg_latency = mean(last_latency, na.rm = TRUE)) %>%
  mutate(preparedAt = lubridate::floor_date(preparedAt, unit = "hours"))

ggplot(data = state_avg_latency, aes(x = preparedAt, y = avg_latency, color = stateCode)) +
  geom_line() +
  xlab("Time prepared (UTC)") +
  ylab("Average latency by state")
