context("loadData")

test_that("Bad input generates errors", {
  expect_error( airnow_loadLatest(parameter = "DUMMY") )
  expect_error( airnow_loadLatest(baseUrl = NULL, dataDir = "/DUMMY") )
  expect_error( airsis_loadLatest(parameter = "DUMMY") )
  expect_error( airsis_loadLatest(baseUrl = NULL, dataDir = "/DUMMY") )
  expect_error( wrcc_loadLatest(parameter = "DUMMY") )
  expect_error( wrcc_loadLatest(baseUrl = NULL, dataDir = "/DUMMY") )
  expect_error( monitor_loadLatest(parameter = "DUMMY") )
  expect_error( monitor_loadLatest(baseUrl = NULL, dataDir = "/DUMMY") )
  expect_error( airnow_loadDaily(parameter = "DUMMY") )
  expect_error( airnow_loadDaily(baseUrl = NULL, dataDir = "/DUMMY") )
  expect_error( airsis_loadDaily(parameter = "DUMMY") )
  expect_error( airsis_loadDaily(baseUrl = NULL, dataDir = "/DUMMY") )
  expect_error( wrcc_loadDaily(parameter = "DUMMY") )
  expect_error( wrcc_loadDaily(baseUrl = NULL, dataDir = "/DUMMY") )
  expect_error( monitor_loadDaily(parameter = "DUMMY") )
  expect_error( monitor_loadDaily(baseUrl = NULL, dataDir = "/DUMMY") )
  # On CRAN -- skip all internet based testing
  skip_on_cran()
  expect_error( airnow_loadLatest(baseUrl = "https://haze.airfire.org/DUMMY") )
  expect_error( airsis_loadLatest(baseUrl = "https://haze.airfire.org/DUMMY") )
  expect_error( wrcc_loadLatest(baseUrl = "https://haze.airfire.org/DUMMY") )
  expect_error( airnow_loadDaily(baseUrl = "https://haze.airfire.org/DUMMY") )
  expect_error( airsis_loadDaily(baseUrl = "https://haze.airfire.org/DUMMY") )
  expect_error( wrcc_loadDaily(baseUrl = "https://haze.airfire.org/DUMMY") )
})

test_that("monitor_load() checks year boundaries", {
  expect_error( monitor_load(20171201,20180201) )
})

test_that("monitor_load() returns the correct year", {
  # On CRAN -- skip all internet based testing
  skip_on_cran()
  ws_monitor <- monitor_load(20180801,20180901)
  expect_equal( lubridate::year(ws_monitor$data$datetime[1]), 2018 )
})

test_that("monitor_load() requires monitorIDs", {
  # Joining w/o monitorIDs is disallowed
  now <- lubridate::now("UTC")
  now_m60 <- now - lubridate::ddays(60)
  expect_error( monitor_load(now_m60, now) )
})
