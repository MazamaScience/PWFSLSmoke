context("test-timeinfo")

test_that("arguments are validated", {
  # Set up for "Thompson Falls, Montana"
  startdate <- lubridate::ymd_hms("2018-11-03 07:00:00")
  enddate <- lubridate::ymd_hms("2018-11-06 07:00:00")
  datetime <- seq(startdate, enddate, by = "hours")
  longitude <- -115.3237
  latitude <- 47.59439
  timezone <- "America/Denver"

  expect_error( timeInfo() )
  expect_error( timeInfo(1:10) )
  expect_error( timeInfo(datetime, "dummy") )
  expect_error( timeInfo(datetime, longitude) )
  expect_error( timeInfo(datetime, longitude, "dummy") )

  skip_on_cran()
  skip_on_travis()

  # Should determine timezone from datetime if no timezone is provided
  expect_silent( {timeInfo <- timeInfo(datetime, longitude, latitude)} )
})

test_that("daylight savings is treated properly", {
  # Set up for "Thompson Falls, Montana"
  startdate <- lubridate::ymd_hms("2018-11-03 07:00:00")
  enddate <- lubridate::ymd_hms("2018-11-06 07:00:00")
  datetime <- seq(startdate, enddate, by = "hours")
  longitude <- -115.3237
  latitude <- 47.59439
  timezone <- "America/Denver"

  timeInfo <- timeInfo(datetime, longitude, latitude, timezone)

  # UTC version is monotonic
  expect_equal(
    strftime(timeInfo$localStandardTime_UTC[24], "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE),
    "2018-11-03 23:00:00 UTC"
  )
  expect_equal(
    strftime(timeInfo$localStandardTime_UTC[25], "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE),
    "2018-11-04 00:00:00 UTC"
  )
  expect_equal(
    strftime(timeInfo$localStandardTime_UTC[26], "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE),
    "2018-11-04 01:00:00 UTC"
  )
  expect_equal(
    strftime(timeInfo$localStandardTime_UTC[27], "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE),
    "2018-11-04 02:00:00 UTC"
  )

  # MDT version has "2018-11-04 01:00:00" twice
  expect_equal(
    strftime(timeInfo$localTime[24], "%Y-%m-%d %H:%M:%S", tz = timezone, usetz = TRUE),
    "2018-11-04 00:00:00 MDT"
  )
  expect_equal(
    strftime(timeInfo$localTime[25], "%Y-%m-%d %H:%M:%S", tz = timezone, usetz = TRUE),
    "2018-11-04 01:00:00 MDT"
  )
  expect_equal(
    strftime(timeInfo$localTime[26], "%Y-%m-%d %H:%M:%S", tz = timezone, usetz = TRUE),
    "2018-11-04 01:00:00 MST"
  )
  expect_equal(
    strftime(timeInfo$localTime[27], "%Y-%m-%d %H:%M:%S", tz = timezone, usetz = TRUE),
    "2018-11-04 02:00:00 MST"
  )

})

test_that("non-Olsen timezones are handled", {
  # Set up for "Portland, Oregon"
  startdate <- lubridate::ymd_hms("2019-06-15 07:00:00")
  enddate <- lubridate::ymd_hms("2019-06-16 06:00:00")
  datetime <- seq(startdate, enddate, by = "hours")

  # Portland, Oregon
  expect_error( timeInfo(datetime, -122.6, 45.5, "US/Pacific"), NA ) # no error
})

test_that("ocean locations require timezones", {
  # Set up for "Portland, Oregon"
  startdate <- lubridate::ymd_hms("2019-06-15 07:00:00")
  enddate <- lubridate::ymd_hms("2019-06-16 06:00:00")
  datetime <- seq(startdate, enddate, by = "hours")

  # Far from land in the Southern Pacific
  expect_error( timeInfo(datetime, -160, -40), NULL ) # error
})
