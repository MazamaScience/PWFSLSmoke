context("loadData")

test_that("Times are validated", {
  now <- lubridate::now()
  expect_error(loadData())
  expect_error(loadData(20180601))
  # bad date
  expect_error(loadData(20180601,20181301))
  # joining annual and latest without monitorIDs
  expect_error(loadData(20180101, now))
})
