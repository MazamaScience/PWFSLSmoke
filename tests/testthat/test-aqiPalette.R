context("aqiPalette")

test_that("arguments are validated", {
  expect_error(aqiPalette("not-a-palette-name"))
})

test_that("reverse argument works", {
  pm25 <- c(5,20,40,100,200,400)  # the middle of each AQI bin
  colors <- aqiPalette("aqi")(pm25)
  rev_colors <- aqiPalette("aqi", reverse = TRUE)(pm25)
  expect_equal(rev_colors, rev(colors))
})