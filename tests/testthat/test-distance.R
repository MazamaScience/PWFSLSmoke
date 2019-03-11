context("test-distance")

test_that("distance calculation works", {
  expected <- c(277.5866, 265.2488, 253.0362, 240.9666, 229.0613,
                217.3459, 205.8511, 194.6145, 183.6818, 173.1087, 162.9633)
  expect_equal(
    distance(-122.3088, 47.4502, seq(-123,-122,.1), seq(45,46,.1)),
    expected,
    tolerance = .01 # within 10 meters
  )
})
