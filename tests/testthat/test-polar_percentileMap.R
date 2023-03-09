options(cli.default_handler = function(...) {})
polar_data <- dplyr::filter(polar_data, site %in% unique(site)[1:2])

test_that("percentile map works in a simple way", {
  expect_no_error(percentileMap(polar_data, "nox"))
})

test_that("percentile map works with two pollutants", {
  expect_no_error(percentileMap(polar_data, c("nox", "pm2.5")))
})

test_that("percentile map works in an advanced way", {
  expect_no_error(percentileMap(
    polar_data,
    "nox",
    control = "weekend",
    popup = c("site", "site_type"),
    label = "site",
    percentile = c(25, 50)
  ))
})

test_that("static percentile map works in a simple way", {
  expect_no_error(percentileMapStatic(polar_data, "nox"))
})

test_that("static percentile map works with two pollutants", {
  expect_no_error(percentileMapStatic(polar_data, c("nox", "pm2.5")))
})

test_that("static percentile map works in an advanced way", {
  expect_no_error(percentileMapStatic(
    polar_data,
    "nox",
    facet = "weekend",
    percentile = c(25, 50)
  ))
})

