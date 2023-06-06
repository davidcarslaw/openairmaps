options(cli.default_handler = function(...) {})
polar_data <- dplyr::filter(polar_data, site %in% unique(site)[1:2])

test_that("pollutionrose map works with two pollutants", {
  expect_no_error(pollroseMap(polar_data, c("nox", "pm2.5")))
})

test_that("pollutionrose map works in an advanced way", {
  expect_no_error(pollroseMap(
    polar_data,
    "nox",
    control = "weekend",
    popup = c("site", "site_type"),
    label = "site",
    breaks = 10
  ))
})

test_that("static pollutionrose map works with two pollutants", {
  expect_no_error(pollroseMapStatic(polar_data, c("nox", "pm2.5")))
})

test_that("static pollutionrose map works in an advanced way", {
  expect_no_error(pollroseMapStatic(
    polar_data,
    "nox",
    facet = "weekend",
    breaks = 10
  ))
})
