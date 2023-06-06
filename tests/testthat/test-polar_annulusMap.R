options(cli.default_handler = function(...) {})
polar_data <- dplyr::filter(polar_data, site %in% unique(site)[1:2])

test_that("annulus map works with two pollutants", {
  expect_no_error(annulusMap(polar_data, c("nox", "pm2.5")))
})

test_that("annulus map works in an advanced way", {
  expect_no_error(annulusMap(
    polar_data,
    "nox",
    control = "weekend",
    popup = c("site", "site_type"),
    label = "site",
    limits = c(0, 200)
  ))
})

test_that("static annulus map works with two pollutants", {
  expect_no_error(annulusMapStatic(polar_data, c("nox", "pm2.5")))
})

test_that("static annulus map works in an advanced way", {
  expect_no_error(annulusMapStatic(
    polar_data,
    "nox",
    facet = "weekend",
    limits = c(0, 200)
  ))
})
