options(cli.default_handler = function(...) {})
polar_data <- dplyr::filter(polar_data, site %in% unique(site)[1:2])

test_that("freq map works with two pollutants", {
  expect_no_error(freqMap(polar_data, c("nox", "pm2.5")))
})

test_that("freq map works in an advanced way", {
  expect_no_error(freqMap(
    polar_data,
    "nox",
    control = "weekend",
    popup = c("site", "site_type"),
    label = "site",
    breaks = c(0, 1, 5, 7, 10)
  ))
})
