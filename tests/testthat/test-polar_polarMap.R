test_that("polar map works in a simple way", {
  expect_no_error(polarMap(polar_data, "nox"))
})

test_that("polar map works with two pollutants", {
  expect_no_error(polarMap(polar_data, c("nox", "pm2.5")))
})

test_that("polar map works in an advanced way", {
  expect_no_error(polarMap(
    polar_data,
    "nox",
    control = "weekend",
    popup = c("site", "site_type"),
    label = "site",
    limits = c(0, 200)
  ))
})

test_that("static polar map works in a simple way", {
  expect_no_error(polarMapStatic(polar_data, "nox"))
})

test_that("static polar map works with two pollutants", {
  expect_no_error(polarMapStatic(polar_data, c("nox", "pm2.5")))
})

test_that("static polar map works in an advanced way", {
  expect_no_error(polarMapStatic(
    polar_data,
    "nox",
    facet = "weekend",
    limits = c(0, 200)
  ))
})
