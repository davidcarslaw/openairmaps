test_that("windrose map works in a simple way", {
  expect_no_error(windroseMap(polar_data))
})

test_that("windrose map works in an advanced way", {
  expect_no_error(windroseMap(
    polar_data,
    control = "weekend",
    popup = c("site", "site_type"),
    label = "site",
    ws.int = 10, breaks = 4
  ))
})

test_that("static windrose map works in a simple way", {
  expect_no_error(windroseMapStatic(polar_data))
})

test_that("static windrose map works in an advanced way", {
  expect_no_error(windroseMapStatic(
    polar_data,
    facet = "weekend",
    ws.int = 10, breaks = 4
  ))
})
