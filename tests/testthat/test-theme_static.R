test_that("theme can be added to a ggplot2 object", {
  testthat::expect_no_error(
    ggplot2::ggplot() + theme_static()
  )
})
