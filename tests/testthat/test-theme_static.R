test_that("theme can be added to a ggplot2 object", {
  options(device = "cairo")

  testthat::expect_no_error(
    plt <- ggplot2::ggplot() + theme_static()
  )
})
