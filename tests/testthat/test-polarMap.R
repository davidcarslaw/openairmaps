test_that("polarMap works", {
  input <-
    polar_data %>%
    dplyr::filter(!is.na(no2), !is.na(pm10)) %>%
    dplyr::slice_head(n = 100, by = site)

  # dynamic map
  testthat::expect_no_error(
    polarMap(
      input,
      "no2",
      progress = FALSE,
      latitude = "lat",
      longitude = "lon",
      key = TRUE,
      k = 50
    )
  )

  # multiple pollutants
  testthat::expect_no_error(
    polarMap(
      input,
      c("no2","pm10"),
      static = TRUE,
      progress = FALSE,
      latitude = "lat",
      longitude = "lon",
      key = TRUE,
      k = 50
    )
  )

  # type opt
  testthat::expect_no_error(
    polarMap(
      input,
      c("no2"),
      static = TRUE,
      progress = FALSE,
      latitude = "lat",
      longitude = "lon",
      key = TRUE,
      k = 50,
      type = "site"
    )
  )

  # warn with multiple pollutants & type
  testthat::expect_warning(
    polarMap(
      input,
      c("no2", "pm10"),
      static = TRUE,
      progress = FALSE,
      latitude = "lat",
      longitude = "lon",
      key = TRUE,
      k = 50,
      type = "site"
    )
  )

  # pairwise stats
  testthat::expect_no_error(
    polarMap(
      polar_data %>%
        dplyr::filter(!is.na(no2), !is.na(pm10)) %>%
        dplyr::slice_head(n = 1000, by = site),
      c("no2", "pm10"),
      statistic = "Pearson",
      static = TRUE,
      progress = FALSE,
      latitude = "lat",
      longitude = "lon"
    )
  )
})
