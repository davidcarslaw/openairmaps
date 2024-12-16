test_that("lat/lng can be assumed", {
  # error with duplicates
  testthat::expect_error(assume_latlon(
    quiet = TRUE,
    data.frame(
      latitude = character(),
      lat = character(),
      longitude = character(),
      lng = character()
    ),
    NULL,
    NULL
  ))

  # duplicates are ignored if specified
  testthat::expect_equal(
    assume_latlon(
      quiet = TRUE,
      data.frame(
        latitude = character(),
        lat = character(),
        longitude = character()
      ),
      latitude = "lat",
      NULL
    ),
    list(latitude = "lat",
         longitude = "longitude")
  )

  # specified lat/lng are honoured
  testthat::expect_equal(
    assume_latlon(
      quiet = TRUE,
      data.frame(
        latitude = character(),
        some_weird_column = character(),
        longitude = character(),
        another_weird_column = character()
      ),
      latitude = "some_weird_column",
      longitude = "another_weird_column"
    ),
    list(latitude = "some_weird_column",
         longitude = "another_weird_column")
  )

  # fail if no matches
  testthat::expect_error(assume_latlon(
    quiet = TRUE,
    data.frame(
      some_weird_column = character(),
      another_weird_column = character()
    ),
    latitude = NULL,
    longitude = NULL
  ))
})
