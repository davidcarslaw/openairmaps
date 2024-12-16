test_that("quickTextHTML works", {
  # returns a character
  testthat::expect_type(
    quickTextHTML("no2"),
    type = "character"
  )

  # formats pollutants
  testthat::expect_equal(
    quickTextHTML("no2 (ug/m3)"),
    "NO<sub>2</sub> (\u00B5g&nbsp;m<sup>-3</sup>)"
  )

  # doesn't do anything weird if character is within another word
  testthat::expect_equal(
    quickTextHTML("obnoxious"),
    "obnoxious"
  )
})

test_that("popup creation works", {
  # columns
  expect_equal(
    polar_data %>%
      dplyr::slice_head(n = 1, by = "site") %>%
      buildPopup(
        columns = c("date", "nox", "site"),
        type = "site",
        latitude = "lat",
        longitude = "lon"
      ) %>%
      dplyr::pull(popup),
    c(
      "<b>site</b>: London Bloomsbury<br><b>NO<sub>x</sub></b>: 113<br><b>date</b>: 2009-01-01 to 2009-01-01",
      "<b>site</b>: London Cromwell Road 2<br><b>NO<sub>x</sub></b>: NaN<br><b>date</b>: 2009-01-01 to 2009-01-01",
      "<b>site</b>: London Marylebone Road<br><b>NO<sub>x</sub></b>: 130<br><b>date</b>: 2009-01-01 to 2009-01-01",
      "<b>site</b>: London N. Kensington<br><b>NO<sub>x</sub></b>: 31<br><b>date</b>: 2009-01-01 to 2009-01-01"
    )
  )

  # named columns
  expect_equal(
    polar_data %>%
      dplyr::slice_head(n = 1, by = "site") %>%
      buildPopup(
        columns = c("Date" = "date", "NOx" = "nox", "Site" = "site"),
        latitude = "lat",
        longitude = "lon"
      ) %>%
      dplyr::pull(popup),
    c(
      "<b>Site</b>: London Bloomsbury<br><b>NO<sub>x</sub></b>: 113<br><b>Date</b>: 2009-01-01 to 2009-01-01",
      "<b>Site</b>: London Cromwell Road 2<br><b>NO<sub>x</sub></b>: NaN<br><b>Date</b>: 2009-01-01 to 2009-01-01",
      "<b>Site</b>: London Marylebone Road<br><b>NO<sub>x</sub></b>: 130<br><b>Date</b>: 2009-01-01 to 2009-01-01",
      "<b>Site</b>: London N. Kensington<br><b>NO<sub>x</sub></b>: 31<br><b>Date</b>: 2009-01-01 to 2009-01-01"
    )
  )

})

test_that("postcode conversion works", {
  testthat::expect_equal(
    fmt_api_call("SW1A 1AAA"),
    "api.postcodes.io/postcodes/sw1a1aaa"
  )

  testthat::expect_equal(raw_to_json(as.raw(
    c(
      0x7b,
      0x22,
      0x73,
      0x74,
      0x61,
      0x74,
      0x75,
      0x73,
      0x22,
      0x3a,
      0x34,
      0x30,
      0x34,
      0x2c,
      0x22,
      0x65,
      0x72,
      0x72,
      0x6f,
      0x72,
      0x22,
      0x3a,
      0x22,
      0x49,
      0x6e,
      0x76,
      0x61,
      0x6c,
      0x69,
      0x64,
      0x20,
      0x70,
      0x6f,
      0x73,
      0x74,
      0x63,
      0x6f,
      0x64,
      0x65,
      0x22,
      0x7d
    )
  )),
  list(status = 404L, error = "Invalid postcode"))

})
