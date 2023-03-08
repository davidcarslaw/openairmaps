test_that("Ricardo managed networks work", {
  expect_no_error(
    openairmaps::networkMap(
      c("aurn", "saqn", "waqn", "ni", "aqe", "local"),
      control = "variable",
      year = 2023,
      provider = "CartoDB.Positron",
      draw.legend = TRUE,
      collapse.control = FALSE
    )
  )
})

test_that("kcl works", {
  expect_no_error(openairmaps::networkMap("kcl", control = "site_type", year = 2023))
})

test_that("saqd works", {
  expect_no_error(openairmaps::networkMap("saqd", year = 2023, control = "site_type"))
})

test_that("europe works", {
  expect_no_error(openairmaps::networkMap(
    "europe",
    year = 2023,
    provider = c("OpenStreetMap", "Esri.WorldImagery"),
    control = "site_type"
  ))
})

test_that("bad control options error", {
  expect_error(
    openairmaps::networkMap("europe", year = 2023, control = "something totally random!")
  )
})

test_that("multiple providers and no control works", {
  expect_no_error(openairmaps::networkMap(
    c("aurn", "aurn"),
    provider = c(
      "CartoDB.Positron",
      "CartoDB.Positron",
      "Esri.WorldImagery"
    )
  ))
})
