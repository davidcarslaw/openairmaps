options(cli.default_handler = function(...) {})
polar_data <- dplyr::filter(polar_data, site %in% unique(site)[1:2])

test_that("the add polar markers function works", {
  expect_no_error(
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      addPolarMarkers(
        data = polar_data,
        pollutant = "ws",
        fun = openair::windRose,
        group = "Wind Rose"
      ) %>%
      addPolarMarkers(
        data = polar_data,
        pollutant = "nox",
        fun = openair::polarPlot,
        group = "Polar Plot"
      ) %>%
      leaflet::addLayersControl(baseGroups = c("Wind Rose", "Polar Plot"))
  )
})


test_that("the add polar diff markers function works", {
  expect_no_error(
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      addPolarDiffMarkers(
        before = polar_data,
        after = dplyr::mutate(polar_data, nox = jitter(.data$nox, 5)),
        pollutant = "nox"
      )
  )
})
