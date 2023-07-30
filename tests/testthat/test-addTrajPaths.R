test_that("multiplication works", {
  expect_no_error(leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    addTrajPaths(data = traj_data))
})
