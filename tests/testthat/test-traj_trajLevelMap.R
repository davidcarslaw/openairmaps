test_that("trajlevelmap works in a basic way", {
  expect_no_error(trajLevelMap(traj_data, pollutant = "pm2.5"))
})

test_that("trajlevelmap works in a more complex way", {
  expect_no_error(
    trajLevelMap(
      traj_data,
      pollutant = "pm2.5",
      control = "weekend",
      statistic = "difference",
      lon.inc = 2,
      lat.inc = 2,
      min.bin = 5,
      alpha = .5,
      tile.border = "white",
      cols = "turbo",
      provider = "CartoDB.Positron"
    )
  )
})
