test_that("trajmap works in a basic way", {
  expect_no_error(trajMap(traj_data))
})

test_that("trajmap works in a more complex way", {
  expect_no_error(
    trajMap(
      traj_data,
      colour = "pm2.5",
      cols = "turbo",
      control = "weekend",
      alpha = .5,
      npoints = 24,
      provider = "CartoDB.Positron",
      collapse.control = TRUE
    )
  )
})
