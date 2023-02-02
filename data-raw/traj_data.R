# import trajectories
traj <- openair::importTraj(site = "london", year = 2010)

# import data for North Kensington
kc1 <- openair::importAURN("kc1", year = 2010)

kc1 <- dplyr::select(kc1, date, nox, no2, o3, pm2.5, pm10)

# now merge with trajectory data by 'date'
traj <- dplyr::left_join(traj, kc1, by = "date")

traj_data <-
  openair::selectByDate(
    traj,
    start = "15/4/2010",
    end = "21/4/2010"
  )

traj_data <- dplyr::tibble(traj_data)

usethis::use_data(traj_data, overwrite = TRUE)
