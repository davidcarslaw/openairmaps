
# Import trajectory data --------------------------------------------------

traj <- openair::importTraj(site = "london", year = 2009)

kc1 <- openair::importAURN("kc1", year = 2009)

aq <- dplyr::select(kc1, date, nox, no2, o3, pm10, pm2.5)

traj_data <- dplyr::left_join(traj, aq, by = "date")

traj_data <- dplyr::tibble(traj_data)

usethis::use_data(traj_data, overwrite = TRUE)
