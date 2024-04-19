
library(webshot2)

devtools::load_all()

polar <-
  polarMap(
    polar_data,
    pollutant = "no2",
    limits = c(0, 180),
    type = "daylight",
    popup = c("site", "site_type")
  )

htmlwidgets::saveWidget(polar, "polardata.html")

webshot("polardata.html", file = "polar.png")

traj <- trajMap(traj_data,
                colour = "pm10",
                provider = "CartoDB.DarkMatter",
                cols = "turbo")

htmlwidgets::saveWidget(traj, "trajdata.html")

webshot("trajdata.html", file = "traj.png")

network <- networkMap(provider = "Esri.WorldImagery", cluster = FALSE)

htmlwidgets::saveWidget(network, "networkdata.html")

webshot(
  "networkdata.html",
  file = "network.png",
  vheight = 744,
  vwidth = floor(992 / 2.5)
)

unlink(dir(pattern = ".html|.png"))
