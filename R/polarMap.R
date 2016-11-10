# function to plot polar plots on leaflet maps

polarMap <- function(data, dir_polar = "~/dir_polar", pollutant = "nox") {

  # check that directory is empty / exists
  if (dir.exists(dir_polar)) {
    # remove existing files
    files <- list.files(dir_polar, full.names = TRUE)
    file.remove(files)

  } else {

    dir.create(dir_polar)

  }

  # function to produce a polar plot, with transparent background
  plot_polar <- function(data, pollutant, ...) {

    png(paste0(dir_polar, data$site[1], ".png"), width = 4 * 300,
        height = 4 * 300, res = 300, bg = "transparent")

    polarPlot(data, pollutant = pollutant, key = FALSE, ...)

    dev.off()

  }

  # go through all sites and make some plots
  plyr::ddply(data, "site", plot_polar, pollutant, cols = "jet")


  # definition of 'icons' aka the openair plots
  leafIcons <- icons(
    iconUrl = list.files(dir_polar, full.names = TRUE),
    iconWidth = 200, iconHeight = 200
  )

  leaflet(data = data_process) %>%
    addTiles() %>%
    # addProviderTiles(provider = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldTopoMap") %>%
    addMarkers(~longitude, ~latitude, icon = leafIcons, popup = ~site_name)

}
