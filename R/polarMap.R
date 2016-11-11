# function to plot polar plots on leaflet maps

#' Bivariate polar plots on interactive leaflet maps
#'
#' @param data A data frame. The data frame must contain the data to
#'   plot a \code{polarPlot}, which includes wind speed (\code{ws}),
#'   wind direction (\code{wd}) and the column representing the
#'   concentration of a pollutant. In addition, \code{data} must
#'   include a decimal latitude and longitude.
#' @param pollutant The column name of the pollutant to plot.
#' @param latitude The decimal latitude.
#' @param longitude The decimal longitude.
#' @param dir_polar The location of a directory to store the polar plots that are generated. Note, if the directory does not exist it is generated. If the directory does exist all plots will be deleted when the function is run.
#' @param type See \code{type} in openair \code{polarPlot}.
#' @param cols The colours used for plotting.
#' @param fig.width The width of the plots to be produced in inches.
#' @param fig.height The height of the plots to be produced in inches.
#'
#' @return
#' @import leaflet
#' @importFrom grDevices dev.off png
#' @export
#'
#' @examples
polarMap <- function(data, pollutant = "nox", latitude = "lat",
                     longitude = "lon",
                     dir_polar = "~/dir_polar",
                     type = "default",
                     cols = "jet",
                     fig.width = 4, fig.height = 4) {

  . <- NULL

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

    png(paste0(dir_polar, data$site[1], ".png"),
        width = fig.width * 300,
        height = fig.height * 300, res = 300, bg = "transparent")

    polarPlot(data, pollutant = pollutant, key = FALSE, ...)

    dev.off()

  }

  # go through all sites and make some plots
  group_by_(data, .dots = type) %>%
    do(plot_polar(., pollutant, cols = cols))


  # definition of 'icons' aka the openair plots
  leafIcons <- icons(
    iconUrl = list.files(dir_polar, full.names = TRUE),
    iconWidth = 200, iconHeight = 200
  )

  leaflet(data = data) %>%
    addTiles() %>%
    # addProviderTiles(provider = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldTopoMap") %>%
    addMarkers(~longitude, ~latitude, icon = leafIcons, popup = ~site_name)

}
