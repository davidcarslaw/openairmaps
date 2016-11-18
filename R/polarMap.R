# function to plot polar plots on leaflet maps

#' Bivariate polar plots on interactive leaflet maps
#'
#' @param data A data frame. The data frame must contain the data to
#'   plot a \code{polarPlot}, which includes wind speed (\code{ws}),
#'   wind direction (\code{wd}) and the column representing the
#'   concentration of a pollutant. In addition, \code{data} must
#'   include a decimal latitude and longitude.
#' @param pollutant The column name of the pollutant to plot.
#' @param x The radial axis variable to plot.
#' @param latitude The decimal latitude.
#' @param longitude The decimal longitude.
#' @param dir_polar The location of a directory to store the polar
#'   plots that are generated. Note, if the directory does not exist
#'   it is generated. If the directory does exist all plots will be
#'   deleted when the function is run.
#' @param provider The base map to be used. See
#'   \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#'   for a lits of all base maps that can be used.
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
polarMap <- function(data, pollutant = "nox", x = "ws",
                     latitude = "lat",
                     longitude = "lon",
                     dir_polar = "~/dir_polar",
                     provider = "OpenStreetMap",
                     type = "default",
                     cols = "jet",
                     fig.width = 4, fig.height = 4) {

  . <- NULL

  ## extract variables of interest
  vars <- c("wd", x, pollutant, latitude, longitude, type)

  # check to see if variables exist in data

  if (length(intersect(vars, names(data))) != length(vars))
    stop(paste(vars[which(!vars %in% names(data))], "not found in data"), call. = FALSE)


    # check that directory is empty / exists
  if (dir.exists(dir_polar)) {
    # remove existing files
    files <- list.files(dir_polar, full.names = TRUE)
    file.remove(files)

  } else {

    dir.create(dir_polar)

  }

  # function to produce a polar plot, with transparent background
  plot_polar <- function(data, pollutant, x, ...) {

    png(paste0(dir_polar, "/", data$site[1], ".png"),
        width = fig.width * 300,
        height = fig.height * 300, res = 300, bg = "transparent")

    plt <- polarPlot(data, pollutant = pollutant, x = x,
                     key = FALSE,
                     par.settings = list(axis.line = list(col = "transparent")),
                     ...)

    dev.off()

    return(plt$data)

  }

  # go through all sites and make some plots
  group_by_(data, .dots = type) %>%
    do(plot_polar(., pollutant, cols = cols, x = x))

  # summarise data - one line per location
  plot_data <- group_by_(data, .dots = type) %>%
    slice(n = 1)


  # definition of 'icons' aka the openair plots
  leafIcons <- icons(
    iconUrl = list.files(dir_polar, full.names = TRUE),
    iconWidth = 200, iconHeight = 200
  )

  m <- leaflet(data = plot_data) %>%
    addTiles() %>%
    addProviderTiles(provider = provider) %>%
    addMarkers(data = plot_data,
               plot_data[[longitude]], plot_data[[latitude]],
               icon = leafIcons, popup = plot_data[[type]])

  print(m)

}
