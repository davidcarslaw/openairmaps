#' Percentile roses on interactive leaflet maps
#'
#' @param data A data frame. The data frame must contain the data to plot a
#'   \code{percentileRose}, which includes wind speed (\code{ws}), wind direction
#'   (\code{wd}), and the column representing the
#'   concentration of a pollutant. In addition, \code{data} must include a
#'   decimal latitude and longitude.
#' @param pollutant The column name(s) of the pollutant(s) to plot. If multiple
#'   pollutants are specified, they can be toggled between using a "layer
#'   control" interface.
#' @param latitude The decimal latitude.
#' @param longitude The decimal longitude.
#' @param provider The base map(s) to be used. See
#'   \url{http://leaflet-extras.github.io/leaflet-providers/preview/} for a list
#'   of all base maps that can be used. If multiple base maps are provided, they
#'   can be toggled between using a "layer control" interface.
#' @param type The grouping variable that provides a data set for a specific
#'   location. Often, with several sites, \code{type = "site"} is used.
#' @param cols The colours used for plotting.
#' @param alpha The alpha transparency to use for the plotting surface (a value
#'   between 0 and 1 with zero being fully transparent and 1 fully opaque).
#' @param key Should the key of the plot be drawn. Default is \code{FALSE}.
#' @param iconWidth The actual width of the plot on the map in pixels.
#' @param iconHeight The actual height of the plot on the map in pixels.
#' @param fig.width The width of the plots to be produced in inches.
#' @param fig.height The height of the plots to be produced in inches.
#' @param ... Other arguments for \code{percentileRose}.
#' @return A leaflet object.
#' @import leaflet
#' @importFrom grDevices dev.off png
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#'
#' percentileMap(polar_data,
#'   latitude = "latitude", longitude = "longitude",
#'   x = "ws", type = "site", provider = "Stamen.Toner"
#' )
percentileMap <- function(data,
                     pollutant = "nox",
                     latitude = "lat",
                     longitude = "lon",
                     provider = "OpenStreetMap",
                     type = "default",
                     cols = "jet",
                     alpha = 1,
                     key = FALSE,
                     iconWidth = 200,
                     iconHeight = 200,
                     fig.width = 4,
                     fig.height = 4,
                     ...) {
  . <- NULL

  data <-
    prepMapData(
      data = data,
      type = type,
      "wd",
      pollutant,
      latitude,
      longitude
    )

  # define plotting function
  args <- list(...)
  fun <- function(...) {
    rlang::exec(openair::percentileRose, !!!args, ...)
  }

  # create icons
  icons <-
    purrr::map(
      .x = sort(pollutant),
      .f = ~ create_icons(
        data = data, fun = fun, pollutant = .x,
        type = type, cols = cols, alpha = alpha, key = key,
        fig.width = fig.width, fig.height = fig.height,
        iconWidth = iconWidth, iconHeight = iconHeight, ...
      )
    )

  # plot leaflet
  makeMap(
    data = data,
    icons = icons,
    provider = provider,
    longitude = longitude,
    latitude = latitude,
    type = type,
    pollutant = pollutant
  )
}
