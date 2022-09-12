#' Polar annulus plots on interactive leaflet maps
#'
#' @param data A data frame. The data frame must contain the data to plot a
#'   [openair::polarAnnulus()], which includes wind speed (\code{ws}), wind
#'   direction (\code{wd}), date (\code{date}), and the column representing the
#'   concentration of a pollutant. In addition, \code{data} must include a
#'   decimal latitude and longitude.
#' @param pollutant The column name(s) of the pollutant(s) to plot. If multiple
#'   pollutants are specified, they can be toggled between using a "layer
#'   control" interface.
#' @param period This determines the temporal period to consider. Options are
#'   \dQuote{hour} (the default, to plot diurnal variations), \dQuote{season} to
#'   plot variation throughout the year, \dQuote{weekday} to plot day of the
#'   week variation and \dQuote{trend} to plot the trend by wind direction.
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
#' @param ... Other arguments for [openair::polarAnnulus()].
#' @return A leaflet object.
#' @import leaflet
#' @importFrom grDevices dev.off png
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' \dontrun{
#' annulusMap(polar_data,
#'   period = "hour",
#'   type = "site",
#'   provider = "Stamen.Toner"
#' )
#' }
annulusMap <- function(data,
                       pollutant = "nox",
                       period = "hour",
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

  # prepare data for mapping
  data <-
    prepMapData(
      data = data,
      type = type,
      "wd",
      "date",
      pollutant,
      latitude,
      longitude
    )

  # define plotting function
  args <- list(...)
  fun <- function(...) {
    rlang::exec(openair::polarAnnulus, period = period, !!!args, ...)
  }

  # create icons
  icons <-
    purrr::map(
      .x = sort(pollutant),
      .f = ~ create_icons(
        data = data, fun = fun, pollutant = .x,
        type = type, period = period, cols = cols, alpha = alpha, key = key,
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
