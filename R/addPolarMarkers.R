#' Add polar markers to leaflet map
#'
#' This function is similar (but not identical to) the [leaflet::addMarkers()]
#' and [leaflet::addCircleMarkers()] functions in \code{leaflet}, which allows
#' users to add \code{openair} directional analysis plots to any leaflet map and
#' have more control over groups and layerIds than in "all-in-one" functions
#' like [polarMap()].
#'
#' @param map a map widget object created from [leaflet::leaflet()].
#' @param lng The decimal longitude.
#' @param lat The decimal latitude.
#' @param layerId The layer id.
#' @param group the name of the group the newly created layers should belong to
#'   (for [leaflet::clearGroup()] and [leaflet::addLayersControl()] purposes).
#'   Human-friendly group names are permitted–they need not be short,
#'   identifier-style names. Any number of layers and even different types of
#'   layers (e.g. markers and polygons) can share the same group name.
#' @param popup A column of \code{data} to be used as a popup.
#' @param data A data frame. The data frame must contain the data to plot your
#'   choice of openair directional analysis plot, which includes wind speed
#'   (\code{ws}), wind direction (\code{wd}), and the column representing the
#'   concentration of a pollutant. In addition, \code{data} must include a
#'   decimal latitude and longitude.
#' @param fun An \code{openair} directional analysis plotting function.
#'   Supported functions include [openair::polarPlot()] (the default),
#'   [openair::polarAnnulus()], [openair::polarFreq()],
#'   [openair::percentileRose()], [openair::pollutionRose()] and
#'   [openair::windRose()].
#' @param pollutant The name of the pollutant to be plot. Note that, if
#'   \code{fun = openair::windRose}, you must set \code{pollutant = "ws"}.
#' @param type The grouping variable that provides a data set for a specific
#'   location. Often, with several sites, \code{type = "site"} is used.
#' @param iconWidth The actual width of the plot on the map in pixels.
#' @param iconHeight The actual height of the plot on the map in pixels.
#' @param fig.width The width of the plots to be produced in inches.
#' @param fig.height The height of the plots to be produced in inches.
#' @param ... Other arguments for the plotting function (e.g. \code{period} for
#'   [openair::polarAnnulus()]).
#' @return A leaflet object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(leaflet)
#' library(openair)
#'
#' leaflet() %>%
#'   addTiles() %>%
#'   addPolarMarkers(
#'     data = polar_data,
#'     lat = "latitude",
#'     lng = "longitude",
#'     pollutant = "ws",
#'     type = "site",
#'     fun = windRose,
#'     group = "Wind Rose"
#'   ) %>%
#'   addPolarMarkers(
#'     data = polar_data,
#'     lat = "latitude",
#'     lng = "longitude",
#'     pollutant = "nox",
#'     type = "site",
#'     group = "Polar Plot"
#'   ) %>%
#'   addLayersControl(
#'     baseGroups = c("Wind Rose", "Polar Plot")
#'   )
#' }
#'
addPolarMarkers <- function(map, lng = NULL, lat = NULL, layerId = NULL, group = NULL, popup,
                            data, fun = openair::polarPlot, pollutant, type = "default",
                            iconWidth = 200, iconHeight = 200, fig.width = 4, fig.height = 4,
                            ...) {
  if (type == "default") {
    data <- dplyr::mutate(data, type = "default")
  }

  if (missing(popup)) popup <- type

  # define plotting function
  args <- list(...)
  thefun <- function(...) {
    rlang::exec(fun, !!!args, ...)
  }

  # where to write files
  icon_dir <- tempdir()

  save_icon <-
    function(data,
             fun,
             dir,
             pollutant,
             fig.width,
             fig.height,
             ...) {
      grDevices::png(
        filename = paste0(dir, "/", data[[type]][1], "_", pollutant, ".png"),
        width = fig.width * 300,
        height = fig.height * 300,
        res = 300,
        bg = "transparent"
      )

      plt <- fun(
        data,
        key = FALSE,
        pollutant = pollutant,
        par.settings = list(axis.line = list(col = "transparent")),
        # ...
      )

      grDevices::dev.off()
    }

  # go through all sites and make some plot
  data %>%
    dplyr::group_split(dplyr::across(dplyr::all_of(type))) %>%
    purrr::walk(
      .f = ~ save_icon(
        data = .x,
        fun = thefun,
        dir = icon_dir,
        pollutant = pollutant,
        fig.width = fig.width,
        fig.height = fig.height,
        # ...
      )
    )

  # definition of 'icons' aka the openair plots
  leafIcons <-
    lapply(sort(paste0(
      icon_dir, "/", unique(data[[type]]), "_", pollutant, ".png"
    )),
    leaflet::makeIcon,
    iconWidth = iconWidth,
    iconHeight = iconHeight
    )

  names(leafIcons) <- unique(data[[type]])
  class(leafIcons) <- "leaflet_icon_set"

  plot_data <-
    dplyr::group_by(data, .data[[type]]) %>%
    dplyr::slice(n = 1) %>%
    dplyr::arrange(.data[[type]])

  map <- leaflet::addMarkers(
    map,
    data = plot_data,
    lng = plot_data[[lng]],
    lat = plot_data[[lat]],
    icon = leafIcons,
    popup = plot_data[[type]],
    group = group,
    layerId = layerId
  )

  map
}
