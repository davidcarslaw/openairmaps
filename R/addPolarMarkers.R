#' Add polar markers to leaflet map
#'
#' This function is similar (but not identical to) the [leaflet::addMarkers()]
#' and [leaflet::addCircleMarkers()] functions in `leaflet`, which allows users
#' to add `openair` directional analysis plots to any leaflet map and have more
#' control over groups and layerIds than in "all-in-one" functions like
#' [polarMap()].
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
#' @param popup A column of `data` to be used as a popup.
#' @param label A column of `data` to be used as a label.
#' @param data A data frame. The data frame must contain the data to plot your
#'   choice of openair directional analysis plot, which includes wind speed
#'   (`ws`), wind direction (`wd`), and the column representing the
#'   concentration of a pollutant. In addition, `data` must include a decimal
#'   latitude and longitude.
#' @param fun An `openair` directional analysis plotting function. Supported
#'   functions include [openair::polarPlot()] (the default),
#'   [openair::polarAnnulus()], [openair::polarFreq()],
#'   [openair::percentileRose()], [openair::pollutionRose()] and
#'   [openair::windRose()].
#' @param pollutant The name of the pollutant to be plot. Note that, if `fun =
#'   openair::windRose`, you must set `pollutant = "ws"`.
#' @param iconWidth The actual width of the plot on the map in pixels.
#' @param iconHeight The actual height of the plot on the map in pixels.
#' @param fig.width The width of the plots to be produced in inches.
#' @param fig.height The height of the plots to be produced in inches.
#' @param ... Other arguments for the plotting function (e.g. `period` for
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
#'     fun = windRose,
#'     group = "Wind Rose"
#'   ) %>%
#'   addPolarMarkers(
#'     data = polar_data,
#'     lat = "latitude",
#'     lng = "longitude",
#'     pollutant = "nox",
#'     group = "Polar Plot"
#'   ) %>%
#'   addLayersControl(
#'     baseGroups = c("Wind Rose", "Polar Plot")
#'   )
#' }
#'
addPolarMarkers <-
  function(map,
           lng = NULL,
           lat = NULL,
           layerId = NULL,
           group = NULL,
           popup = NULL,
           label = NULL,
           data,
           fun = openair::polarPlot,
           pollutant,
           iconWidth = 200,
           iconHeight = 200,
           fig.width = 3.5,
           fig.height = 3.5,
           ...) {
    # guess lat/lon
    latlon <- assume_latlon(
      data = data,
      latitude = lat,
      longitude = lng
    )
    lat <- latlon$latitude
    lng <- latlon$longitude

    # define plotting function
    args <- list(...)
    thefun <- function(...) {
      rlang::exec(fun, !!!args, ..., annotate = FALSE)
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
          filename = paste0(dir, "/", data[[lat]][1], data[[lng]][1], "_", pollutant, ".png"),
          width = fig.width * 300,
          height = fig.height * 300,
          res = 300,
          bg = "transparent"
        )

        plt <- fun(
          data,
          key = FALSE,
          pollutant = pollutant,
          par.settings = list(axis.line = list(col = "transparent"))
        )

        grDevices::dev.off()
      }

    # go through all sites and make some plot
    data %>%
      dplyr::group_split(.data[[lat]], .data[[lng]]) %>%
      purrr::walk(
        .f = ~ save_icon(
          data = .x,
          fun = thefun,
          dir = icon_dir,
          pollutant = pollutant,
          fig.width = fig.width,
          fig.height = fig.height
        )
      )

    # definition of 'icons' aka the openair plots
    leafIcons <-
      lapply(
        sort(paste0(
          icon_dir, "/", unique(data[[lat]]), unique(data[[lng]]), "_", pollutant, ".png"
        )),
        leaflet::makeIcon,
        iconWidth = iconWidth,
        iconHeight = iconHeight
      )

    names(leafIcons) <- paste0(unique(data[[lat]]), unique(data[[lng]]))
    class(leafIcons) <- "leaflet_icon_set"

    plot_data <-
      dplyr::group_by(data, .data[[lat]], .data[[lng]]) %>%
      dplyr::slice(n = 1) %>%
      dplyr::arrange(.data[[lat]], .data[[lng]])

    if (!is.null(label)) label <- plot_data[[label]]
    if (!is.null(popup)) popup <- plot_data[[popup]]

    map <- leaflet::addMarkers(
      map,
      data = plot_data,
      lng = plot_data[[lng]],
      lat = plot_data[[lat]],
      icon = leafIcons,
      popup = popup,
      label = label,
      group = group,
      layerId = layerId
    )

    map
  }
