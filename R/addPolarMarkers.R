#' Add polar markers to leaflet map
#'
#' This function is similar (but not identical to) the [leaflet::addMarkers()]
#' and [leaflet::addCircleMarkers()] functions in `leaflet`, which allows users
#' to add `openair` directional analysis plots to any leaflet map and have more
#' control over groups and layerIds than in "all-in-one" functions like
#' [polarMap()].
#'
#' @inheritParams leaflet::addMarkers
#' @param lng The decimal longitude.
#' @param lat The decimal latitude.
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
#' @param key Should a key for each marker be drawn? Default is `FALSE`.
#' @param d.icon The diameter of the plot on the map in pixels. This will affect
#'   the size of the individual polar markers. Alternatively, a vector in the
#'   form `c(width, height)` can be provided if a non-circular marker is
#'   desired.
#' @param d.fig The diameter of the plots to be produced using `openair` in
#'   inches. This will affect the resolution of the markers on the map.
#'   Alternatively, a vector in the form `c(width, height)` can be provided if a
#'   non-circular marker is desired.
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
addPolarMarkers <-
  function(map,
           data,
           fun = openair::polarPlot,
           pollutant,
           lng = NULL,
           lat = NULL,
           layerId = NULL,
           group = NULL,
           popup = NULL,
           label = NULL,
           key = FALSE,
           d.icon = 200,
           d.fig = 3.5,
           ...) {
    # guess lat/lon
    latlon <- assume_latlon(
      data = data,
      latitude = lat,
      longitude = lng
    )
    latitude <- latlon$latitude
    longitude <- latlon$longitude

    # define plotting function
    args <- list(...)
    thefun <- function(...) {
      rlang::exec(fun,
        !!!args,
        ...,
        annotate = FALSE,
        par.settings = list(axis.line = list(col = "transparent")),
        plot = FALSE,
        key = key
      )
    }

    # create dummy split_col
    data$dummyvar <- "dummyvar"
    split_col <- "dummyvar"

    # plot and save static markers
    plots_df <-
      create_static_markers(
        fun = thefun,
        data = data,
        latitude = latitude,
        longitude = longitude,
        split_col = split_col,
        d.fig = d.fig,
        popup = popup,
        label = label,
        dropcol = pollutant
      )

    # work out width/height
    if (length(d.icon) == 1) {
      width <- height <- d.icon
    }
    if (length(d.icon) == 2) {
      width <- d.icon[[1]]
      height <- d.icon[[2]]
    }

    # get marker arguments
    marker_arg <- list(
      map = map,
      lat = plots_df[[latitude]],
      lng = plots_df[[longitude]],
      icon = leaflet::makeIcon(
        iconUrl = plots_df$url,
        iconHeight = height,
        iconWidth = width,
        iconAnchorX = width / 2,
        iconAnchorY = height / 2
      ),
      group = group,
      layerId = layerId
    )

    # deal w/ popups/labels
    if (!is.null(popup)) {
      marker_arg <- append(marker_arg, list(popup = plots_df[[popup]]))
    }
    if (!is.null(label)) {
      marker_arg <- append(marker_arg, list(label = plots_df[[label]]))
    }

    # add markers to map
    map <- rlang::exec(leaflet::addMarkers, !!!marker_arg)

    # return map
    return(map)
  }
