#' Bivariate polar plots on interactive leaflet maps
#'
#' [polarMap()] creates a `leaflet` map using bivariate polar plots as markers.
#' Any number of pollutants can be specified using the `pollutant` argument, and
#' multiple layers of markers can be added and toggled between using `control`.
#'
#' @family interactive directional analysis maps
#'
#' @param data A data frame. The data frame must contain the data to plot the
#'   directional analysis marker, which includes wind speed (`ws`), wind
#'   direction (`wd`), and the column representing the concentration of a
#'   pollutant. In addition, `data` must include a decimal latitude and
#'   longitude.
#' @param pollutant The column name(s) of the pollutant(s) to plot. If multiple
#'   pollutants are specified, they can be toggled between using a "layer
#'   control" interface.
#' @param x The radial axis variable to plot.
#' @param limits By default, each individual polar marker has its own colour
#'   scale. The `limits` argument will force all markers to use the same colour
#'   scale. The limits are set in the form `c(lower, upper)`, so `limits = c(0,
#'   100)` would force the plot limits to span 0-100.
#' @param latitude,longitude The decimal latitude/longitude. If not provided,
#'   will be automatically inferred from data by looking for a column named
#'   "lat"/"latitude" or "lon"/"lng"/"long"/"longitude" (case-insensitively).
#' @param control Column to be used for splitting the input data into different
#'   groups which can be selected between using a "layer control" interface.
#'   Appropriate columns could be those added by [openair::cutData()] or
#'   [openair::splitByDate()]. `control` cannot be used if multiple `pollutant`
#'   columns have been provided.
#' @param popup Column to be used as the HTML content for marker popups. Popups
#'   may be useful to show information about the individual sites (e.g., site
#'   names, codes, types, etc.).
#' @param label Column to be used as the HTML content for hover-over labels.
#'   Labels are useful for the same reasons as popups, though are typically
#'   shorter.
#' @param provider The base map(s) to be used. See
#'   <http://leaflet-extras.github.io/leaflet-providers/preview/> for a list of
#'   all base maps that can be used. If multiple base maps are provided, they
#'   can be toggled between using a "layer control" interface.
#' @param cols The colours used for plotting. See [openair::openColours()] for
#'   more information.
#' @param alpha The alpha transparency to use for the plotting surface (a value
#'   between 0 and 1 with zero being fully transparent and 1 fully opaque).
#' @param key Should a key for each marker be drawn? Default is `FALSE`.
#' @param draw.legend When `limits` are specified, should a shared legend be
#'   created at the side of the map? Default is `TRUE`.
#' @param collapse.control Should the "layer control" interface be collapsed?
#'   Defaults to `FALSE`.
#' @param d.icon The diameter of the plot on the map in pixels. This will affect
#'   the size of the individual polar markers. Alternatively, a vector in the
#'   form `c(width, height)` can be provided if a non-circular marker is
#'   desired.
#' @param d.fig The diameter of the plots to be produced using `openair` in
#'   inches. This will affect the resolution of the markers on the map.
#'   Alternatively, a vector in the form `c(width, height)` can be provided if a
#'   non-circular marker is desired.
#' @param type Deprecated. Please use `label` and/or `popup` to label different
#'   sites.
#' @inheritDotParams openair::polarPlot -mydata -pollutant -x -limits -type
#'   -cols -key -alpha -plot
#' @return A leaflet object.
#' @export
#'
#' @seealso the original [openair::polarPlot()]
#' @seealso [polarMapStatic()] for the static `ggmap` equivalent of [polarMap()]
#'
#' @examples
#' \dontrun{
#' polarMap(polar_data,
#'   pollutant = "nox",
#'   x = "ws",
#'   provider = "Stamen.Toner"
#' )
#' }
polarMap <- function(data,
                     pollutant = NULL,
                     x = "ws",
                     limits = NULL,
                     latitude = NULL,
                     longitude = NULL,
                     control = NULL,
                     popup = NULL,
                     label = NULL,
                     provider = "OpenStreetMap",
                     cols = "turbo",
                     alpha = 1,
                     key = FALSE,
                     draw.legend = TRUE,
                     collapse.control = FALSE,
                     d.icon = 200,
                     d.fig = 3.5,
                     type = NULL,
                     ...) {
  if (!is.null(type)) {
    cli::cli_warn(c(
      "!" = "{.code type} is deprecated. Different sites are now automatically identified.",
      "i" = "Please use {.code label} and/or {.code popup} to label sites."
    ))
  }

  # assume lat/lon
  latlon <- assume_latlon(
    data = data,
    latitude = latitude,
    longitude = longitude
  )
  latitude <- latlon$latitude
  longitude <- latlon$longitude

  # deal with limits
  theLimits <- limits
  if (is.null(limits)) {
    theLimits <- NA
  }

  # prep data
  data <-
    prepMapData(
      data = data,
      pollutant = pollutant,
      control = control,
      "wd",
      x,
      latitude,
      longitude,
      popup,
      label
    )

  # identify splitting column (defaulting to pollutant)
  if (length(pollutant) > 1) {
    split_col <- "pollutant_name"
  } else if (!is.null(control)) {
    data[control] <- as.factor(data[[control]])
    split_col <- control
  } else {
    split_col <- "pollutant_name"
  }

  # define function
  fun <- function(data) {
    openair::polarPlot(
      data,
      pollutant = "conc",
      x = x,
      plot = FALSE,
      limits = theLimits,
      cols = cols,
      alpha = alpha,
      key = key,
      ...,
      par.settings = list(axis.line = list(col = "transparent"))
    )$plot
  }

  # plot and save static markers
  plots_df <-
    create_polar_markers(
      fun = fun,
      data = data,
      latitude = latitude,
      longitude = longitude,
      split_col = split_col,
      d.fig = d.fig,
      popup = popup,
      label = label
    )

  # create leaflet map
  map <-
    make_leaflet_map(plots_df, latitude, longitude, provider, d.icon, popup, label, split_col, collapse.control)

  # add legend if limits are set
  if (!is.null(limits) & all(!is.na(limits)) & draw.legend) {
    map <-
      leaflet::addLegend(
        map,
        title = quickTextHTML(paste(pollutant, collapse = ",<br>")),
        pal = leaflet::colorNumeric(
          palette = openair::openColours(scheme = cols),
          domain = theLimits
        ),
        values = theLimits
      )
  }

  # return map
  return(map)
}

#' Bivariate polar plots on a static ggmap
#'
#' [polarMapStatic()] creates a `ggplot2` map using bivariate polar plots as
#' markers. As this function returns a `ggplot2` object, further customisation
#' can be achieved using functions like [ggplot2::theme()] and
#' [ggplot2::guides()].
#'
#' @section Further customisation using ggplot2:
#'
#'   As the outputs of the static directional analysis functions are `ggplot2`
#'   figures, further customisation is possible using functions such as
#'   [ggplot2::theme()], [ggplot2::guides()] and [ggplot2::labs()].
#'
#'   If multiple pollutants are specified, subscripting (e.g., the "x" in "NOx")
#'   is achieved using the [ggtext][ggtext::ggtext] package. Therefore if you
#'   choose to override the plot theme, it is recommended to use
#'   `[ggplot2::theme()]` and `[ggtext::element_markdown()]` to define the
#'   `strip.text` parameter.
#'
#'   When arguments like `limits`, `percentile` or `breaks` are defined, a
#'   legend is automatically added to the figure. Legends can be removed using
#'   `ggplot2::theme(legend.position = "none")`, or further customised using
#'   [ggplot2::guides()] and either `color = ggplot2::guide_colourbar()` for
#'   continuous legends or `fill = ggplot2::guide_legend()` for discrete
#'   legends.
#'
#' @family static directional analysis maps
#'
#' @inheritParams polarMap
#' @param pollutant The column name(s) of the pollutant(s) to plot. If multiple
#'   pollutants are specified, they will each form part of a separate panel.
#' @param facet Column to be used for splitting the input data into different
#'   panels. Appropriate columns could be those added by [openair::cutData()] or
#'   [openair::splitByDate()]. `facet` cannot be used if multiple `pollutant`
#'   columns have been provided.
#' @param zoom The zoom level to use for the basemap, passed to
#'   [ggmap::get_stamenmap()]. Alternatively, the `ggmap` argument can be used
#'   for more precise control of the basemap.
#' @param ggmap By default, `openairmaps` will try to estimate an appropriate
#'   bounding box for the input data and then run [ggmap::get_stamenmap()] to
#'   import a basemap. The `ggmap` argument allows users to provide their own
#'   `ggmap` object to override this, which allows for alternative bounding
#'   boxes, map types and colours.
#' @param facet.nrow Passed to the `nrow` argument of [ggplot2::facet_wrap()].
#' @inheritDotParams openair::polarPlot -mydata -pollutant -x -limits -type
#'   -cols -key -alpha -plot
#'
#' @seealso the original [openair::polarPlot()]
#' @seealso [polarMap()] for the interactive `leaflet` equivalent of
#'   [polarMapStatic()]
#'
#' @return a `ggplot2` plot with a `ggmap` basemap
#' @export
polarMapStatic <- function(data,
                           pollutant = NULL,
                           x = "ws",
                           facet = NULL,
                           limits = NULL,
                           latitude = NULL,
                           longitude = NULL,
                           zoom = 13,
                           ggmap = NULL,
                           cols = "turbo",
                           alpha = 1,
                           key = FALSE,
                           facet.nrow = NULL,
                           d.icon = 150,
                           d.fig = 3,
                           ...) {
  # assume lat/lon
  latlon <- assume_latlon(
    data = data,
    latitude = latitude,
    longitude = longitude
  )
  latitude <- latlon$latitude
  longitude <- latlon$longitude

  # deal with limits
  theLimits <- limits
  if (is.null(limits)) {
    theLimits <- NA
  }

  # prep data
  data <-
    prepMapData(
      data = data,
      pollutant = pollutant,
      control = facet,
      "wd",
      x,
      latitude,
      longitude
    )

  # identify splitting column (defaulting to pollutant)
  if (length(pollutant) > 1) {
    split_col <- "pollutant_name"
  } else if (!is.null(facet)) {
    data[facet] <- as.factor(data[[facet]])
    split_col <- facet
  } else {
    split_col <- "pollutant_name"
  }

  # define function
  fun <- function(data) {
    openair::polarPlot(
      data,
      pollutant = "conc",
      x = x,
      plot = FALSE,
      limits = theLimits,
      cols = cols,
      alpha = alpha,
      key = key,
      ...,
      par.settings = list(axis.line = list(col = "transparent"))
    )$plot
  }

  # plot and save static markers
  plots_df <-
    create_polar_markers(
      fun = fun,
      data = data,
      latitude = latitude,
      longitude = longitude,
      split_col = split_col,
      d.fig = d.fig
    )

  # load ggmap if not provided
  ggmap <-
    estimate_ggmap(
      ggmap = ggmap,
      data = plots_df,
      latitude = latitude,
      longitude = longitude,
      zoom = zoom
    )

  # create static map - deals with basics & facets
  plt <-
    create_static_map(
      ggmap = ggmap,
      plots_df = plots_df,
      latitude = latitude,
      longitude = longitude,
      split_col = split_col,
      pollutant = pollutant,
      facet = facet,
      facet.nrow = facet.nrow,
      d.icon = d.icon
    )

  # create colorbar if limits specified
  if (!is.null(limits)) {
    plt <-
      plt +
      ggplot2::geom_point(ggplot2::aes(.data[[longitude]], .data[[latitude]], color = 0),
                          alpha = 0
      ) +
      ggplot2::scale_color_gradientn(
        limits = theLimits,
        colours = openair::openColours(scheme = cols)
      ) +
      ggplot2::labs(color = openair::quickText(paste(pollutant, collapse = ", ")))
  }

  # return plot
  return(plt)
}
