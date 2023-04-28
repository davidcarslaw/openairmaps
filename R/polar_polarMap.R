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
#' @param limits One of:
#' - `"fixed"` which ensures all of the markers use the same colour scale.
#' - `"free"` (the default) which allows all of the markers to use different
#' colour scales.
#' - A numeric vector in the form `c(lower, upper)` used to define the colour
#' scale. For example, `limits = c(0, 100)` would force the plot limits to
#' span 0-100.
#' @param upper One of:
#' - `"fixed"` (the default) which ensures all of the markers use the same radial axis scale.
#' - `"free"` which allows all of the markers to use different radial axis scales.
#' - A numeric value, used as the upper limit for the radial axis scale.
#' @param latitude,longitude The decimal latitude/longitude. If not provided,
#'   will be automatically inferred from data by looking for a column named
#'   "lat"/"latitude" or "lon"/"lng"/"long"/"longitude" (case-insensitively).
#' @param control Used for splitting the input data into different groups which
#'   can be selected between using a "layer control" interface, passed to the
#'   `type` argument of [openair::cutData()]. `control` cannot be used if
#'   multiple `pollutant` columns have been provided.
#' @param popup Columns to be used as the HTML content for marker popups. Popups
#'   may be useful to show information about the individual sites (e.g., site
#'   names, codes, types, etc.). If a vector of column names are provided they
#'   are passed to [buildPopup()] using its default values.
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
#' @param type `r lifecycle::badge("deprecated")`. Different sites are now
#'   automatically detected based on latitude and longitude. Please use `label`
#'   and/or `popup` to label different sites.
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
                     limits = "free",
                     upper = "fixed",
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
                     type = deprecated(),
                     ...) {
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_soft(
      when = "0.5.0",
      what = "openairmaps::polarMap(type)",
      details = c("Different sites are now automatically detected based on latitude and longitude",
                  "Please use the `popup` argument to create popups.")
    )
  }

  # assume lat/lon
  latlon <- assume_latlon(
    data = data,
    latitude = latitude,
    longitude = longitude
  )
  latitude <- latlon$latitude
  longitude <- latlon$longitude

  # auto limits
  limits <- check_multipoll(limits, pollutant)

  if ("fixed" %in% limits) {
    data <-
      dplyr::mutate(data, latlng = paste(.data[[latitude]], .data[[longitude]]))

    type <- control
    if (is.null(control)) {
      type <- "default"
    }

    testplots <-
      openair::polarPlot(
        data,
        pollutant = pollutant,
        x = x,
        type = c("latlng", type),
        plot = FALSE,
        ...
      )$data

    theLimits <- range(testplots$z, na.rm = TRUE)
  } else if ("free" %in% limits) {
    theLimits <- NA
  } else if (is.numeric(limits)){
    theLimits <- limits
  } else {
    cli::cli_abort(
      c("!" = "Do not recognise {.field limits} value of {.code {limits}}",
        "i" = "{.field limits} should be one of {.code 'fixed'}, {.code 'free'} or a numeric vector of length 2.")
    )
  }

  # cut data
  data <- quick_cutdata(data = data, type = control)

  # deal with upper
  if (upper == "fixed") {
    upper <- max(data[[x]], na.rm = TRUE)
  }

  # deal with popups
  if (length(popup) > 1) {
    data <-
      quick_popup(
        data = data,
        popup = popup,
        latitude = latitude,
        longitude = longitude,
        control = control
      )
    popup <- "popup"
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
    if (upper == "free") {
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
    } else {
      openair::polarPlot(
        data,
        pollutant = "conc",
        x = x,
        plot = FALSE,
        limits = theLimits,
        upper = upper,
        cols = cols,
        alpha = alpha,
        key = key,
        ...,
        par.settings = list(axis.line = list(col = "transparent"))
      )$plot
    }
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
  if (!all(is.na(theLimits)) & draw.legend) {
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
#' @param facet Used for splitting the input data into different panels, passed
#'   to the `type` argument of [openair::cutData()]. `facet` cannot be used if
#'   multiple `pollutant` columns have been provided.
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
                           limits = "free",
                           upper = "fixed",
                           latitude = NULL,
                           longitude = NULL,
                           facet = NULL,
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

  # auto limits
  limits <- check_multipoll(limits, pollutant)

  if ("fixed" %in% limits) {
    data <-
      dplyr::mutate(data, latlng = paste(.data[[latitude]], .data[[longitude]]))

    type <- facet
    if (is.null(facet)) {
      type <- "default"
    }

    testplots <-
      openair::polarPlot(
        data,
        pollutant = pollutant,
        x = x,
        type = c("latlng", type),
        plot = FALSE,
        ...
      )$data

    theLimits <- range(testplots$z, na.rm = TRUE)
  } else if ("free" %in% limits) {
    theLimits <- NA
  } else if (is.numeric(limits)){
    theLimits <- limits
  } else {
    cli::cli_abort(
      c("!" = "Do not recognise {.field limits} value of {.code {limits}}",
        "i" = "{.field limits} should be one of {.code 'fixed'}, {.code 'free'} or a numeric vector of length 2.")
    )
  }

  # cut data
  data <- quick_cutdata(data = data, type = facet)

  # deal with upper
  if (upper == "fixed") {
    upper <- max(data[[x]], na.rm = TRUE)
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
    if (upper == "free") {
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
    } else {
      openair::polarPlot(
        data,
        pollutant = "conc",
        x = x,
        plot = FALSE,
        limits = theLimits,
        upper = upper,
        cols = cols,
        alpha = alpha,
        key = key,
        ...,
        par.settings = list(axis.line = list(col = "transparent"))
      )$plot
    }
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
  if (!all(is.na(theLimits))) {
    plt <-
      plt +
      ggplot2::geom_point(data = plots_df,
                          ggplot2::aes(.data[[longitude]], .data[[latitude]], color = 0),
                          alpha = 0) +
      ggplot2::scale_color_gradientn(
        limits = theLimits,
        colours = openair::openColours(scheme = cols)
      ) +
      ggplot2::labs(color = openair::quickText(paste(pollutant, collapse = ", ")))
  }

  # return plot
  return(plt)
}
