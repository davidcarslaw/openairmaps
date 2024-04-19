#' Bivariate polar plots on dynamic and static maps
#'
#' The [polarMap()] function creates a map using bivariate polar plots as
#' markers. Any number of pollutants can be specified using the `pollutant`
#' argument, and multiple layers of markers can be created using `type`. By
#' default, these maps are dynamic and can be panned, zoomed, and otherwise
#' interacted with. Using the `static` argument allows for static images to be
#' produced instead.
#'
#' @section Customisation of static maps using ggplot2:
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
#' @family directional analysis maps
#'
#' @param data *Input data table with pollutant, wind, and geo-spatial
#'   information.*
#'
#'   **required** | *scope:* dynamic & static
#'
#'   A data frame. The data frame must contain the data to plot the directional
#'   analysis marker, which includes wind speed (`ws`), wind direction (`wd`),
#'   and the column representing the concentration of a pollutant. In addition,
#'   `data` must include a decimal latitude and longitude (or X/Y coordinate
#'   used in conjunction with `crs`).
#'
#' @param pollutant *Pollutant name(s).*
#'
#'   **required** | *scope:* dynamic & static
#'
#'   The column name(s) of the pollutant(s) to plot. If multiple pollutants are
#'   specified the `type` argument will no longer be able to be used, and:
#'
#'   - *Dynamic*: The pollutants can be toggled between using a "layer control" menu.
#'
#'   - *Static:*: The pollutants will each appear in a different panel.
#'
#'   Multiple `pollutants` prohibit the use of the `type` argument.
#'
#' @param x *The radial axis variable.*
#'
#'   *default:* `"ws"` | *scope:* dynamic & static
#'
#'   The column name for the radial axis variable to use in
#'   [openair::polarPlot()]. Defaults to using wind speed, `"ws"`, but other
#'   meteorological variables such as ambient temperature or atmospheric
#'   stability may be useful.
#'
#' @param limits *Specifier for the plot colour scale bounds.*
#'
#'   *default:* `"free"` | *scope:* dynamic & static
#'
#'   One of:
#'  - `"fixed"` which ensures all of the markers use the same colour scale.
#'  - `"free"` (the default) which allows all of the markers to use different
#'   colour scales.
#'  - A numeric vector in the form `c(lower, upper)` used to define the colour
#'   scale. For example, `limits = c(0, 100)` would force the plot limits to
#'   span 0-100.
#'
#' @param upper *Specifier for the polar plot radial axis upper boundary.*
#'
#'  *default:* `"fixed"` | *scope:* dynamic & static
#'
#'   One of:
#'  - `"fixed"` (the default) which ensures all of the markers use the same radial axis scale.
#'  - `"free"` which allows all of the markers to use different radial axis scales.
#'  - A numeric value, used as the upper limit for the radial axis scale.
#'
#' @param latitude,longitude *The decimal latitude(Y)/longitude(X).*
#'
#'  *default:* `NULL` | *scope:* dynamic & static
#'
#'   Column names representing the decimal latitude and longitude (or other Y/X
#'   coordinate if using a different `crs`). If not provided, will be
#'   automatically inferred from data by looking for a column named
#'   "lat"/"latitude" or "lon"/"lng"/"long"/"longitude" (case-insensitively).
#'
#' @param crs *The coordinate reference system (CRS).*
#'
#'  *default:* `4326` | *scope:* dynamic & static
#'
#'   The coordinate reference system (CRS) of the data, passed to
#'   [sf::st_crs()]. By default this is [EPSG:4326](https://epsg.io/4326), the
#'   CRS associated with the commonly used latitude and longitude coordinates.
#'   Different coordinate systems can be specified using `crs` (e.g., `crs =
#'   27700` for the [British National Grid](https://epsg.io/27700)). Note that
#'   non-lat/lng coordinate systems will be re-projected to EPSG:4326 for
#'   plotting on the map.
#'
#' @param type *A method to condition the `data` for separate plotting.*
#'
#'  *default:* `NULL` | *scope:* dynamic & static
#'
#'   Used for splitting the input data into different groups, passed to the
#'   `type` argument of [openair::cutData()]. When `type` is specified:
#'
#'   - *Dynamic*: The different data splits can be toggled between using a "layer control" menu.
#'
#'   - *Static:*: The data splits will each appear in a different panel.
#'
#'   `type` cannot be used if multiple `pollutant` columns have been provided.
#'
#' @param popup *Content for marker popups on dynamic maps.*
#'
#'  *default:* `NULL` | *scope:* dynamic
#'
#'   Columns to be used as the HTML content for marker popups on dynamic maps.
#'   Popups may be useful to show information about the individual sites (e.g.,
#'   site names, codes, types, etc.). If a vector of column names are provided
#'   they are passed to [buildPopup()] using its default values.
#'
#' @param label *Content for marker hover-over on dynamic maps.*
#'
#'  *default:* `NULL` | *scope:* dynamic
#'
#'   Column to be used as the HTML content for hover-over labels. Labels are
#'   useful for the same reasons as popups, though are typically shorter.
#'
#' @param provider *The basemap(s) to be used.*
#'
#'  *default:* `NULL` | *scope:* dynamic & static
#'
#'   The base map(s) to be used beneath the polar markers. If not provided, will
#'   default to OpenStreetMap for both dynamic and static maps.
#'
#'   - *Dynamic*: Any number of [leaflet::providers].
#'   See <http://leaflet-extras.github.io/leaflet-providers/preview/> for a list
#'   of all base maps that can be used. If multiple base maps are provided, they
#'   can be toggled between using a "layer control" interface. By default, the
#'   interface will use the provider names as labels, but users can define their
#'   own using a named vector (e.g., `c("Default" = "OpenStreetMap", "Satellite"
#'   = "Esri.WorldImagery")`)
#'
#'  - *Static*: One of [rosm::osm.types()].
#'
#' @param cols *Colours to use for plotting.*
#'
#'  *default:* `"turbo"` | *scope:* dynamic & static
#'
#'   The colours used for plotting, passed to [openair::openColours()]. The
#'   default, `"turbo"`, is a rainbow palette with relatively perceptually
#'   uniform colours. Read more about this palette at
#'   <https://research.google/blog/turbo-an-improved-rainbow-colormap-for-visualization/>.
#'
#' @param alpha *Transparency value for polar markers.*
#'
#'  *default:* `1` | *scope:* dynamic & static
#'
#'   A value between 0 (fully transparent) and 1 (fully opaque).
#'
#' @param key *Transparency value for polar markers.*
#'
#'  *default:* `FALSE` | *scope:* dynamic & static
#'
#'   Draw a key for each individual marker? Potentially useful when `limits =
#'   "free"`, but of limited use otherwise.
#'
#' @param legend *Draw a shared legend?*
#'
#'  *default:* `TRUE` | *scope:* dynamic & static
#'
#'   When all markers share the same colour scale (e.g., when `limits != "free"`
#'   in [polarMap()]), should a shared legend be created at the side of the map?
#'
#' @param legend.position *Position of the shared legend.*
#'
#'  *default:* `NULL` | *scope:* dynamic & static
#'
#'   When `legend = TRUE`, where should the legend be placed? Defaults to
#'   "topleft"
#'
#'   - *Dynamic*: One of "topright", "topright", "bottomleft" or "bottomright". Passed to the `position` argument of [leaflet::addLegend()].
#'
#'   - *Static:*: One of "top", "right", "bottom" or "left". Passed to the `legend.position` argument of [ggplot2::theme()].
#'
#' @param legend.title *Title of the legend.*
#'
#'   *default:* `NULL` | *scope:* dynamic & static
#'
#'   By default, when `legend.title = NULL`, the function will attempt to
#'   provide a sensible legend title. `legent.title` allows users to overwrite
#'   this - for example, to include units or other contextual information. For
#'   *dynamic* maps, users may wish to use HTML tags to format the title.
#'
#' @param legend.title.autotext *Automatically format the title of the legend?*
#'
#'   *default:* `TRUE` | *scope:* dynamic & static
#'
#'   When `legend.title.autotext = TRUE`, `legend.title` will be first run
#'   through [quickTextHTML()] (*dynamic*) or [openair::quickText()] (*static*).
#'
#' @param control.collapsed *Show the layer control as a collapsed?*
#'
#'  *default:* `FALSE` | *scope:* dynamic
#'
#'   For *dynamic* maps, should the "layer control" interface be collapsed? If
#'   `TRUE`, users will have to hover over an icon to view the options.
#'
#' @param control.position *Position of the layer control menu*
#'
#'  *default:* `"topright"` | *scope:* dynamic
#'
#'   When `type != NULL`, or multiple pollutants are specified, where should the
#'   legend be placed? One of "topleft", "topright", "bottomleft" or
#'   "bottomright". Passed to the `position` argument of
#'   [leaflet::addLayersControl()].
#'
#' @param d.icon *The diameter of the plot on the map in pixels.*
#'
#'  *default:* `200` | *scope:* dynamic & static
#'
#'   This will affect the size of the individual polar markers. Alternatively, a
#'   vector in the form `c(width, height)` can be provided if a non-circular
#'   marker is desired.
#'
#' @param d.fig *The diameter of the plots to be produced using `{openair}` in
#'   inches.*
#'
#'  *default:* `3.5` | *scope:* dynamic & static
#'
#'   This will affect the resolution of the markers on the map. Alternatively, a
#'   vector in the form `c(width, height)` can be provided if a non-circular
#'   marker is desired.
#'
#' @param static *Produce a static map?*
#'
#'  *default:* `FALSE`
#'
#'   This controls whether a *dynamic* or *static* map is produced. The former
#'   is the default and is broadly more useful, but the latter may be preferable
#'   for DOCX or PDF outputs (e.g., academic papers).
#'
#' @param static.nrow *Number of rows in a static map.*
#'
#'  *default:* `NULL` | *scope:* static
#'
#'   Controls the number of rows of panels on a static map when multiple
#'   `pollutant`s or `type` are specified; passed to the `nrow` argument of
#'   [ggplot2::facet_wrap()]. The default, `NULL`, results in a roughly square
#'   grid of panels.
#'
#' @inheritDotParams openair::polarPlot -mydata -pollutant -x -limits -type
#'   -cols -key -alpha -plot
#'
#' @returns Either:
#'
#'  - *Dynamic:* A leaflet object
#'  - *Static:* A `ggplot2` object using [ggplot2::coord_sf()] coordinates with a `ggspatial` basemap
#'
#' @export
#'
#' @seealso [openair::polarPlot()]
#'
#' @examples
#' \dontrun{
#' polarMap(polar_data,
#'   pollutant = "nox",
#'   x = "ws",
#'   provider = "CartoDB.Voyager"
#' )
#' }
polarMap <- function(data,
                     pollutant = NULL,
                     x = "ws",
                     limits = "free",
                     upper = "fixed",
                     latitude = NULL,
                     longitude = NULL,
                     crs = 4326,
                     type = NULL,
                     popup = NULL,
                     label = NULL,
                     provider = NULL,
                     cols = "turbo",
                     alpha = 1,
                     key = FALSE,
                     legend = TRUE,
                     legend.position = NULL,
                     legend.title = NULL,
                     legend.title.autotext = TRUE,
                     control.collapsed = FALSE,
                     control.position = "topright",
                     d.icon = 200,
                     d.fig = 3.5,
                     static = FALSE,
                     static.nrow = NULL,
                     ...) {
  # check basemap providers are valid
  provider <- check_providers(provider, static)
  legend.position <- check_leafposition(legend.position, static)

  # check for old facet/control opts
  type <- type %||% check_facet_control(...)

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

    type_fixed <- type
    if (is.null(type)) {
      type_fixed <- "default"
    }

    testplots <-
      openair::polarPlot(
        data,
        pollutant = pollutant,
        x = x,
        type = c("latlng", type_fixed),
        plot = FALSE,
        ...
      )$data

    theLimits <- range(testplots$z, na.rm = TRUE)
  } else if ("free" %in% limits) {
    theLimits <- NA
  } else if (is.numeric(limits)) {
    theLimits <- limits
  } else {
    cli::cli_abort(
      c(
        "!" = "Do not recognise {.field limits} value of {.code {limits}}",
        "i" = "{.field limits} should be one of {.code 'fixed'}, {.code 'free'} or a numeric vector of length 2."
      )
    )
  }

  # cut data
  data <- quick_cutdata(data = data, type = type)

  # deal with upper
  if (any(upper == "fixed")) {
    upper <- max(data[[x]], na.rm = TRUE)
  } else if (any(upper == "free")) {
    upper <- NA
  }

  # deal with popups
  if (length(popup) > 1) {
    data <-
      quick_popup(
        data = data,
        popup = popup,
        latitude = latitude,
        longitude = longitude,
        control = type
      )
    popup <- "popup"
  }

  # prep data
  data <-
    prepMapData(
      data = data,
      pollutant = pollutant,
      control = type,
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
  } else if (!is.null(type)) {
    data[type] <- as.factor(data[[type]])
    split_col <- type
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
      upper = upper,
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

  if (!static) {
    # create leaflet map
    map <-
      make_leaflet_map(
        plots_df,
        latitude,
        longitude,
        crs,
        provider,
        d.icon,
        popup,
        label,
        split_col,
        control.collapsed,
        control.position
      )

    # add legend if limits are set
    if (!all(is.na(theLimits)) & legend) {
      if (legend.title.autotext) {
        textfun <- quickTextHTML
      } else {
        textfun <- function(x) {
          return(x)
        }
      }

      legend.title <-
        legend.title %||% paste(pollutant, collapse = ",<br>")
      legend.title <- textfun(legend.title)

      map <-
        leaflet::addLegend(
          map,
          title = legend.title,
          pal = leaflet::colorNumeric(
            palette = openair::openColours(scheme = cols),
            domain = theLimits
          ),
          position = legend.position,
          values = theLimits
        )
    }
  }

  if (static) {
    map <-
      create_static_map(
        plots_df = plots_df,
        latitude = latitude,
        longitude = longitude,
        split_col = split_col,
        pollutant = pollutant,
        facet = type,
        facet.nrow = static.nrow,
        d.icon = d.icon,
        crs = crs,
        provider = provider
      )

    if (!all(is.na(theLimits)) & legend) {
      if (legend.title.autotext) {
        textfun <- openair::quickText
      } else {
        textfun <- function(x) {
          return(x)
        }
      }

      legend.title <-
        legend.title %||% paste(pollutant, collapse = ", ")
      legend.title <- textfun(legend.title)

      map <-
        map +
        ggplot2::geom_point(
          data = plots_df,
          ggplot2::aes(.data[[longitude]], .data[[latitude]], color = 0),
          alpha = 0
        ) +
        ggplot2::scale_color_gradientn(
          limits = theLimits,
          colours = openair::openColours(scheme = cols)
        ) +
        ggplot2::labs(color = legend.title) +
        ggplot2::theme(legend.position = legend.position)
    }
  }

  # return map
  return(map)
}
