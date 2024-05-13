#' Wind roses on dynamic and static maps
#'
#' The [windroseMap()] function creates a map using wind roses as markers.
#' Multiple layers of markers can be created using the `type` argument. By
#' default, these maps are dynamic and can be panned, zoomed, and otherwise
#' interacted with. Using the `static` argument allows for static images to be
#' produced instead.
#'
#' @inheritSection polarMap Customisation of static maps using ggplot2
#' @family directional analysis maps
#'
#' @inheritParams polarMap
#' @param data *Input data table with wind and geo-spatial information.*
#'
#'   **required** | *scope:* dynamic & static
#'
#'   A data frame. The data frame must contain the data to plot the directional
#'   analysis marker, which includes wind speed (`ws`) and wind direction
#'   (`wd`). In addition, `data` must include a decimal latitude and longitude
#'   (or X/Y coordinate used in conjunction with `crs`).
#'
#' @param ws.int *The wind speed interval of the colour axis.*
#'
#'  *default:* `2` | *scope:* dynamic & static
#'
#'   The wind speed interval. Default is 2 m/s but for low met masts with low
#'   mean wind speeds a value of 1 or 0.5 m/s may be better.
#'
#' @param breaks *Specifier for the number of breaks of the colour axis.*
#'
#'  *default:* `4` | *scope:* dynamic & static
#'
#'   Most commonly, the number of break points for wind speed in
#'   [openair::windRose()]. For the `ws.int` default of `2`, the default
#'   `breaks`, `4`, generates the break points 2, 4, 6, and 8. Breaks can also
#'   be used to set specific break points. For example, the argument `breaks =
#'   c(0, 1, 10, 100)`` breaks the data into segments <1, 1-10, 10-100, >100.
#'
#' @inheritDotParams openair::windRose -ws.int -breaks -mydata -plot -annotate
#'   -pollutant -type -cols -key
#' @returns Either:
#'
#'  - *Dynamic:* A leaflet object
#'  - *Static:* A `ggplot2` object using [ggplot2::coord_sf()] coordinates with a `ggspatial` basemap
#'
#' @export
#'
#' @seealso [openair::windRose()]
#'
#' @examples
#' \dontrun{
#' windroseMap(polar_data,
#'   provider = "CartoDB.Voyager"
#' )
#' }
windroseMap <- function(data,
                        ws.int = 2,
                        breaks = 4,
                        latitude = NULL,
                        longitude = NULL,
                        crs = 4326,
                        type = NULL,
                        popup = NULL,
                        label = NULL,
                        provider = "OpenStreetMap",
                        cols = "turbo",
                        alpha = 1,
                        key = FALSE,
                        legend = TRUE,
                        legend.position = NULL,
                        legend.title = NULL,
                        legend.title.autotext = TRUE,
                        control.collapsed = FALSE,
                        control.position = "topright",
                        control.autotext = TRUE,
                        d.icon = 200,
                        d.fig = 3.5,
                        static = FALSE,
                        static.nrow = NULL,
                        ...,
                        control = NULL) {
  # check basemap providers are valid
  provider <- check_providers(provider, static)
  legend.position <- check_legendposition(legend.position, static)

  # check for old facet/control opts
  type <- type %||% check_facet_control(control = control, ...)

  # assume lat/lon
  latlon <- assume_latlon(
    data = data,
    latitude = latitude,
    longitude = longitude
  )
  latitude <- latlon$latitude
  longitude <- latlon$longitude

  # need to put ws in a separate column to work with the rest of openairmaps
  # utilities...
  data$ws_dup <- data$ws

  # cut data
  data <- quick_cutdata(data = data, type = type)

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
      pollutant = "ws_dup",
      control = type,
      "ws",
      "wd",
      latitude,
      longitude,
      popup,
      label
    )

  # work out breaks
  # needs to happen before plotting to ensure same scales
  breaks <-
    getBreaks(
      breaks = breaks,
      ws.int = ws.int,
      vec = data$conc,
      polrose = FALSE
    )

  # identify splitting column (defaulting to pollutant)
  if (!is.null(type)) {
    data[type] <- as.factor(data[[type]])
    split_col <- type
  } else {
    split_col <- "pollutant_name"
  }

  # define function
  fun <- function(data) {
    openair::windRose(
      data,
      plot = FALSE,
      ws.int = ws.int,
      breaks = breaks,
      cols = cols,
      alpha = alpha,
      key = key,
      annotate = FALSE,
      ...,
      par.settings = list(axis.line = list(col = "transparent"))
    )
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

  if (static) {
    # create static map - deals with basics & facets
    map <-
      create_static_map(
        plots_df = plots_df,
        latitude = latitude,
        longitude = longitude,
        split_col = split_col,
        pollutant = "ws",
        facet = type,
        facet.nrow = static.nrow,
        d.icon = d.icon,
        crs = crs,
        provider = provider
      )

    if (legend) {
      # sort out legend
      intervals <- attr(plots_df$plot[[1]]$data, "intervals")
      intervals <- factor(intervals, intervals)
      pal <- openair::openColours(scheme = cols, n = length(intervals)) %>%
        stats::setNames(intervals)

      # create dummy df for creating legend
      dummy <-
        dplyr::distinct(plots_df, .data[[longitude]], .data[[latitude]]) %>%
        tidyr::crossing(intervals)

      legend.title <-
        create_legend_title(
          static = static,
          legend.title.autotext = legend.title.autotext,
          legend.title = legend.title,
          str = "Wind Speed"
        )

      # add legend
      map <-
        map +
        ggplot2::geom_point(
          data = dummy,
          ggplot2::aes(.data[[longitude]], .data[[latitude]],
            fill = .data[["intervals"]]
          ),
          size = 0,
          key_glyph = ggplot2::draw_key_rect
        ) +
        ggplot2::scale_fill_manual(values = pal, drop = FALSE) +
        ggplot2::labs(fill = legend.title) +
        ggplot2::theme(legend.position = legend.position)
    }

    return(map)
  }

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
        control.position,
        control.autotext
      )

    # add legend
    if (legend) {
      legend.title <-
        create_legend_title(
          static = static,
          legend.title.autotext = legend.title.autotext,
          legend.title = legend.title,
          str = "Wind Speed"
        )

      map <-
        leaflet::addLegend(
          map,
          position = legend.position,
          pal = leaflet::colorBin(
            palette = openair::openColours(cols),
            domain = breaks,
            bins = breaks
          ),
          values = breaks,
          title = legend.title
        )
    }
  }

  return(map)
}
