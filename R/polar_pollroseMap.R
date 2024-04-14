#' Pollution roses on dynamic and static maps
#'
#' The [pollroseMap()] function creates a map using pollution roses as markers.
#' Any number of pollutants can be specified using the `pollutant` argument, and
#' multiple layers of markers can be created using `type`. By default, these
#' maps are dynamic and can be panned, zoomed, and otherwise interacted with.
#' Using the `static` argument allows for static images to be produced instead.
#'
#' @inheritSection polarMap Customisation of static maps using ggplot2
#' @family directional analysis maps
#'
#' @inheritParams polarMap
#' @param statistic *The statistic to be applied to each data bin in the plot*
#'
#'  *default:* `"prop.mean"` | *scope:* dynamic & static
#'
#'   Options currently include `"prop.count"`, `"prop.mean"` and `"abs.count"`.
#'   `"prop.count"` sizes bins according to the proportion of the frequency of
#'   measurements.  Similarly, `"prop.mean"` sizes bins according to their
#'   relative contribution to the mean. `"abs.count"` provides the absolute
#'   count of measurements in each bin.
#'
#' @param breaks *Specifier for the number of breaks of the colour axis.*
#'
#'  *default:* `NULL` | *scope:* dynamic & static
#'
#'   Most commonly, the number of break points. If not specified, each marker
#'   will independently break its supplied data at approximately 6 sensible
#'   break points. When `breaks` are specified, all markers will use the same
#'   break points. Breaks can also be used to set specific break points. For
#'   example, the argument `breaks = c(0, 1, 10, 100)` breaks the data into
#'   segments <1, 1-10, 10-100, >100.
#'
#' @inheritDotParams openair::pollutionRose -breaks -mydata -pollutant -plot
#' @returns Either:
#'
#'  - *Dynamic:* A leaflet object
#'  - *Static:* A `ggplot2` object using [ggplot2::coord_sf()] coordinates with a `ggspatial` basemap
#' @export
#'
#' @seealso [openair::pollutionRose()]
#'
#' @examples
#' \dontrun{
#' pollroseMap(polar_data,
#'   pollutant = "nox",
#'   statistic = "prop.count",
#'   provider = "CartoDB.Voyager"
#' )
#' }
pollroseMap <- function(data,
                        pollutant = NULL,
                        statistic = "prop.count",
                        breaks = NULL,
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
                        draw.legend = TRUE,
                        collapse.control = FALSE,
                        d.icon = 200,
                        d.fig = 3.5,
                        static = FALSE,
                        static.nrow = NULL,
                        ...) {
  # check basemap providers are valid
  provider <- check_providers(provider, static)

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
      pollutant = pollutant,
      control = type,
      "wd",
      "ws",
      latitude,
      longitude,
      popup,
      label
    )

  # work out breaks
  # needs to happen before plotting to ensure same scales
  if (!is.null(breaks)) {
    theBreaks <-
      getBreaks(breaks = breaks, ws.int = NULL, vec = data$conc, polrose = TRUE)
  } else {
    theBreaks <- 6
  }

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
    openair::pollutionRose(
      data,
      pollutant = "conc",
      statistic = statistic,
      breaks = theBreaks,
      plot = FALSE,
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
        pollutant = pollutant,
        facet = type,
        facet.nrow = static.nrow,
        d.icon = d.icon,
        crs = crs,
        provider = provider
      )

    # create legend
    if (!is.null(breaks) & draw.legend) {
      intervals <- attr(plots_df$plot[[1]]$data, "intervals")
      intervals <- factor(intervals, intervals)
      pal <-
        openair::openColours(scheme = cols, n = length(intervals)) %>%
        stats::setNames(intervals)

      # create dummy df for creating legend
      dummy <-
        dplyr::distinct(plots_df, .data[[longitude]], .data[[latitude]]) %>%
        tidyr::crossing(intervals)

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
        ggplot2::labs(fill = openair::quickText(paste(pollutant, collapse = ", ")))
    }
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
        collapse.control
      )

    # add legend if breaks are defined
    if (!is.null(breaks) & draw.legend) {
      map <-
        leaflet::addLegend(
          map,
          pal = leaflet::colorBin(
            palette = openair::openColours(cols),
            domain = theBreaks,
            bins = theBreaks
          ),
          values = theBreaks,
          title = quickTextHTML(paste(pollutant, collapse = ", "))
        )
    }
  }

  # return map
  return(map)
}
