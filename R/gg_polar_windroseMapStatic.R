#' Wind rose plots on a static ggmap
#'
#' [windroseMapStatic()] creates a `ggplot2` map using wind roses as markers. As
#' this function returns a `ggplot2` object, further customisation can be
#' achieved using functions like [ggplot2::theme()] and [ggplot2::guides()]. See
#' [openair::polarPlot()] for more information.
#'
#' @family static directional analysis maps
#'
#' @inheritParams polarMapStatic
#' @param period This determines the temporal period to consider. Options are
#'   "hour" (the default, to plot diurnal variations), "season" to plot
#'   variation throughout the year, "weekday" to plot day of the week variation
#'   and "trend" to plot the trend by wind direction.
#' @inheritDotParams openair::polarAnnulus -mydata -pollutant -period -limits
#'   -type -cols -key -plot
#'
#' @seealso [windroseMap()] for the interactive `leaflet` equivalent of
#'   [windroseMapStatic()]
#'
#' @return a `ggplot2` plot with a `ggmap` basemap
#' @export
windroseMapStatic <- function(data,
                              ws.int = 2,
                              breaks = 4,
                              period = "hour",
                              facet = NULL,
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
  latlon <- assume_latlon(data = data,
                          latitude = latitude,
                          longitude = longitude)
  latitude <- latlon$latitude
  longitude <- latlon$longitude

  # need to put ws in a separate column to work with the rest of openairmaps
  # utilities...
  data$ws_dup <- data$ws

  # prep data
  data <-
    prepMapData(
      data = data,
      pollutant = "ws_dup",
      control = facet,
      "ws",
      "wd",
      latitude,
      longitude
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
  if (!is.null(facet)) {
    data[facet] <- as.factor(data[[facet]])
    split_col <- facet
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

  # create temp directory
  tempdir <- tempdir()

  # plot and save static markers
  plots_df <-
    create_static_markers(
      fun = fun,
      data = data,
      dir = tempdir,
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
      dir = tempdir,
      latitude = latitude,
      longitude = longitude,
      split_col = split_col,
      pollutant = "ws",
      facet = facet,
      facet.nrow = facet.nrow,
      d.icon = d.icon
    )

  # sort out legend
  intervals <- attr(plots_df$plot[[1]]$data, "intervals")
  intervals <- factor(intervals, intervals)
  pal <- openair::openColours(scheme = cols, n = length(intervals)) %>%
    setNames(intervals)

  plt <-
    plt +
    ggplot2::geom_point(
      ggplot2::aes(.data[[longitude]], .data[[latitude]],
                   fill = intervals[1]),
      size = 0,
      key_glyph = ggplot2::draw_key_rect
    ) +
    ggplot2::scale_fill_manual(values = pal, drop = FALSE) +
    ggplot2::labs(fill = openair::quickText("ws"))

  # return plot
  return(plt)
}
