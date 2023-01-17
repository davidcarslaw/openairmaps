#' Percentile roses on a static ggmap
#'
#' [pollroseMapStatic()] creates a `ggplot2` map using percentile roses as
#' markers. As this function returns a `ggplot2` object, further customisation
#' can be achieved using functions like [ggplot2::theme()] and
#' [ggplot2::guides()].
#'
#' @inheritSection polarMapStatic Multiple Pollutants
#'
#' @family static directional analysis maps
#'
#' @inheritParams polarMapStatic
#' @param statistic The `statistic` to be applied to each data bin in the
#'   plot. Options currently include "prop.count", "prop.mean" and
#'   "abs.count". The default "prop.count" sizes bins according to
#'   the proportion of the frequency of measurements.  Similarly,
#'   "prop.mean" sizes bins according to their relative contribution to
#'   the mean. "abs.count" provides the absolute count of measurements in
#'   each bin.
#' @param breaks Most commonly, the number of break points. If not specified,
#'   each marker will independently break its supplied data at approximately 6
#'   sensible break points. When `breaks` are specified, all markers will
#'   use the same break points. Breaks can also be used to set specific break
#'   points. For example, the argument `breaks = c(0, 1, 10, 100)` breaks
#'   the data into segments <1, 1-10, 10-100, >100.
#' @inheritDotParams openair::pollutionRose -breaks -mydata -pollutant -plot
#'
#' @seealso the original [openair::pollutionRose()]
#' @seealso [pollroseMap()] for the interactive `leaflet` equivalent of
#'   [pollroseMapStatic()]
#'
#' @return a `ggplot2` plot with a `ggmap` basemap
#' @export
pollroseMapStatic <- function(data,
                                pollutant = NULL,
                                statistic = "prop.count",
                                breaks = NULL,
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
  latlon <- assume_latlon(data = data,
                          latitude = latitude,
                          longitude = longitude)
  latitude <- latlon$latitude
  longitude <- latlon$longitude

  # prep data
  data <-
    prepMapData(
      data = data,
      pollutant = pollutant,
      control = facet,
      "wd",
      "ws",
      latitude,
      longitude
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
  } else if (!is.null(facet)) {
    data[facet] <- as.factor(data[[facet]])
    split_col <- facet
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
      pollutant = pollutant,
      facet = facet,
      facet.nrow = facet.nrow,
      d.icon = d.icon
    )

  # create legend
  if (!is.null(breaks)) {
    intervals <- attr(plots_df$plot[[1]]$data, "intervals")
    intervals <- factor(intervals, intervals)
    pal <-
      openair::openColours(scheme = cols, n = length(intervals)) %>%
      stats::setNames(intervals)

    plt <-
      plt +
      ggplot2::geom_point(
        ggplot2::aes(.data[[longitude]], .data[[latitude]],
                     fill = intervals[1]),
        size = 0,
        key_glyph = ggplot2::draw_key_rect
      ) +
      ggplot2::scale_fill_manual(values = pal, drop = FALSE) +
      ggplot2::labs(fill = openair::quickText(paste(pollutant, collapse = ", ")))
  }

  # return plot
  return(plt)
}
