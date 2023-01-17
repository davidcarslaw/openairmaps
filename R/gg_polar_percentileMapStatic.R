#' Percentile roses on a static ggmap
#'
#' [percentileMapStatic()] creates a `ggplot2` map using percentile roses as
#' markers. As this function returns a `ggplot2` object, further customisation
#' can be achieved using functions like [ggplot2::theme()] and
#' [ggplot2::guides()].
#'
#' @inheritSection polarMapStatic Further customisation using ggplot2 
#'
#' @family static directional analysis maps
#'
#' @inheritParams polarMapStatic
#' @param percentile The percentile value(s) to plot. Must be between 0–100. If
#'   `percentile = NA` then only a mean line will be shown.
#' @inheritDotParams openair::percentileRose -mydata -pollutant -percentile
#'   -type -cols -key -plot
#'
#' @seealso the original [openair::percentileRose()]
#' @seealso [percentileMap()] for the interactive `leaflet` equivalent of
#'   [percentileMapStatic()]
#'
#' @return a `ggplot2` plot with a `ggmap` basemap
#' @export
percentileMapStatic <- function(data,
                                pollutant = NULL,
                                percentile = c(25, 50, 75, 90, 95),
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
    openair::percentileRose(
      data,
      pollutant = "conc",
      percentile = percentile,
      plot = FALSE,
      cols = cols,
      alpha = alpha,
      key = key,
      ...,
      par.settings = list(axis.line = list(col = "transparent"))
    )$plot
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
  percs <- unique(c(0, percentile))
  intervals <- stringr::str_c(percs, dplyr::lead(percs), sep = " - ")
  intervals <- intervals[!is.na(intervals)]
  intervals <- factor(intervals, intervals)
  pal <- openair::openColours(scheme = cols, n = length(intervals)) %>%
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
    ggplot2::labs(fill = "percentile")

  # return plot
  return(plt)
}
