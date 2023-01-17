#' Bivariate polar plots on a static ggmap
#'
#' [polarMapStatic()] creates a `ggplot2` map using bivariate polar plots as
#' markers. As this function returns a `ggplot2` object, further customisation
#' can be achieved using functions like [ggplot2::theme()] and
#' [ggplot2::guides()].
#'
#' @section Multiple Pollutants:
#'
#' If multiple pollutants are specified, subscripting (e.g., the
#' "x" in "NOx") are achieved using the [ggtext][ggtext::ggtext] package.
#' Therefore if you choose to override the plot theme, it is recommended to use
#' `[ggplot2::theme()]` and `[ggtext::element_markdown()]` to define the
#' `strip.text` parameter.
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
#' @param d.icon The diameter of the plot on the map. This will affect the size
#'   of the individual polar markers.
#' @param d.fig The width of the plots to be produced in inches. This will
#'   affect the resolution of the markers on the map.
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

  # create colorbar if limits specified
  if (!is.null(limits)) {
    plt <-
      plt +
      ggplot2::geom_point(ggplot2::aes(.data[[longitude]], .data[[latitude]], color = 0),
                          alpha = 0) +
      ggplot2::scale_color_gradientn(limits = theLimits,
                                     colours = openair::openColours(scheme = cols)) +
      ggplot2::labs(color = openair::quickText(paste(pollutant, collapse = ", ")))
  }

  # return plot
  return(plt)
}
