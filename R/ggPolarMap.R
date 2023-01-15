#' Bivariate polar plots on a static ggmap
#'
#' [ggPolarMap()] creates a `ggplot2` map using bivariate polar plots as
#' markers. As this function returns a `ggplot2` object, further customisation
#' can be achieved using functions like [ggplot2::theme()] and
#' [ggplot2::guides()]. See [openair::polarPlot()] for more information.
#'
#' ## Facet Labels If multiple pollutants are specified, subscripting (e.g., the
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
#' @seealso [polarMap()] for the interactive `leaflet` equivalent of
#'   [ggPolarMap()]
#'
#' @return a `ggplot2` plot with a `ggmap` basemap
#' @export
ggPolarMap <- function(data,
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
  latlon <- assume_latlon(data = data,
                          latitude = latitude,
                          longitude = longitude)
  latitude <- latlon$latitude
  longitude <- latlon$longitude

  # deal with limits
  theLimits <- limits
  if (is.null(limits))
    theLimits <- NA

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

  # # if no "facet", create a dummy column
  # if (is.null(facet)) {
  #   facet <- "default"
  #   data$default <- "default"
  # }

  # identify splitting column (defaulting to pollutant)
  if (length(pollutant) > 1) {
    split_col <- "pollutant_name"
  } else if (!is.null(facet)) {
    data[facet] <- as.factor(data[[facet]])
    split_col <- facet
  } else {
    split_col <- "pollutant_name"
  }

  # create plots
  plots_df <-
    data %>%
    tidyr::drop_na(.data$conc) %>%
    dplyr::nest_by(.data[[latitude]], .data[[longitude]], .data[[split_col]]) %>%
    dplyr::mutate(plot = list(
      try(silent = TRUE,
      openair::polarPlot(
        .data$data,
        pollutant = "conc",
        plot = FALSE,
        limits = theLimits,
        cols = cols,
        alpha = alpha,
        key = key,
        ...,
        par.settings = list(axis.line = list(col = "transparent"))
      )$plot
    )),
    plot = dplyr::if_else(
      inherits(plot, "try-error"),
      list(ggplot2::ggplot() + ggplot2::theme_minimal()),
      list(plot)
    ))

  dir <- tempdir()

  purrr::pwalk(list(plots_df[[latitude]], plots_df[[longitude]], plots_df[[split_col]], plots_df$plot),
               .f = ~ {
                 grDevices::png(
                   filename = paste0(dir, "/", ..1, "_", ..2, "_", ..3, ".png"),
                   width = d.fig * 300,
                   height = d.fig * 300,
                   res = 300,
                   bg = "transparent",
                   type = "cairo",
                   antialias = "none"
                 )

                 plot(..4)

                 grDevices::dev.off()
               })

  if (is.null(ggmap)) {
    lat_d <- abs(diff(range(plots_df[[latitude]])) / 2)
    minlat <- min(plots_df[[latitude]]) - lat_d
    maxlat <- max(plots_df[[latitude]]) + lat_d

    lon_d <- abs(diff(range(plots_df[[longitude]])) / 2)
    minlon <- min(plots_df[[longitude]]) - lon_d
    maxlon <- max(plots_df[[longitude]]) + lon_d

    ggmap <-
      ggmap::get_stamenmap(bbox = c(minlon, minlat, maxlon, maxlat),
                           zoom = zoom)
  }

  plt <-
    ggmap::ggmap(ggmap) +
    ggtext::geom_richtext(
      data = dplyr::mutate(
        plots_df,
        url = paste0(dir, "/", .data[[latitude]], "_", .data[[longitude]], "_", .data[[split_col]], ".png"),
        url = stringr::str_glue("<img src='{url}' width='{d.icon}'/>")
      ),
      ggplot2::aes(.data[[longitude]], .data[[latitude]], label = .data$url),
      fill = NA,
      color = NA,
      alpha = alpha
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, color = "black"))

  if (length(pollutant) > 1 | !is.null(facet)) {
    plt <-
      plt + ggplot2::facet_wrap(ggplot2::vars(quickTextHTML(.data[[split_col]])), nrow = facet.nrow) +
      ggplot2::theme(strip.text = ggtext::element_markdown())
  }

  if (!is.null(limits)) {
    plt <-
      plt  +
      ggplot2::geom_point(ggplot2::aes(.data[[longitude]], .data[[latitude]], color = 0),
                          alpha = 0) +
      ggplot2::scale_color_gradientn(limits = theLimits,
                                     colours = openair::openColours(scheme = cols)) +
      ggplot2::labs(color = openair::quickText(paste(pollutant, collapse = ", ")))
  }

  return(plt)
}
