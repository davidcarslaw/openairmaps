#' Polar frequency plots on interactive leaflet maps
#'
#' [freqMap()] creates a `leaflet` map using binned polar plots as markers. Any
#' number of pollutants can be specified using the `pollutant` argument, and
#' multiple layers of markers can be added and toggled between using `control`.
#'
#' @family interactive directional analysis maps
#'
#' @inheritParams polarMap
#' @param statistic The statistic that should be applied to each wind
#'   speed/direction bin. Can be "frequency", "mean", "median", "max" (maximum),
#'   "stdev" (standard deviation) or "weighted.mean". The option "frequency" is
#'   the simplest and plots the frequency of wind speed/direction in different
#'   bins. The scale therefore shows the counts in each bin. The option "mean"
#'   (the default) will plot the mean concentration of a pollutant (see next
#'   point) in wind speed/direction bins, and so on.  Finally, "weighted.mean"
#'   will plot the concentration of a pollutant weighted by wind
#'   speed/direction. Each segment therefore provides the percentage overall
#'   contribution to the total concentration. Note that for options other than
#'   "frequency", it is necessary to also provide the name of a pollutant. See
#'   function [openair::cutData()] for further details.
#' @param breaks The user can provide their own scale. breaks expects a sequence
#'   of numbers that define the range of the scale. The sequence could represent
#'   one with equal spacing, e.g., `breaks = seq(0, 100, 10)` - a scale from
#'   0-10 in intervals of 10, or a more flexible sequence, e.g., `breaks = c(0,
#'   1, 5, 7, 10)`, which may be useful for some situations.
#' @param draw.legend When `breaks` are specified, should a shared legend be
#'   created at the side of the map? Default is `TRUE`.
#' @inheritDotParams openair::polarFreq -mydata -pollutant -statistic -breaks
#'   -type -cols -key -plot
#' @return A leaflet object.
#' @export
#'
#' @seealso the original [openair::polarFreq()]
#' @seealso [freqMapStatic()] for the static `ggmap` equivalent of
#'   [freqMap()]
#'
#' @examples
#' \dontrun{
#' freqMap(polar_data,
#'   pollutant = "nox",
#'   statistic = "mean",
#'   provider = "Stamen.Toner"
#' )
#' }
freqMap <- function(data,
                    pollutant = NULL,
                    breaks = NULL,
                    statistic = "mean",
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

  # allow no pollutant when statistic = "frequency"
  if (statistic == "frequency") {
    data$dummy <- "freq"
    pollutant <- "dummy"
  }

  # prep data
  data <-
    prepMapData(
      data = data,
      type = type,
      pollutant = pollutant,
      control = control,
      "wd",
      "ws",
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
    if (!is.null(breaks)) {
      openair::polarFreq(
        data,
        pollutant = "conc",
        breaks = breaks,
        plot = FALSE,
        statistic = statistic,
        cols = cols,
        alpha = alpha,
        key = key,
        ...,
        par.settings = list(axis.line = list(col = "transparent"))
      )$plot
    } else {
      openair::polarFreq(
        data,
        pollutant = "conc",
        statistic = statistic,
        plot = FALSE,
        cols = cols,
        alpha = alpha,
        key = key,
        ...,
        par.settings = list(axis.line = list(col = "transparent"))
      )$plot
    }
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
      d.fig = d.fig,
      popup = popup,
      label = label
    )

  # create leaflet map
  map <-
    make_leaflet_map(plots_df, latitude, longitude, provider, d.icon, popup, label, split_col, collapse.control)

  # add legends if breaks are set
  if (!is.null(breaks) & draw.legend) {
    if (statistic == "frequency") {
      title <- "Frequency"
    } else {
      title <- quickTextHTML(paste(pollutant, collapse = ", "))
    }
    map <-
      leaflet::addLegend(
        map,
        pal = leaflet::colorBin(
          palette = openair::openColours(scheme = cols),
          domain = breaks, bins = breaks
        ),
        values = breaks,
        title = title
      )
  }

  map
}

#' Polar frequency plots on a static ggmap
#'
#' [freqMapStatic()] creates a `ggplot2` map using polar frequency plots as
#' markers. As this function returns a `ggplot2` object, further customisation
#' can be achieved using functions like [ggplot2::theme()] and
#' [ggplot2::guides()].
#'
#' @inheritSection polarMapStatic Further customisation using ggplot2
#'
#' @family static directional analysis maps
#'
#' @inheritParams polarMapStatic
#' @param statistic The statistic that should be applied to each wind
#'   speed/direction bin. Can be "frequency", "mean", "median", "max" (maximum),
#'   "stdev" (standard deviation) or "weighted.mean". The option "frequency" is
#'   the simplest and plots the frequency of wind speed/direction in different
#'   bins. The scale therefore shows the counts in each bin. The option "mean"
#'   (the default) will plot the mean concentration of a pollutant (see next
#'   point) in wind speed/direction bins, and so on.  Finally, "weighted.mean"
#'   will plot the concentration of a pollutant weighted by wind
#'   speed/direction. Each segment therefore provides the percentage overall
#'   contribution to the total concentration. Note that for options other than
#'   "frequency", it is necessary to also provide the name of a pollutant. See
#'   function [openair::cutData()] for further details.
#' @param breaks The user can provide their own scale. breaks expects a sequence
#'   of numbers that define the range of the scale. The sequence could represent
#'   one with equal spacing, e.g., `breaks = seq(0, 100, 10)` - a scale from
#'   0-10 in intervals of 10, or a more flexible sequence, e.g., `breaks = c(0,
#'   1, 5, 7, 10)`, which may be useful for some situations.
#' @inheritDotParams openair::polarFreq -mydata -pollutant -statistic -breaks
#'   -type -cols -key -plot
#'
#' @seealso the original [openair::polarFreq()]
#' @seealso [freqMap()] for the interactive `leaflet` equivalent of
#'   [freqMapStatic()]
#'
#' @return a `ggplot2` plot with a `ggmap` basemap
#' @export
freqMapStatic <- function(data,
                          pollutant = NULL,
                          breaks = NULL,
                          statistic = "mean",
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

  # allow no pollutant when statistic = "frequency"
  if (statistic == "frequency") {
    data$dummy <- "freq"
    lab <- "frequency"
    pollutant <- "dummy"
  } else {
    lab <- pollutant
  }

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
    if (!is.null(breaks)) {
      openair::polarFreq(
        data,
        pollutant = "conc",
        breaks = breaks,
        plot = FALSE,
        statistic = statistic,
        cols = cols,
        alpha = alpha,
        key = key,
        ...,
        par.settings = list(axis.line = list(col = "transparent"))
      )$plot
    } else {
      openair::polarFreq(
        data,
        pollutant = "conc",
        statistic = statistic,
        plot = FALSE,
        cols = cols,
        alpha = alpha,
        key = key,
        ...,
        par.settings = list(axis.line = list(col = "transparent"))
      )$plot
    }
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
    intervals <- stringr::str_c(breaks, dplyr::lead(breaks), sep = " - ")
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
      ggplot2::labs(fill = openair::quickText(paste(lab, collapse = ", ")))
  }

  # return plot
  return(plt)
}
