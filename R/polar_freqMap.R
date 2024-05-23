#' Polar frequency plots on dynamic and static maps
#'
#' The [freqMap()] function creates a map using polar frequency plots as
#' markers. Any number of pollutants can be specified using the `pollutant`
#' argument, and multiple layers of markers can be created using `type`. By
#' default, these maps are dynamic and can be panned, zoomed, and otherwise
#' interacted with. Using the `static` argument allows for static images to be
#' produced instead.
#'
#' @inheritSection polarMap Customisation of static maps using ggplot2
#' @family directional analysis maps
#'
#' @inheritParams polarMap
#' @param statistic *The statistic that should be applied to each wind
#'  speed/direction bin.*
#'
#'  *default:* `"mean"` | *scope:* dynamic & static
#'
#'  Can be "frequency", "mean", "median", "max" (maximum), "stdev" (standard
#'  deviation) or "weighted.mean". The option "frequency" is the simplest and
#'  plots the frequency of wind speed/direction in different bins. The scale
#'  therefore shows the counts in each bin. The option "mean" (the default) will
#'  plot the mean concentration of a pollutant (see next point) in wind
#'  speed/direction bins, and so on.  Finally, "weighted.mean" will plot the
#'  concentration of a pollutant weighted by wind speed/direction. Each segment
#'  therefore provides the percentage overall contribution to the total
#'  concentration. Note that for options other than "frequency", it is necessary
#'  to also provide the name of a pollutant. See function [openair::cutData()]
#'  for further details.
#'
#' @param breaks *Specifier for the breaks of the plot colour scale.*
#'
#'   *default:* `"free"` | *scope:* dynamic & static
#'
#'   One of:
#' - `"fixed"` which ensures all of the markers use the same colour scale.
#' - `"free"` (the default) which allows all of the markers to use different
#'   colour scales.
#' - A numeric vector defining a sequence of numbers to use as the breaks. The
#'   sequence could represent one with equal spacing, e.g., `breaks = seq(0,
#'   100, 10)` - a scale from 0-10 in intervals of 10, or a more flexible
#'   sequence, e.g., `breaks = c(0, 1, 5, 7, 10)`, which may be useful for some
#'   situations.
#' @inheritDotParams openair::polarFreq -mydata -pollutant -statistic -breaks
#'   -type -cols -key -plot
#' @returns Either:
#'
#'  - *Dynamic:* A leaflet object
#'  - *Static:* A `ggplot2` object using [ggplot2::coord_sf()] coordinates with a `ggspatial` basemap
#' @export
#'
#' @seealso [openair::polarFreq()]
#'
#' @examples
#' \dontrun{
#' freqMap(polar_data,
#'   pollutant = "nox",
#'   statistic = "mean",
#'   provider = "CartoDB.Voyager"
#' )
#' }
freqMap <- function(data,
                    pollutant = NULL,
                    statistic = "mean",
                    breaks = "free",
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

  # allow no pollutant when statistic = "frequency"
  if (statistic == "frequency") {
    data$dummy <- "freq"
    lab <- "frequency"
    pollutant <- "dummy"
  } else {
    lab <- pollutant
  }

  # auto limits
  breaks <- check_multipoll(breaks, pollutant)

  if ("fixed" %in% breaks) {
    data <-
      dplyr::mutate(data, latlng = paste(.data[[latitude]], .data[[longitude]]))

    type_fixed <- type
    if (is.null(type)) {
      type_fixed <- "default"
    }

    testplots <-
      openair::polarFreq(
        data,
        pollutant = pollutant,
        statistic = statistic,
        trans = FALSE,
        type = c("latlng", type_fixed),
        plot = FALSE,
        ...
      )$data

    theBreaks <- pretty(testplots$weights, n = 10)
  } else if ("free" %in% breaks) {
    theBreaks <- NULL
  } else if (is.numeric(breaks)) {
    theBreaks <- breaks
  } else {
    cli::cli_abort(
      c(
        "!" = "Do not recognise {.field breaks} value of {.code {breaks}}",
        "i" = "{.field breaks} should be one of {.code 'fixed'}, {.code 'free'} or a numeric vector."
      )
    )
  }

  # cut data
  data <- openair::cutData(x = data, type = type %||% "default", ...)

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
    openair::polarFreq(
      data,
      pollutant = "conc",
      breaks = theBreaks,
      plot = FALSE,
      statistic = statistic,
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
    if (!all(is.na(theBreaks)) & legend) {
      intervals <-
        stringr::str_c(theBreaks, dplyr::lead(theBreaks), sep = " - ")
      intervals <- intervals[!is.na(intervals)]
      intervals <- factor(intervals, intervals)
      pal <-
        openair::openColours(scheme = cols, n = length(intervals)) %>%
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
          str = paste(lab, collapse = ", ")
        )

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

    # add legends if breaks are set
    if (!all(is.na(theBreaks)) & legend) {
      legend.title <-
        create_legend_title(
          static = static,
          legend.title.autotext = legend.title.autotext,
          legend.title = legend.title,
          str = paste(lab, collapse = ",<br>")
        )

      map <-
        leaflet::addLegend(
          map,
          position = legend.position,
          pal = leaflet::colorBin(
            palette = openair::openColours(scheme = cols),
            domain = theBreaks,
            bins = theBreaks
          ),
          values = theBreaks,
          title = legend.title
        )
    }
  }

  map
}
