#' Percentile roses on interactive leaflet maps
#'
#' [percentileMap()] creates a `leaflet` map using percentile roses as
#' markers. Any number of pollutants can be specified using the `pollutant`
#' argument, and multiple layers of markers can be added and toggled between
#' using `control`. See [openair::percentileRose()] for more information.
#'
#' @family directional analysis maps
#'
#' @inheritParams polarMap
#' @param percentile The percentile value(s) to plot. Must be between 0–100. If
#'   `percentile = NA` then only a mean line will be shown.
#' @param draw.legend Should a shared legend be created at the side of the map?
#'   Default is `TRUE`.
#' @inheritDotParams openair::percentileRose -mydata -pollutant -percentile
#'   -type -cols -key -plot
#' @return A leaflet object.
#' @export
#'
#' @examples
#' \dontrun{
#' percentileMap(polar_data,
#'   pollutant = "nox",
#'   provider = "Stamen.Toner"
#' )
#' }
percentileMap <- function(data,
                          pollutant = NULL,
                          percentile = c(25, 50, 75, 90, 95),
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
                          iconWidth = 200,
                          iconHeight = 200,
                          fig.width = 3.5,
                          fig.height = 3.5,
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

  data <-
    prepMapData(
      data = data,
      type = type,
      pollutant = pollutant,
      control = control,
      "wd",
      latitude,
      longitude,
      popup,
      label
    )

  # define plotting function
  args <- list(...)
  fun <- function(...) {
    rlang::exec(openair::percentileRose, alpha = alpha, !!!args, percentile = percentile, ...)
  }

  # identify splitting column (defaulting to pollutant)
  if (length(pollutant) > 1) {
    split_col <- "pollutant_name"
  } else if (!is.null(control)) {
    data[control] <- as.factor(data[[control]])
    split_col <- control
  } else {
    split_col <- "pollutant_name"
  }

  # create icons
  icons <-
    data %>%
    dplyr::group_split(.data[[split_col]]) %>%
    rlang::set_names(levels(data[[split_col]])) %>%
    purrr::imap(
      .f = ~ create_icons(
        data = .x, fun = fun, pollutant = "conc", split = .y,
        lat = latitude, lon = longitude, x = x, cols = cols,
        key = key, fig.width = fig.width, fig.height = fig.height,
        iconWidth = iconWidth, iconHeight = iconHeight, ...
      )
    )

  # plot leaflet
  map <-
    makeMap(
      data = data,
      icons = icons,
      provider = provider,
      longitude = longitude,
      latitude = latitude,
      popup = popup,
      label = label,
      split_col = split_col,
      collapse = collapse.control
    )

  if (all(!is.na(percentile)) & draw.legend) {
    percs <- unique(c(0, percentile))
    map <-
      leaflet::addLegend(
        title = "Percentile",
        map,
        pal = leaflet::colorBin(
          palette = openair::openColours(scheme = cols, n = length(percs)),
          bins = percs,
          domain = 0:100
        ),
        values = 0:100
      )
  }

  map
}
