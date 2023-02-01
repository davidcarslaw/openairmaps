#' Percentile roses on interactive leaflet maps
#'
#' [percentileMap()] creates a `leaflet` map using percentile roses as
#' markers. Any number of pollutants can be specified using the `pollutant`
#' argument, and multiple layers of markers can be added and toggled between
#' using `control`.
#'
#' @family interactive directional analysis maps
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
#' @seealso the original [openair::percentileRose()]
#' @seealso [percentileMapStatic()] for the static `ggmap` equivalent of
#'   [percentileMap()]
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

  # prep data
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
      d.fig = d.fig,
      popup = popup,
      label = label
    )

  # create leaflet map
  map <-
    make_leaflet_map(plots_df, latitude, longitude, provider, d.icon, popup, label, split_col, collapse.control)

  # add legend
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

  # return map
  return(map)
}
