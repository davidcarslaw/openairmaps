#' Percentile roses on interactive leaflet maps
#'
#' [percentileMap()] creates a \code{leaflet} map using percentile roses as
#' markers. Any number of pollutants can be specified using the \code{pollutant}
#' argument, and multiple layers of markers can be added and toggled between
#' using \code{control}. See [openair::percentileRose()] for more information.
#'
#' @seealso Directional analysis maps: [annulusMap()], [freqMap()],
#'   [percentileMap()], [polarMap()], [pollroseMap()], [windroseMap()].
#'
#' @param data A data frame. The data frame must contain the data to plot a
#'   [openair::percentileRose()], which includes wind speed (\code{ws}), wind
#'   direction (\code{wd}), and the column representing the concentration of a
#'   pollutant. In addition, \code{data} must include a decimal latitude and
#'   longitude.
#' @param pollutant The column name(s) of the pollutant(s) to plot. If multiple
#'   pollutants are specified, they can be toggled between using a "layer
#'   control" interface.
#' @param percentile The percentile value(s) to plot. Must be between 0–100. If
#'   \code{percentile = NA} then only a mean line will be shown.
#' @param latitude The decimal latitude. If not provided, latitude will be
#'   automatically inferred from data by looking for a column named \dQuote{lat}
#'   or \dQuote{latitude} (case-insensitively).
#' @param longitude The decimal longitude. If not provided, longitude will be
#'   automatically inferred from data by looking for a column named
#'   \dQuote{lon}, \dQuote{lng}, \dQuote{long}, or \dQuote{longitude}
#'   (case-insensitively).
#' @param control Column to be used for splitting the input data into different
#'   groups which can be selected between using a "layer control" interface.
#'   Appropriate columns could be those added by [openair::cutData()] or
#'   [openair::splitByDate()]. \code{control} cannot be used if multiple
#'   \code{pollutant} columns have been provided.
#' @param popup Column to be used as the HTML content for marker popups. Popups
#'   may be useful to show information about the individual sites (e.g., site
#'   names, codes, types, etc.).
#' @param label Column to be used as the HTML content for hover-over labels.
#'   Labels are useful for the same reasons as popups, though are typically
#'   shorter.
#' @param provider The base map(s) to be used. See
#'   \url{http://leaflet-extras.github.io/leaflet-providers/preview/} for a list
#'   of all base maps that can be used. If multiple base maps are provided, they
#'   can be toggled between using a "layer control" interface.
#' @param cols The colours used for plotting.
#' @param key Should a key for each marker be drawn? Default is \code{FALSE}.
#' @param draw.legend Should a shared legend be created at the side of the map?
#'   Default is \code{TRUE}.
#' @param collapse.control Should the "layer control" interface be collapsed?
#'   Defaults to \code{FALSE}.
#' @param iconWidth The actual width of the plot on the map in pixels.
#' @param iconHeight The actual height of the plot on the map in pixels.
#' @param fig.width The width of the plots to be produced in inches.
#' @param fig.height The height of the plots to be produced in inches.
#' @param type Deprecated. Please use \code{label} and/or \code{popup} to label
#'   different sites.
#' @param ... Other arguments for [openair::percentileRose()].
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
                          cols = "jet",
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
    rlang::exec(openair::percentileRose, !!!args, percentile = percentile, ...)
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
