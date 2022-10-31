#' Wind rose plots on interactive leaflet maps
#'
#' [windroseMap()] creates a \code{leaflet} map using wind roses as markers.
#' Multiple layers of markers can be added and toggled between using
#' \code{control}. See [openair::windRose()] for more information.
#'
#' @seealso Directional analysis maps: [annulusMap()], [freqMap()],
#'   [percentileMap()], [polarMap()], [pollroseMap()], [windroseMap()].
#'
#' @param data A data frame. The data frame must contain the data to plot a
#'   [openair::windRose()], which includes wind speed (\code{ws}), and wind
#'   direction (\code{wd}). In addition, \code{data} must include a decimal
#'   latitude and longitude.
#' @param ws.int The wind speed interval. Default is 2 m/s but for low met masts
#'   with low mean wind speeds a value of 1 or 0.5 m/s may be better.
#' @param breaks Most commonly, the number of break points for wind speed in
#'   windRose. For windRose and the ws.int default of 2 m/s, the default, 4,
#'   generates the break points 2, 4, 6, 8 m/s. Breaks can also be used to set
#'   specific break points. For example, the argument breaks = c(0, 1, 10, 100)
#'   breaks the data into segments <1, 1-10, 10-100, >100.
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
#'   [openair::splitByDate()].
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
#' @param alpha The alpha transparency to use for the plotting surface (a value
#'   between 0 and 1 with zero being fully transparent and 1 fully opaque).
#' @param key Should a key for each marker be drawn? Default is \code{FALSE}.
#' @param draw.legend Should a shared legend be created at the side of the map?
#'   Default is \code{TRUE}.
#' @param iconWidth The actual width of the plot on the map in pixels.
#' @param iconHeight The actual height of the plot on the map in pixels.
#' @param fig.width The width of the plots to be produced in inches.
#' @param fig.height The height of the plots to be produced in inches.
#' @param type Deprecated. Please use \code{label} and/or \code{popup} to label
#'   different sites.
#' @param ... Other arguments for [openair::windRose()].
#' @return A leaflet object.
#' @export
#'
#' @examples
#' \dontrun{
#' windroseMap(polar_data,
#'   provider = "Stamen.Toner"
#' )
#' }
windroseMap <- function(data,
                        ws.int = 2,
                        breaks = 4,
                        latitude = NULL,
                        longitude = NULL,
                        control = NULL,
                        popup = NULL,
                        label = NULL,
                        provider = "OpenStreetMap",
                        cols = "jet",
                        alpha = 1,
                        key = FALSE,
                        draw.legend = TRUE,
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

  # need to put ws in a separate column to work with the rest of openairmaps
  # utilities...
  data$ws_dup <- data$ws

  data <-
    prepMapData(
      data = data,
      type = type,
      pollutant = "ws_dup",
      control = control,
      "ws",
      "wd",
      latitude,
      longitude,
      popup,
      label
    )

  # work out breaks
  # needs to happen before plotting to ensure same scales
  breaks <-
    getBreaks(breaks = breaks, ws.int = ws.int, vec = data$conc, polrose = FALSE)

  # define plotting function
  args <- list(...)
  if ("pollutant" %in% names(args)) {
    args <- args[names(args) != "pollutant"]
    cli::cli_alert_warning("{.fun windroseMap} does not support the {.code pollutant} argument.")
  }

  fun <- function(...) {
    rlang::exec(openair::windRose, annotate = FALSE, breaks = breaks, ws.int = ws.int, !!!args, ...)
  }

  # identify splitting column (defaulting to pollutant)
  if (!is.null(control)) {
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
        lat = latitude, lon = longitude, x = x, cols = cols, alpha = alpha,
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
      split_col = split_col
    )

  if (draw.legend) {
    map <-
      leaflet::addLegend(
        map,
        pal = leaflet::colorBin(
          palette = openair::openColours(cols),
          domain = breaks,
          bins = breaks
        ),
        values = breaks,
        title = "Wind Speed"
      )
  }

  map
}
