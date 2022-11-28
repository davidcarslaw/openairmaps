#' Wind rose plots on interactive leaflet maps
#'
#' [windroseMap()] creates a \code{leaflet} map using wind roses as markers.
#' Multiple layers of markers can be added and toggled between using
#' \code{control}. See [openair::windRose()] for more information.
#'
#' @family directional analysis maps
#'
#' @inheritParams polarMap
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
#' @param draw.legend Should a shared legend be created at the side of the map?
#'   Default is \code{TRUE}.
#' @inheritDotParams openair::windRose -ws.int -breaks -mydata -plot -annotate -pollutant -type -cols -key
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
