#' Bivariate polar plots on interactive leaflet maps
#'
#' [polarMap()] creates a `leaflet` map using bivariate polar plots as
#' markers. Any number of pollutants can be specified using the `pollutant`
#' argument, and multiple layers of markers can be added and toggled between
#' using `control`. See [openair::polarPlot()] for more information.
#'
#' @family directional analysis maps
#'
#' @param data A data frame. The data frame must contain the data to plot the
#'   directional analysis marker, which includes wind speed (`ws`), wind
#'   direction (`wd`), and the column representing the concentration of a
#'   pollutant. In addition, `data` must include a decimal latitude and
#'   longitude.
#' @param pollutant The column name(s) of the pollutant(s) to plot. If multiple
#'   pollutants are specified, they can be toggled between using a "layer
#'   control" interface.
#' @param x The radial axis variable to plot.
#' @param limits By default, each individual polar marker has its own colour
#'   scale. The `limits` argument will force all markers to use the same
#'   colour scale. The limits are set in the form `c(lower, upper)`, so
#'   `limits = c(0, 100)` would force the plot limits to span 0-100.
#' @param latitude,longitude The decimal latitude/longitude. If not provided,
#'   will be automatically inferred from data by looking for a column named
#'   "lat"/"latitude" or
#'   "lon"/"lng"/"long"/"longitude"
#'   (case-insensitively).
#' @param control Column to be used for splitting the input data into different
#'   groups which can be selected between using a "layer control" interface.
#'   Appropriate columns could be those added by [openair::cutData()] or
#'   [openair::splitByDate()]. `control` cannot be used if multiple
#'   `pollutant` columns have been provided.
#' @param popup Column to be used as the HTML content for marker popups. Popups
#'   may be useful to show information about the individual sites (e.g., site
#'   names, codes, types, etc.).
#' @param label Column to be used as the HTML content for hover-over labels.
#'   Labels are useful for the same reasons as popups, though are typically
#'   shorter.
#' @param provider The base map(s) to be used. See
#'   <http://leaflet-extras.github.io/leaflet-providers/preview/> for a list
#'   of all base maps that can be used. If multiple base maps are provided, they
#'   can be toggled between using a "layer control" interface.
#' @param cols The colours used for plotting.
#' @param alpha The alpha transparency to use for the plotting surface (a value
#'   between 0 and 1 with zero being fully transparent and 1 fully opaque).
#' @param key Should a key for each marker be drawn? Default is `FALSE`.
#' @param draw.legend When `limits` are specified, should a shared legend
#'   be created at the side of the map? Default is `TRUE`.
#' @param collapse.control Should the "layer control" interface be collapsed?
#'   Defaults to `FALSE`.
#' @param iconWidth The actual width of the plot on the map in pixels.
#' @param iconHeight The actual height of the plot on the map in pixels.
#' @param fig.width The width of the plots to be produced in inches.
#' @param fig.height The height of the plots to be produced in inches.
#' @param type Deprecated. Please use `label` and/or `popup` to label
#'   different sites.
#' @inheritDotParams openair::polarPlot -mydata -pollutant -x -limits -type
#'   -cols -key -alpha -plot
#' @return A leaflet object.
#' @export
#'
#' @examples
#' \dontrun{
#' polarMap(polar_data,
#'   pollutant = "nox",
#'   x = "ws",
#'   provider = "Stamen.Toner"
#' )
#' }
polarMap <- function(data,
                     pollutant = NULL,
                     x = "ws",
                     limits = NULL,
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
  # warn type
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

  # deal with limits
  theLimits <- limits
  if (is.null(limits)) theLimits <- NA

  # prep data
  data <-
    prepMapData(
      data = data,
      pollutant = pollutant,
      control = control,
      "wd",
      x,
      latitude,
      longitude,
      popup,
      label
    )

  # define plotting function
  args <- list(...)
  fun <- function(...) {
    rlang::exec(openair::polarPlot, x = x, limits = theLimits, alpha = alpha, !!!args, ...)
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

  # add legend if limits are set
  if (!is.null(limits) & all(!is.na(limits)) & draw.legend) {
    map <-
      leaflet::addLegend(
        map,
        title = quickTextHTML(paste(pollutant, collapse = ",<br>")),
        pal = leaflet::colorNumeric(
          palette = openair::openColours(scheme = cols),
          domain = theLimits
        ),
        values = theLimits
      )
  }

  map
}
