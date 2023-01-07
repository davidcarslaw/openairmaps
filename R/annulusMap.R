#' Polar annulus plots on interactive leaflet maps
#'
#' [annulusMap()] creates a `leaflet` map using polar annulus plots as markers.
#' Any number of pollutants can be specified using the `pollutant` argument, and
#' multiple layers of markers can be added and toggled between using `control`.
#' See [openair::polarAnnulus()] for more information.
#'
#' @family directional analysis maps
#'
#' @inheritParams polarMap
#' @param period This determines the temporal period to consider. Options are
#'   "hour" (the default, to plot diurnal variations), "season" to plot
#'   variation throughout the year, "weekday" to plot day of the week variation
#'   and "trend" to plot the trend by wind direction.
#' @inheritDotParams openair::polarAnnulus -mydata -pollutant -period -limits
#'   -type -cols -key -plot
#' @return A leaflet object.
#' @export
#'
#' @examples
#' \dontrun{
#' annulusMap(polar_data,
#'   pollutant = "nox",
#'   period = "hour",
#'   provider = "Stamen.Toner"
#' )
#' }
annulusMap <- function(data,
                       pollutant = NULL,
                       period = "hour",
                       limits = NULL,
                       latitude = NULL,
                       longitude = NULL,
                       control = NULL,
                       popup = NULL,
                       label = NULL,
                       provider = "OpenStreetMap",
                       cols = "turbo",
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

  # deal with limits
  theLimits <- limits
  if (is.null(limits)) theLimits <- NA

  # prepare data for mapping
  data <-
    prepMapData(
      data = data,
      type = type,
      pollutant = pollutant,
      control = control,
      "wd",
      "date",
      latitude,
      longitude,
      popup,
      label
    )

  # define plotting function
  args <- list(...)
  if (is.null(limits)) {
    fun <- function(...) {
      rlang::exec(openair::polarAnnulus, period = period, !!!args, ...)
    }
  } else {
    fun <- function(...) {
      rlang::exec(openair::polarAnnulus, period = period, limits = theLimits, !!!args, ...)
    }
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
