#' Polar frequency plots on interactive leaflet maps
#'
#' [freqMap()] creates a \code{leaflet} map using binned polar plots as markers.
#' Any number of pollutants can be specified using the \code{pollutant}
#' argument, and multiple layers of markers can be added and toggled between
#' using \code{control}. See [openair::polarFreq()] for more information.
#'
#' @seealso Directional analysis maps: [annulusMap()], [freqMap()],
#'   [percentileMap()], [polarMap()], [pollroseMap()], [windroseMap()].
#'
#' @param data A data frame. The data frame must contain the data to plot a
#'   [openair::polarFreq()], which includes wind speed (\code{ws}), wind
#'   direction (\code{wd}), and the column representing the concentration of a
#'   pollutant. In addition, \code{data} must include a decimal latitude and
#'   longitude.
#' @param pollutant The column name(s) of the pollutant(s) to plot. If multiple
#'   pollutants are specified, they can be toggled between using a "layer
#'   control" interface.
#' @param statistic The statistic that should be applied to each wind
#'   speed/direction bin. Can be \dQuote{frequency}, \dQuote{mean},
#'   \dQuote{median}, \dQuote{max} (maximum), \dQuote{stdev} (standard
#'   deviation) or \dQuote{weighted.mean}. The option \dQuote{frequency} is the
#'   simplest and plots the frequency of wind speed/direction in different bins.
#'   The scale therefore shows the counts in each bin. The option \dQuote{mean}
#'   (the default) will plot the mean concentration of a pollutant (see next
#'   point) in wind speed/direction bins, and so on.  Finally,
#'   \dQuote{weighted.mean} will plot the concentration of a pollutant weighted
#'   by wind speed/direction. Each segment therefore provides the percentage
#'   overall contribution to the total concentration. Note that for options
#'   other than \dQuote{frequency}, it is necessary to also provide the name of
#'   a pollutant. See function [openair::cutData()] for further details.
#' @param breaks The user can provide their own scale. breaks expects a sequence
#'   of numbers that define the range of the scale. The sequence could represent
#'   one with equal spacing, e.g., \code{breaks = seq(0, 100, 10)} - a scale
#'   from 0-10 in intervals of 10, or a more flexible sequence, e.g.,
#'   \code{breaks = c(0, 1, 5, 7, 10)}, which may be useful for some situations.
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
#' @param draw.legend When \code{breaks} are specified, should a shared legend
#'   be created at the side of the map? Default is \code{TRUE}.
#' @param collapse.control When \code{control} or multiple \code{pollutant}
#'   columns are specified, should the "layer control" interface be
#'   collapsed? Defaults to \code{FALSE}.
#' @param iconWidth The actual width of the plot on the map in pixels.
#' @param iconHeight The actual height of the plot on the map in pixels.
#' @param fig.width The width of the plots to be produced in inches.
#' @param fig.height The height of the plots to be produced in inches.
#' @param type Deprecated. Please use \code{label} and/or \code{popup} to label
#'   different sites.
#' @param ... Other arguments for [openair::polarFreq()].
#' @return A leaflet object.
#' @export
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

  # define plotting function
  # need this strange setup because of how openair is set up
  args <- list(...)
  if (!is.null(breaks)) {
    fun <- function(...) {
      rlang::exec(openair::polarFreq, statistic = statistic, breaks = breaks, !!!args, ...)
    }
  } else {
    fun <- function(...) {
      rlang::exec(openair::polarFreq, statistic = statistic, !!!args, ...)
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
