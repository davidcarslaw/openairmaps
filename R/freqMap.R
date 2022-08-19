#' Bivariate polar plots on interactive leaflet maps
#'
#' @param data A data frame. The data frame must contain the data to plot a
#'   \code{polarPlot}, which includes wind speed (\code{ws}), wind direction
#'   (\code{wd}) and the column representing the concentration of a pollutant.
#'   In addition, \code{data} must include a decimal latitude and longitude.
#' @param pollutant The column name(s) of the pollutant(s) to plot. If multiple
#'   pollutants are specified, they can be toggled between using a "layer
#'   control" interface.
#' @param statistic The statistic that should be applied to each wind
#'   speed/direction bin. Can be “frequency”, “mean”, “median”, “max” (maximum),
#'   “stdev” (standard deviation) or “weighted.mean”. The option “frequency”
#'   (the default) is the simplest and plots the frequency of wind
#'   speed/direction in different bins. The scale therefore shows the counts in
#'   each bin. The option “mean” will plot the mean concentration of a pollutant
#'   (see next point) in wind speed/direction bins, and so on. Finally,
#'   “weighted.mean” will plot the concentration of a pollutant weighted by wind
#'   speed/direction. Each segment therefore provides the percentage overall
#'   contribution to the total concentration. More information is given in the
#'   examples. Note that for options other than “frequency”, it is necessary to
#'   also provide the name of a pollutant. See function cutData for further
#'   details.
#' @param latitude The decimal latitude.
#' @param longitude The decimal longitude.
#' @param provider The base map to be used. See
#'   \url{http://leaflet-extras.github.io/leaflet-providers/preview/} for a lits
#'   of all base maps that can be used.
#' @param type The grouping variable that provides a data set for a specific
#'   location. Often, with several sites, \code{type = "site"} is used.
#' @param cols The colours used for plotting.
#' @param alpha The alpha transparency to use for the plotting surface (a value
#'   between 0 and 1 with zero being fully transparent and 1 fully opaque).
#' @param key Should the key of the polar plot be drawn. Default is
#'   \code{FALSE}.
#' @param iconWidth The actual width of the plot on the map in pixels.
#' @param iconHeight The actual height of the plot on the map in pixels.
#' @param fig.width The width of the plots to be produced in inches.
#' @param fig.height The height of the plots to be produced in inches.
#' @param ... Other arguments for \code{polarPlot}.
#' @return A leaflet object.
#' @import leaflet
#' @importFrom grDevices dev.off png
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#'
#' polarMap(polar_data, latitude = "latitude", longitude = "longitude",
#' x = "ws", type = "site", provider = "Stamen.Toner")
freqMap <- function(data,
                    pollutant = "nox",
                    statistic = "mean",
                    latitude = "latitude",
                    longitude = "longitude",
                    provider = "OpenStreetMap",
                    type = "default",
                    cols = "jet",
                    alpha = 1,
                    key = FALSE,
                    iconWidth = 200,
                    iconHeight = 200,
                    fig.width = 4,
                    fig.height = 4,
                    ...) {

  . <- NULL

  ## extract variables of interest
  vars <- c("wd", x, pollutant, latitude, longitude, type)

  if (type == "default")
    vars <- c("wd", x, pollutant, latitude, longitude)

  # check and select variables
  data <- openair:::checkPrep(data, vars, type = type)

  # cut data
  data <- openair::cutData(data, type)

  # remove missing data
  data <- na.omit(data)

  # check to see if variables exist in data
  if (length(intersect(vars, names(data))) != length(vars))
    stop(paste(vars[which(!vars %in% names(data))], "not found in data"), call. = FALSE)

  # where to write files
  dir_polar <- tempdir()

  # summarise data - one line per location
  plot_data <-
    dplyr::group_by(data, .data[[type]]) %>%
    dplyr::slice(n = 1) %>%
    dplyr::arrange(.data[[type]])

  # define plotting function
  args <- list(...)
  fun <- function(...){

    rlang::exec(polarFreq, !!!args, ...)

  }

  # create icons
  icons <-
    purrr::map(.x = sort(pollutant),
               .f = ~create_icons(data = data, fun = fun, dir = dir_polar, pollutant = .x,
                                  type = type, x = x, cols = cols, alpha = alpha, key = key,
                                  fig.width = fig.width, fig.height = fig.height,
                                  iconWidth = iconWidth, iconHeight = iconHeight, ...
               ))

  # plot leaflet
  m <- leaflet::leaflet(data = plot_data) %>%
    leaflet::addProviderTiles(provider = provider)

  # add markers
  for (i in 1:length(icons)) {

    m <- leaflet::addMarkers(
      m,
      data = plot_data,
      lng = plot_data[[longitude]],
      lat = plot_data[[latitude]],
      icon = icons[[i]],
      popup = plot_data[[type]],
      group = sort(pollutant)[[i]] %>% quickTextHTML()
    )

  }

  # add layer control for pollutants
  if(length(pollutant) > 1){
    m <-
      leaflet::addLayersControl(
        m,
        baseGroups = sort(pollutant) %>% purrr::map_chr(quickTextHTML)
      )
  }


  # return
  m

}

