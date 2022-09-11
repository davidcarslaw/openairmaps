#' Trajectory level plots in leaflet
#'
#' This function plots back trajectories on a \code{leaflet} map. This function
#' requires that data are imported using the [openair::importTraj()] function.
#'
#' @param data Data frame, the result of importing a trajectory file using
#'   [openair::importTraj()].
#' @param lon Column containing the longitude, as a decimal.
#' @param lat Column containing the latitude, as a decimal.
#' @param pollutant Pollutant to be plotted.
#' @param statistic By default the function will plot the trajectory
#'   frequencies. There are also various ways of plotting concentrations.
#'
#'   It is also possible to set \code{statistic = "difference"}. In this case
#'   trajectories where the associated concentration is greater than
#'   \code{percentile} are compared with the the full set of trajectories to
#'   understand the differences in frequencies of the origin of air masses. The
#'   comparison is made by comparing the percentage change in gridded
#'   frequencies. For example, such a plot could show that the top 10\% of
#'   concentrations of PM10 tend to originate from air-mass origins to the east.
#'
#'   If \code{statistic = "pscf"} then a Potential Source Contribution Function
#'   map is produced. If \code{statistic = "cwt"} then the Concentration
#'   Weighted Trajectory approach is used. If \code{statistic = "saqn"} then
#'   Simplified Quantitative Transport Bias Analysis is used. See "details" of
#'   [openair::trajLevel()] for more information.
#' @param percentile For [openair::trajLevel()]. The percentile concentration of
#'   \code{pollutant} against which the all trajectories are compared.
#' @param min.bin The minimum number of unique points in a grid cell. Counts
#'   below \code{min.bin} are set as missing.
#' @param cols Colours to be used for plotting. Options include
#'   \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet} and
#'   [RColorBrewer::brewer.pal()] colours — see the [openair::openColours()]
#'   function for more details. For user defined the user can supply a list of
#'   colour names recognised by R (type [grDevices::colours()] to see the full
#'   list). An example would be \code{cols = c("yellow", "green", "blue")}.
#' @param alpha Opacity of the tiles. Must be between \code{0} and \code{1}.
#' @param provider The base map(s) to be used. See
#'   \url{http://leaflet-extras.github.io/leaflet-providers/preview/} for a list
#'   of all base maps that can be used. If multiple base maps are provided, they
#'   can be toggled between using a "layer control" interface.
#'
#' @return A leaflet object.
#' @export
#'
#' @examples
#' \dontrun{
#' trajLevelMap(traj_data, pollutant = "pm2.5", statistic = "pscf", min.bin = 10)
#' }
#'
trajLevelMap <- function(data, lon = "lon", lat = "lat", pollutant = "nox",
                         statistic = "frequency", percentile = 90, min.bin = 1,
                         cols = "default", alpha = .5,
                         provider = "OpenStreetMap") {

  # get titles/legend styles

  style <- leaflet::labelFormat()
  if (statistic == "frequency") {
    title <- "percentage<br>trajectories"
    style <- leaflet::labelFormat(between = " to ", suffix = "%")
    data[[pollutant]] <- pollutant
  }
  if (statistic == "difference") {
    lastnum <- stringr::str_sub(percentile, 2, 2)
    suff <- "th"
    if (lastnum == "1") suff <- "st"
    if (lastnum == "2") suff <- "nd"
    if (lastnum == "3") suff <- "rd"
    title <- stringr::str_glue("gridded<br>differences<br>({percentile}{suff} percentile)")
    style <- leaflet::labelFormat(between = " to ", suffix = "%")
  }

  if (statistic == "pscf") title <- "PSCF<br>probability"
  if (statistic == "cwt") title <- ""
  if (statistic == "sqtba") title <- stringr::str_glue("SQTBA<br>{quickTextHTML(pollutant)}")

  # start map
  map <- leaflet::leaflet()

  # set provider tiles
  for (i in seq(length(unique(provider)))) {
    map <- leaflet::addProviderTiles(map,
      provider = unique(provider)[[i]],
      group = unique(provider)[[i]]
    )
  }
  if (length(unique(provider)) > 1) {
    map <- leaflet::addLayersControl(map, baseGroups = unique(provider))
  }

  # do trajLevel (temp dir to not print plot)
  png(filename = paste0(tempdir(), "/temp.png"))
  tl <- openair::trajLevel(
    mydata = data,
    lon = lon,
    lat = lat,
    pollutant = pollutant,
    statistic = statistic,
    percentile = percentile,
    min.bin = min.bin
  )
  dev.off()

  # get data
  data <- tl$data
  names(data)[names(data) == "height"] <- pollutant

  if (statistic == "frequency") {
    pal <- leaflet::colorBin(
      palette = openair::openColours(scheme = cols),
      domain = data[[pollutant]],
      bins = c(0, 1, 5, 10, 25, 100)
    )
  } else if (statistic == "difference") {
    pal <- leaflet::colorBin(
      palette = openair::openColours(scheme = cols),
      domain = data[[pollutant]],
      bins = c(
        floor(min(data[[pollutant]])),
        -10, -5, -1, 1, 5, 10,
        ceiling(max(data[[pollutant]]))
      )
    )
  } else {
    pal <- leaflet::colorNumeric(
      palette = openair::openColours(scheme = cols),
      domain = data[[pollutant]]
    )
  }

  # each statistic outputs a different name for "count"
  data$val <- data[[pollutant]]
  if ("N" %in% names(data)) {
    names(data)[names(data) == "N"] <- "gridcount"
  } else if ("count" %in% names(data)) {
    names(data)[names(data) == "count"] <- "gridcount"
  } else if ("n" %in% names(data)) {
    names(data)[names(data) == "n"] <- "gridcount"
  }

  # create label
  data <- dplyr::mutate(
    data,
    lab = stringr::str_glue(
      "<b>Lat:</b> {ygrid} | <b>Lon:</b> {xgrid}<br>
       <b>Count:</b> {gridcount}<br>
       <b>Value:</b> {signif(val, 3)}"
    ),
    coord = stringr::str_glue("({ygrid}, {xgrid})")
  )

  if (statistic %in% c("difference", "frequency")) data$lab <- paste0(data$lab, "%")

  # make map

  map %>%
    leaflet::addRectangles(
      data = data,
      lng1 = data[["xgrid"]] - .5,
      lng2 = data[["xgrid"]] + .5,
      lat1 = data[["ygrid"]] - .5,
      lat2 = data[["ygrid"]] + .5,
      color = "white", weight = 1,
      fillOpacity = alpha,
      fillColor = pal(data[[pollutant]]),
      popup = data[["lab"]],
      label = data[["coord"]]
    ) %>%
    leaflet::addLegend(
      title = title,
      pal = pal, values = data[[pollutant]],
      labFormat = style
    )
}
