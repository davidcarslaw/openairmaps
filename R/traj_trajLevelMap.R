#' Trajectory level plots in leaflet
#'
#' This function plots back trajectories on a `leaflet` map. This function
#' requires that data are imported using the [openair::importTraj()] function.
#'
#' @family trajectory maps
#' @inheritParams openair::trajLevel
#' @param control Column to be used for splitting the input data into different
#'   groups which can be selected between using a "layer control" interface.
#'   Passed to the `type` argument of [openair::trajLevel()] (see
#'   [openair::cutData()] for more information).
#' @param latitude,longitude The decimal latitude/longitude.
#' @param cols Colours to be used for plotting. Options include "default",
#'   "increment", "heat", "turbo" and `RColorBrewer` colours — see the
#'   [openair::openColours()] function for more details. For user defined the
#'   user can supply a list of colour names recognised by R (type
#'   [grDevices::colours()] to see the full list). An example would be `cols =
#'   c("yellow", "green", "blue")`.
#' @param alpha Opacity of the tiles. Must be between `0` and `1`.
#' @param tile.border Colour to use for the border of binned tiles. Defaults to
#'   `NA`, which draws no border.
#' @param provider The base map(s) to be used. See
#'   <http://leaflet-extras.github.io/leaflet-providers/preview/> for a list of
#'   all base maps that can be used. If multiple base maps are provided, they
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
trajLevelMap <-
  function(data,
           longitude = "lon",
           latitude = "lat",
           pollutant,
           control = "default",
           statistic = "frequency",
           percentile = 90,
           lon.inc = 1,
           lat.inc = 1,
           min.bin = 1,
           cols = "default",
           alpha = .5,
           tile.border = NA,
           provider = "OpenStreetMap") {
    # get titles/legend styles

    style <- leaflet::labelFormat()
    if (statistic == "frequency") {
      title <- "percentage<br>trajectories"
      style <- leaflet::labelFormat(between = " to ", suffix = "%")
      pollutant <- "default_pollutant"
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

    # run openair::trajLevel()
    data <- openair::trajLevel(
      mydata = data,
      lon = longitude,
      lat = latitude,
      pollutant = pollutant,
      statistic = statistic,
      percentile = percentile,
      lat.inc = lat.inc,
      lon.inc = lon.inc,
      min.bin = min.bin,
      type = control,
      plot = FALSE
    )$data

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

    map <-
      leaflet::addRectangles(
        map = map,
        data = data,
        lng1 = data[["xgrid"]] - (lon.inc / 2),
        lng2 = data[["xgrid"]] + (lon.inc / 2),
        lat1 = data[["ygrid"]] - (lat.inc / 2),
        lat2 = data[["ygrid"]] + (lat.inc / 2),
        color = tile.border,
        weight = 1,
        fillOpacity = alpha,
        fillColor = pal(data[[pollutant]]),
        popup = data[["lab"]],
        label = data[["coord"]],
        group = data[[control]]
      ) %>%
      leaflet::addLegend(
        title = title,
        pal = pal, values = data[[pollutant]],
        labFormat = style
      )

    # control menu
    if (length(unique(provider)) > 1 & control == "default") {
      map <- leaflet::addLayersControl(map, baseGroups = unique(provider))
    } else if (length(unique(provider)) == 1 & control != "default") {
      map <- leaflet::addLayersControl(map, baseGroups = unique(data[[control]]))
    } else if (length(unique(provider)) > 1 & control != "default") {
      map <- leaflet::addLayersControl(map, overlayGroups = unique(provider), baseGroups = unique(data[[control]]))
    }

    # return map
    return(map)
  }
