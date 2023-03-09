#' Trajectory level plots in `leaflet`
#'
#' This function plots back trajectories on a `leaflet` map. This function
#' requires that data are imported using the [openair::importTraj()] function.
#'
#' @family interactive trajectory maps
#' @inheritParams trajMap
#' @inheritParams openair::trajLevel
#' @param cols Colours to be used for plotting. Options include "default",
#'   "increment", "heat", "turbo" and `RColorBrewer` colours — see the
#'   [openair::openColours()] function for more details. For user defined the
#'   user can supply a list of colour names recognised by R (type
#'   [grDevices::colours()] to see the full list). An example would be `cols =
#'   c("yellow", "green", "blue")`.
#' @param alpha Opacity of the tiles. Must be between `0` and `1`.
#' @param tile.border Colour to use for the border of binned tiles. Defaults to
#'   `NA`, which draws no border.
#'
#' @return A leaflet object.
#' @export
#'
#' @seealso the original [openair::trajLevel()]
#' @seealso [trajLevelMapStatic()] for the static `ggplot2` equivalent of [trajLevelMap()]
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
      if (lastnum == "1")
        suff <- "st"
      if (lastnum == "2")
        suff <- "nd"
      if (lastnum == "3")
        suff <- "rd"
      title <-
        stringr::str_glue("gridded<br>differences<br>({percentile}{suff} percentile)")
      style <- leaflet::labelFormat(between = " to ", suffix = "%")
    }

    if (statistic == "pscf")
      title <- "PSCF<br>probability"
    if (statistic == "cwt")
      title <- ""
    if (statistic == "sqtba")
      title <-
        stringr::str_glue("SQTBA<br>{quickTextHTML(pollutant)}")

    # start map
    map <- leaflet::leaflet()

    # set provider tiles
    for (i in seq(length(unique(provider)))) {
      map <- leaflet::addProviderTiles(map,
                                       provider = unique(provider)[[i]],
                                       group = unique(provider)[[i]])
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
        bins = c(floor(min(data[[pollutant]])), -10, -5, -1, 1, 5, 10,
                 ceiling(max(data[[pollutant]])))
      )
    } else {
      pal <-
        leaflet::colorNumeric(palette = openair::openColours(scheme = cols),
                              domain = data[[pollutant]])
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

    if (statistic %in% c("difference", "frequency"))
      data$lab <- paste0(data$lab, "%")

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
        pal = pal,
        values = data[[pollutant]],
        labFormat = style
      )

    # control menu
    if (length(unique(provider)) > 1 & control == "default") {
      map <- leaflet::addLayersControl(map, baseGroups = unique(provider))
    } else if (length(unique(provider)) == 1 &
               control != "default") {
      map <-
        leaflet::addLayersControl(map, baseGroups = unique(data[[control]]))
    } else if (length(unique(provider)) > 1 &
               control != "default") {
      map <-
        leaflet::addLayersControl(map,
                                  overlayGroups = unique(provider),
                                  baseGroups = unique(data[[control]]))
    }

    # return map
    return(map)
  }

#' Trajectory level plots in `ggplot2`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function plots back trajectories on a `ggplot2` map. This function
#' requires that data are imported using the [openair::importTraj()] function.
#' It is a `ggplot2` implementation of [openair::trajLevel()] with many of the
#' same arguments, which should be more flexible for post-hoc changes.
#'
#' @family static trajectory maps
#' @inheritParams trajLevelMap
#' @inheritParams trajMapStatic
#' @param crs The coordinate reference system (CRS) into which all data should
#'   be projected before plotting. Defaults to latitude/longitude
#'   (`sf::st_crs(4326)`).
#' @inheritDotParams ggplot2::coord_sf -xlim -ylim -crs -default_crs
#'
#' @return A `ggplot2` plot
#' @export
#'
#' @seealso the original [openair::trajLevel()]
#' @seealso [trajLevelMap()] for the interactive `leaflet` equivalent of
#'   [trajLevelMapStatic()]
trajLevelMapStatic <-
  function(data,
           longitude = "lon",
           latitude = "lat",
           pollutant,
           facet = "default",
           statistic = "frequency",
           percentile = 90,
           lon.inc = 1,
           lat.inc = 1,
           min.bin = 1,
           alpha = .5,
           tile.border = NA,
           xlim = NULL,
           ylim = NULL,
           crs = sf::st_crs(4326),
           map = TRUE,
           map.fill = "grey85",
           map.colour = "grey75",
           map.alpha = 0.8,
           map.lwd = 0.5,
           map.lty = 1,
           ...) {
    # prep data for running in TrajLevel
    if (statistic == "frequency") {
      title <- "percentage\ntrajectories"
      pollutant <- "default_pollutant"
      data[[pollutant]] <- pollutant
    }

    if (statistic == "difference") {
      lastnum <- stringr::str_sub(percentile, 2, 2)
      suff <- "th"
      if (lastnum == "1")
        suff <- "st"
      if (lastnum == "2")
        suff <- "nd"
      if (lastnum == "3")
        suff <- "rd"
      title <-
        stringr::str_glue("gridded\ndifferences\n({percentile}{suff} percentile)")
    }

    if (statistic == "pscf")
      title <- "PSCF\nprobability"
    if (statistic == "cwt")
      title <- ""
    if (statistic == "sqtba")
      title <-
        openair::quickText(stringr::str_glue("SQTBA\n({pollutant})"))

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
      type = facet,
      plot = FALSE
    )$data

    # fix names
    names(data)[names(data) == "height"] <- pollutant

    # start plot
    plt <-
      ggplot2::ggplot(data, ggplot2::aes(x = .data$xgrid,
                                         y = .data$ygrid,
                                         fill = .data[[pollutant]]))

    if (map) {
      world <- ggplot2::map_data("world")

      plt <- plt +
        ggplot2::geom_polygon(
          data = world,
          fill = map.fill,
          colour = map.colour,
          alpha = map.alpha,
          linewidth = map.lwd,
          lty = map.lty,
          ggplot2::aes(.data$long, .data$lat, group = .data$group)
        )
    }

    # predict x/ylims
    if (is.null(xlim)) {
      d_lon <-
        diff(range(c(min(data$xgrid), max(data$xgrid)))) * 0.1
      xlim <-
        c(min(data$xgrid) - d_lon, max(data$xgrid) + d_lon)
    }
    if (is.null(ylim)) {
      d_lat <-
        diff(range(c(min(data$ygrid), max(data$ygrid)))) * 0.1
      ylim <-
        c(min(data$ygrid) - d_lat, max(data$ygrid) + d_lat)
    }

    # add to plot
    plt <- plt +
      ggplot2::geom_tile(alpha = alpha, color = tile.border) +
      ggplot2::coord_sf(
        xlim = xlim,
        ylim = ylim,
        default_crs = sf::st_crs(4326),
        crs = crs,
        ...
      ) +
      theme_static() +
      ggplot2::labs(x = "longitude", y = "latitude", fill = title)

    # deal with facets
    if (facet != "default") {
      plt <-
        plt + ggplot2::facet_wrap(ggplot2::vars(.data[[facet]]))
    }

    # return plot
    return(plt)
  }
