#' Trajectory level plots in `leaflet`
#'
#' This function plots back trajectories on a `leaflet` map. This function
#' requires that data are imported using the [openair::importTraj()] function.
#'
#' @family interactive trajectory maps
#' @inheritParams trajMap
#' @inheritParams openair::trajLevel
#' @param cols The colours used for plotting, passed to
#'   [openair::openColours()]. The default, `"turbo"`, is a rainbow palette with
#'   relatively perceptually uniform colours. Read more about this palette at
#'   <https://research.google/blog/turbo-an-improved-rainbow-colormap-for-visualization/>.
#' @param alpha Opacity of the tiles. Must be between `0` and `1`.
#' @param tile.border Colour to use for the border of binned tiles. Defaults to
#'   `NA`, which draws no border.
#' @param smooth Should the trajectory surface be smoothed? Defaults to `FALSE`.
#'   Note that, when `smooth = TRUE`, no popup information will be available.
#'
#' @returns A leaflet object.
#' @export
#'
#' @seealso [openair::trajLevel()]
#' @seealso [trajLevelMapStatic()] for the static `ggplot2` equivalent of
#'   [trajLevelMap()]
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
           type = NULL,
           smooth = FALSE,
           statistic = "frequency",
           percentile = 90,
           lon.inc = 1,
           lat.inc = 1,
           min.bin = 1,
           .combine = NA,
           sigma = 1.5,
           cols = "turbo",
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
      if (lastnum == "1") {
        suff <- "st"
      }
      if (lastnum == "2") {
        suff <- "nd"
      }
      if (lastnum == "3") {
        suff <- "rd"
      }
      title <-
        stringr::str_glue("gridded<br>differences<br>({percentile}{suff} percentile)")
      style <- leaflet::labelFormat(between = " to ", suffix = "%")
    }

    if (statistic == "pscf") {
      title <- "PSCF<br>probability"
    }
    if (statistic == "cwt") {
      title <- ""
    }
    if (statistic == "sqtba") {
      title <-
        stringr::str_glue("SQTBA<br>{quickTextHTML(pollutant)}")
    }

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
      .combine = .combine,
      sigma = sigma,
      type = type %||% "default",
      plot = FALSE
    )$data

    # smooth
    if (smooth) {
      data <- smooth_trajgrid(data, pollutant)

      xtest <- dplyr::filter(data, .data$ygrid == .data$ygrid[[1]]) %>%
        dplyr::arrange(.data$xgrid)
      xtest <- xtest$xgrid - dplyr::lag(xtest$xgrid)
      lon.inc <- unique(xtest[!is.na(xtest)])[[1]]

      ytest <- dplyr::filter(data, .data$xgrid == .data$xgrid[[1]]) %>%
        dplyr::arrange(.data$ygrid)
      ytest <- ytest$ygrid - dplyr::lag(ytest$ygrid)
      lat.inc <- unique(ytest[!is.na(ytest)])[[1]]
    }

    names(data)[names(data) == "height"] <- pollutant

    if (statistic == "frequency" & !smooth) {
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
          floor(min(data[[pollutant]])), -10, -5, -1, 1, 5, 10,
          ceiling(max(data[[pollutant]]))
        )
      )
    } else {
      pal <-
        leaflet::colorNumeric(
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
    } else if (tolower(statistic) == "sqtba" & !is.na(.combine)) {
      data$gridcount <- NA
    }

    # create label
    if (!smooth) {
      data <- dplyr::mutate(
        data,
        lab = stringr::str_glue(
          "<b>Lat:</b> {ygrid} | <b>Lon:</b> {xgrid}<br>
       <b>Count:</b> {gridcount}<br>
       <b>Value:</b> {signif(val, 3)}"
        ),
        coord = stringr::str_glue("({ygrid}, {xgrid})")
      )

      if (statistic %in% c("difference", "frequency")) {
        data$lab <- paste0(data$lab, "%")
      }
    }

    # make hover label & popups
    data$label <- signif(data$val, 3)
    if (statistic %in% c("difference", "frequency")) {
      data$label <- paste0(data$label, "%")
    }

    if (smooth) {
      popup <- NA
    } else {
      popup <- data[["lab"]]
    }

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
        popup = popup,
        label = data[["label"]],
        group = data[[type %||% "default"]]
      ) %>%
      leaflet::addLegend(
        title = title,
        pal = pal,
        values = data[[pollutant]],
        labFormat = style
      )

    # control menu
    if (length(unique(provider)) > 1 & is.null(type)) {
      map <- leaflet::addLayersControl(map, baseGroups = unique(provider))
    } else if (length(unique(provider)) == 1 &
      !is.null(type)) {
      map <-
        leaflet::addLayersControl(map, baseGroups = sort(unique(data[[type]])))
    } else if (length(unique(provider)) > 1 &
      !is.null(type)) {
      map <-
        leaflet::addLayersControl(map,
          overlayGroups = unique(provider),
          baseGroups = sort(unique(data[[type]]))
        )
    }

    # return map
    return(map)
  }

#' Trajectory level plots in `ggplot2`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function plots back trajectories on a `ggplot2` map. This function
#'   requires that data are imported using the [openair::importTraj()] function.
#'   It is a `ggplot2` implementation of [openair::trajLevel()] with many of the
#'   same arguments, which should be more flexible for post-hoc changes.
#'
#' @family static trajectory maps
#' @inheritParams trajLevelMap
#' @inheritParams trajMapStatic
#' @param crs The coordinate reference system (CRS) into which all data should
#'   be projected before plotting. Defaults to latitude/longitude
#'   (`sf::st_crs(4326)`).
#' @param smooth Should the trajectory surface be smoothed? Defaults to `FALSE`.
#'   Note that smoothing may cause the plot to render slower, so consider
#'   setting `crs` to `sf::st_crs(4326)` or `NULL`.
#' @inheritDotParams ggplot2::coord_sf -xlim -ylim -crs -default_crs
#'
#' @returns A `ggplot2` plot
#' @export
#'
#' @seealso [openair::trajLevel()]
#' @seealso [trajLevelMap()] for the interactive `leaflet` equivalent of
#'   [trajLevelMapStatic()]
trajLevelMapStatic <-
  function(data,
           longitude = "lon",
           latitude = "lat",
           pollutant,
           type = NULL,
           smooth = FALSE,
           statistic = "frequency",
           percentile = 90,
           lon.inc = 1,
           lat.inc = 1,
           min.bin = 1,
           .combine = NA,
           sigma = 1.5,
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
           facet = NULL,
           ...) {
    # handle deprecated argument
    if (!is.null(facet)) {
      lifecycle::deprecate_soft(
        when = "0.9.0",
        what = "trajLevelMapStatic(facet)",
        with = "trajLevelMapStatic(type)"
      )
    }
    type <- type %||% facet

    # prep data for running in TrajLevel
    if (statistic == "frequency") {
      title <- "percentage\ntrajectories"
      pollutant <- "default_pollutant"
      data[[pollutant]] <- pollutant
    }

    if (statistic == "difference") {
      lastnum <- stringr::str_sub(percentile, 2, 2)
      suff <- "th"
      if (lastnum == "1") {
        suff <- "st"
      }
      if (lastnum == "2") {
        suff <- "nd"
      }
      if (lastnum == "3") {
        suff <- "rd"
      }
      title <-
        stringr::str_glue("gridded\ndifferences\n({percentile}{suff} percentile)")
    }

    if (statistic == "pscf") {
      title <- "PSCF\nprobability"
    }
    if (statistic == "cwt") {
      title <- ""
    }
    if (statistic == "sqtba") {
      title <-
        openair::quickText(stringr::str_glue("SQTBA\n({pollutant})"))
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
      type = type %||% "default",
      .combine = .combine,
      sigma = sigma,
      plot = FALSE
    )$data

    # fix names
    names(data)[names(data) == "height"] <- pollutant

    # smooth
    if (smooth) {
      data <- smooth_trajgrid(data, pollutant)
    }

    # start plot
    plt <-
      ggplot2::ggplot(data, ggplot2::aes(
        x = .data$xgrid,
        y = .data$ygrid,
        fill = .data[[pollutant]]
      ))

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

    # create coordinate system
    if (!is.null(crs)) {
      coords <-
        ggplot2::coord_sf(
          xlim = xlim,
          ylim = ylim,
          default_crs = sf::st_crs(4326),
          crs = crs,
          ...
        )
    } else {
      coords <-
        ggplot2::coord_sf(
          xlim = xlim,
          ylim = ylim,
          ...
        )
    }

    # add to plot
    plt <- plt +
      ggplot2::geom_tile(alpha = alpha, color = tile.border) +
      theme_static() +
      ggplot2::labs(x = "longitude", y = "latitude", fill = title) +
      coords

    # deal with facets
    if (!is.null(type)) {
      plt <-
        plt + ggplot2::facet_wrap(ggplot2::vars(.data[[type]]))
    }

    # return plot
    return(plt)
  }


#' Smooth grid for trajectories
#' @noRd
smooth_trajgrid <- function(mydata, z, k = 50, dist = 0.05) {
  myform <-
    stats::formula(stringr::str_glue("{z}^0.5 ~ s(xgrid, ygrid, k = {k})"))

  res <- 101
  Mgam <- mgcv::gam(myform, data = mydata)

  new.data <- expand.grid(
    xgrid = seq(min(mydata$xgrid),
      max(mydata$xgrid),
      length = res
    ),
    ygrid = seq(min(mydata$ygrid),
      max(mydata$ygrid),
      length = res
    )
  )

  pred <- mgcv::predict.gam(Mgam, newdata = new.data)
  pred <- as.vector(pred)^2

  new.data[, z] <- pred

  ## exlcude too far
  ## exclude predictions too far from data (from mgcv)
  x <- seq(min(mydata$xgrid), max(mydata$xgrid), length = res)
  y <- seq(min(mydata$ygrid), max(mydata$ygrid), length = res)

  wsp <- rep(x, res)
  wdp <- rep(y, rep(res, res))

  ## data with gaps caused by min.bin
  all.data <-
    stats::na.omit(data.frame(xgrid = mydata$xgrid, ygrid = mydata$ygrid, z))

  ind <- with(
    all.data,
    mgcv::exclude.too.far(wsp, wdp, mydata$xgrid,
      mydata$ygrid,
      dist = dist
    )
  )

  new.data[ind, z] <- NA

  new.data <- tidyr::drop_na(new.data)

  dplyr::tibble(new.data)
}
