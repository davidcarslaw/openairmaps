#' Trajectory line plots in `leaflet`
#'
#' This function plots back trajectories on a `leaflet` map. This function
#' requires that data are imported using the [openair::importTraj()] function.
#' Options are provided to colour the individual trajectories (e.g., by
#' pollutant concentrations) or create "layer control" menus to show/hide
#' different layers.
#'
#' @family interactive trajectory maps
#'
#' @param data Data frame, the result of importing a trajectory file using
#'   [openair::importTraj()].
#' @param latitude,longitude The decimal latitude/longitude.
#' @param colour Column to be used for colouring each trajectory. This column
#'   may be numeric, character or factor. This will commonly be a pollutant
#'   concentration which has been joined (e.g., by [dplyr::left_join()]) to the
#'   trajectory data by "date".
#' @param control Used for splitting the trajectories into different groups
#'   which can be selected between using a "layer control" menu. Passed to
#'   [openair::cutData()].
#' @param cols Colours to be used for plotting. Options include "default",
#'   "increment", "heat", "turbo" and `RColorBrewer` colours — see the
#'   [openair::openColours()] function for more details. For user defined the
#'   user can supply a list of colour names recognised by R (type
#'   [grDevices::colours()] to see the full list). An example would be `cols =
#'   c("yellow", "green", "blue")`. If the `"colour"` argument was not used, a
#'   single colour can be named which will be used consistently for all
#'   lines/points (e.g., `cols = "red"`).
#' @param alpha Opacity of lines/points. Must be between `0` and `1`.
#' @param npoints A dot is placed every `npoints` along each full trajectory.
#'   For hourly back trajectories points are plotted every `npoints` hours. This
#'   helps to understand where the air masses were at particular times and get a
#'   feel for the speed of the air (points closer together correspond to slower
#'   moving air masses). Defaults to `12`.
#' @param provider The base map to be used. See
#'   <http://leaflet-extras.github.io/leaflet-providers/preview/> for a list of
#'   all base maps that can be used.
#' @param collapse.control Should the "layer control" interface be collapsed?
#'   Defaults to `FALSE`.
#'
#' @return A leaflet object.
#' @export
#'
#' @seealso the original [openair::trajPlot()]
#' @seealso [trajMapStatic()] for the static `ggplot2` equivalent of [trajMap()]
#'
#' @examples
#' \dontrun{
#' trajMap(traj_data, colour = "nox")
#' }
#'
trajMap <-
  function(data,
           longitude = "lon",
           latitude = "lat",
           colour,
           control = "default",
           cols = "default",
           alpha = .5,
           npoints = 12,
           provider = "OpenStreetMap",
           collapse.control = FALSE) {
    # make lat/lon easier to use
    names(data)[names(data) == longitude] <- "lon"
    names(data)[names(data) == latitude] <- "lat"

    # default colour is black
    fixedcol <- "black"

    # get factor version of date to reorder by "colour"
    data$datef <- factor(data$date)

    # if no "control", get a fake column
    data <- openair::cutData(data, control)

    # initialise map
    map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(provider = provider)

    # if "colour", create colour palette
    if (!missing(colour)) {
      if (colour %in% names(data)) {
        data <- dplyr::arrange(data, .data$datef)

        if ("factor" %in% class(data[[colour]]) | "character" %in% class(data[[colour]])) {
          pal <- leaflet::colorFactor(
            palette = openair::openColours(scheme = cols, n = length(unique(data[[colour]]))),
            domain = data[[colour]]
          )
        } else if ("POSIXct" %in% class(data[[colour]])) {
          pal <- leaflet::colorNumeric(
            palette = openair::openColours(scheme = cols),
            domain = as.numeric(data[[colour]], origin = "1964-10-22")
          )
        } else {
          pal <- leaflet::colorNumeric(
            palette = openair::openColours(scheme = cols),
            domain = data[[colour]]
          )
        }
      } else {
        fixedcol <- colour
      }
    }

    # make labels
    data <- dplyr::mutate(
      data,
      lab = stringr::str_glue("<b>Arrival Date:</b> {date}<br>
                             <b>Trajectory Date:</b> {date2}<br>
                             <b>Lat:</b> {lat} | <b>Lon:</b> {lon}<br>
                             <b>Height:</b> {height} m | <b>Pressure:</b> {pressure} Pa")
    )

    if (!missing(colour)) {
      if (colour %in% names(data) & !colour %in% c("date", "date2", "lat", "lon", "height", "pressure")) {
        data$lab <- paste(
          data$lab,
          paste0("<b>", quickTextHTML(colour), ":</b> ", data[[colour]]),
          sep = "<br>"
        )
      }
    }

    # iterate over columns in "control" column
    for (j in seq(length(unique(data[[control]])))) {
      # get jth instance of "control"
      data2 <- dplyr::filter(data, .data[[control]] == unique(data[[control]])[[j]])

      # iterate over different arrival dates to plot separate trajectories
      for (i in seq(length(unique(data2$datef)))) {
        # get line/points data
        ldata <- dplyr::filter(data2, .data$datef == unique(data2$datef)[[i]])
        pdata <- dplyr::filter(ldata, .data$hour.inc %% npoints == 0)

        lcolors <- fixedcol
        pcolors <- fixedcol
        # apply color pal if it exists
        if (!missing(colour)) {
          if (colour %in% names(data)) {
            lcolors <- pal(ldata[[colour]])[1]
            pcolors <- pal(pdata[[colour]])
          }
        }

        # add points/lines to plot
        map <-
          leaflet::addPolylines(
            map = map,
            data = ldata,
            lng = ldata$lon,
            lat = ldata$lat,
            opacity = alpha,
            weight = 2,
            color = lcolors,
            group = as.character(unique(data[[control]])[[j]])
          ) %>%
          leaflet::addCircleMarkers(
            data = pdata,
            radius = 3, stroke = F,
            lng = pdata$lon,
            lat = pdata$lat,
            fillOpacity = alpha,
            color = pcolors,
            group = as.character(unique(data[[control]])[[j]]),
            popup = pdata$lab
          )
      }
    }

    # if "group" exists, add a legend
    if (!missing(colour)) {
      if (colour %in% names(data)) {
        if ("POSIXct" %in% class(data[[colour]])) {
          map <-
            leaflet::addLegend(map,
                               title = quickTextHTML(colour),
                               pal = pal,
                               values = as.numeric(data[[colour]], origin = "1964-10-22"),
                               labFormat = leaflet::labelFormat(
                                 transform = function(x) as.Date.POSIXct(x, origin = "1964-10-22")
                               )
            )
        } else {
          map <-
            leaflet::addLegend(map,
                               title = quickTextHTML(colour),
                               pal = pal,
                               values = data[[colour]]
            )
        }
      }
    }

    # if "control" exists, add the layer control menu
    if (control != "default") {
      map <-
        leaflet::addLayersControl(
          map,
          options = leaflet::layersControlOptions(collapsed = collapse.control),
          overlayGroups = as.character(unique(data[[control]]))
        )
    }

    map
  }

#' Trajectory line plots in `ggplot2`
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function plots back trajectories using `ggplot2`. The function requires
#' that data are imported using [openair::importTraj()]. It is a `ggplot2`
#' implementation of [openair::trajPlot()] with many of the same arguments,
#' which should be more flexible for post-hoc changes.
#'
#' @family static trajectory maps
#'
#' @inheritParams trajMap
#' @param facet Used for splitting the trajectories into different panels.
#'   Passed to [openair::cutData()].
#' @param group By default, trajectory paths are distinguished using the arrival
#'   date. `group` allows for additional columns to be used (e.g.,
#'   `"receptor"`).
#' @param xlim,ylim The x- and y-limits of the plot. If `NULL`, limits will be
#'   estimated based on the lat/lon ranges of the input data.
#' @param crs The coordinate reference system (CRS) into which all data should
#'   be projected before plotting. Defaults to the Lambert projection
#'   (`sf::st_crs(3812)`).
#' @param map Should a base map be drawn? Defaults to `TRUE`.
#' @param map.fill Colour to use to fill the polygons of the base map (see
#'   `colors()`).
#' @param map.colour Colour to use for the polygon borders of the base map (see
#'   `colors()`).
#' @param map.alpha Transparency of the base map polygons. Must be between `0`
#'   (fully transparent) and `1` (fully opaque).
#' @param map.lwd Line width of the base map polygon borders.
#' @param map.lty Line type of the base map polygon borders. See
#'   [ggplot2::scale_linetype()] for common examples.
#' @param origin Should the receptor point be marked with a circle? Defaults to
#'   `TRUE`.
#'
#' @inheritDotParams ggplot2::coord_sf -xlim -ylim -crs -default_crs
#'
#' @return a `ggplot2` plot
#' @export
#'
#' @seealso the original [openair::trajPlot()]
#' @seealso [trajMap()] for the interactive `leaflet` equivalent of [trajMapStatic()]
#'
#' @examples
#' \dontrun{
#' # colour by height
#' trajMapStatic(traj_data) +
#'   ggplot2::scale_color_gradientn(colors = openair::openColours())
#'
#' # colour by PM10, log transform scale
#' trajMapStatic(traj_data, colour = "pm10") +
#'   ggplot2::scale_color_viridis_c(trans = "log10") +
#'   ggplot2::labs(color = openair::quickText("PM10"))
#'
#' # color by PM2.5, lat/lon projection
#' trajMapStatic(traj_data, colour = "pm2.5", crs = sf::st_crs(4326)) +
#'   ggplot2::scale_color_viridis_c(option = "turbo") +
#'   ggplot2::labs(color = openair::quickText("PM2.5"))
#' }
trajMapStatic <-
  function(data,
           colour = "height",
           facet = NULL,
           group = NULL,
           longitude = "lon",
           latitude = "lat",
           npoints = 12,
           xlim = NULL,
           ylim = NULL,
           crs = sf::st_crs(3812),
           origin = TRUE,
           map = TRUE,
           map.fill = "grey85",
           map.colour = "grey75",
           map.alpha = 0.8,
           map.lwd = 0.5,
           map.lty = 1,
           ...) {
    plt <-
      ggplot2::ggplot(data, ggplot2::aes(x = .data[[longitude]], y = .data[[latitude]]))

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
          ggplot2::aes(.data$long, .data$lat, group = group)
        )
    }

    if (is.null(xlim)) {
      d_lon <- diff(range(c(min(data[[longitude]]), max(data[[longitude]])))) * 0.1
      xlim <- c(min(data[[longitude]]) - d_lon, max(data[[longitude]]) + d_lon)
    }
    if (is.null(ylim)) {
      d_lat <- diff(range(c(min(data[[latitude]]), max(data[[latitude]])))) * 0.1
      ylim <- c(min(data[[latitude]]) - d_lat, max(data[[latitude]]) + d_lat)
    }

    points_df <- dplyr::filter(data, .data$hour.inc %% npoints == 0)

    if (!is.null(group)) {
      plt_aes <-
        ggplot2::aes(group = interaction(.data$date, .data[[group]]), color = .data[[colour]])
    } else {
      plt_aes <- ggplot2::aes(group = .data$date, color = .data[[colour]])
    }

    plt <- plt +
      ggplot2::geom_path(mapping = plt_aes) +
      ggplot2::geom_point(data = points_df, mapping = plt_aes) +
      ggplot2::coord_sf(
        xlim = xlim,
        ylim = ylim,
        default_crs = sf::st_crs(4326),
        crs = crs,
        ...
      ) +
      theme_static()

    if (!is.null(facet)) {
      plt <-
        plt + ggplot2::facet_wrap(ggplot2::vars(.data[[facet]]))
    }

    if (origin) {
      plt <-
        plt + ggplot2::geom_point(
          data = dplyr::filter(data, .data$hour.inc == 0) %>%
            dplyr::distinct(.data[[latitude]], .data[[longitude]]),
          size = 5,
          stroke = 1.5,
          shape = 21,
          color = "white",
          fill = "black"
        )
    }

    return(plt)
  }
