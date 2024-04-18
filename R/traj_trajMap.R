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
#' @param data *A data frame containing a HYSPLIT trajectory, perhaps accessed
#'   with [openair::importTraj()].*
#'
#'   **required**
#'
#'   A data frame containing HYSPLIT model outputs. If this data were not
#'   obtained using [openair::importTraj()].
#'
#' @param latitude,longitude *The decimal latitude/longitude.*
#'
#'  *default:* `"lat"` / `"lon"`
#'
#'   Column names representing the decimal latitude and longitude.
#'
#' @param colour *Column to be used for colouring each trajectory.*
#'
#'  *default:* `NULL`
#'
#'   This column may be numeric, character, factor or date(time). This will
#'   commonly be a pollutant concentration which has been joined (e.g., by
#'   [dplyr::left_join()]) to the trajectory data by "date".
#'
#' @param type *A method to condition the `data` for separate plotting.*
#'
#'  *default:* `NULL`
#'
#'   Used for splitting the trajectories into different groups which can be
#'   selected between using a "layer control" menu. Passed to
#'   [openair::cutData()].
#'
#' @param cols *Colours to use for plotting.*
#'
#'  *default:* `"turbo"`
#'
#'   The colours used for plotting, passed to [openair::openColours()].
#'
#' @param alpha *Transparency value for trajectories.*
#'
#'  *default:* `1`
#'
#'   A value between `0` (fully transparent) and `1` (fully opaque).
#'
#' @param npoints *Interval at which points are placed along the trajectory
#'   paths.*
#'
#'  *default:* `12`
#'
#'   A dot is placed every `npoints` along each full trajectory. For hourly back
#'   trajectories points are plotted every `npoints` hours. This helps to
#'   understand where the air masses were at particular times and get a feel for
#'   the speed of the air (points closer together correspond to slower moving
#'   air masses). Defaults to `12`.
#'
#' @param provider *The basemap to be used.*
#'
#'  *default:* `"OpenStreetMap"`
#'
#'   A single [leaflet::providers]. See
#'   <http://leaflet-extras.github.io/leaflet-providers/preview/> for a list of
#'   all base maps that can be used.
#'
#' @param collapse.control *Show the layer control as a collapsed?*
#'
#'  *default:* `FALSE`
#'
#'   Should the "layer control" interface be collapsed? If `TRUE`, users will
#'   have to hover over an icon to view the options.
#'
#' @param control Deprecated. Please use `type`.
#'
#' @returns A leaflet object.
#' @export
#'
#' @seealso [openair::trajPlot()]
#' @seealso [trajMapStatic()] for the static `ggplot2` equivalent of [trajMap()]
#'
#' @examples
#' \dontrun{
#' trajMap(traj_data, colour = "pm10")
#' }
#'
trajMap <-
  function(data,
           longitude = "lon",
           latitude = "lat",
           colour = NULL,
           type = NULL,
           cols = "default",
           alpha = .5,
           npoints = 12,
           provider = "OpenStreetMap",
           collapse.control = FALSE,
           control = NULL) {
    # handle deprecated argument
    if (!is.null(control)) {
      lifecycle::deprecate_soft(
        when = "0.9.0",
        what = "trajMap(control)",
        with = "trajMap(type)"
      )
    }
    type <- type %||% facet

    # make lat/lon easier to use
    names(data)[names(data) == longitude] <- "lon"
    names(data)[names(data) == latitude] <- "lat"

    # default colour is black
    fixedcol <- "black"

    # get factor version of date to reorder by "colour"
    data$datef <- factor(data$date)

    # if no "type", get a fake column
    data <- quick_cutdata(data, type)
    type <- type %||% "default"

    # initialise map
    map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(provider = provider)

    # if "colour", create colour palette
    if (!is.null(colour)) {
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

    if (!is.null(colour)) {
      if (colour %in% names(data) & !colour %in% c("date", "date2", "lat", "lon", "height", "pressure")) {
        data$lab <- paste(
          data$lab,
          paste0("<b>", quickTextHTML(colour), ":</b> ", data[[colour]]),
          sep = "<br>"
        )
      }
    }

    # iterate over columns in "type" column
    for (j in seq(length(unique(data[[type]])))) {
      # get jth instance of "type"
      data2 <- dplyr::filter(data, .data[[type]] == unique(data[[type]])[[j]])

      # iterate over different arrival dates to plot separate trajectories
      for (i in seq(length(unique(data2$datef)))) {
        # get line/points data
        ldata <- dplyr::filter(data2, .data$datef == unique(data2$datef)[[i]])
        pdata <- dplyr::filter(ldata, .data$hour.inc %% npoints == 0)

        lcolors <- fixedcol
        pcolors <- fixedcol
        # apply color pal if it exists
        if (!is.null(colour)) {
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
            group = as.character(unique(data[[type]])[[j]])
          ) %>%
          leaflet::addCircleMarkers(
            data = pdata,
            radius = 3, stroke = F,
            lng = pdata$lon,
            lat = pdata$lat,
            fillOpacity = alpha,
            color = pcolors,
            group = as.character(unique(data[[type]])[[j]]),
            popup = pdata$lab
          )
      }
    }

    # if "group" exists, add a legend
    if (!is.null(colour)) {
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
    if (type != "default") {
      map <-
        leaflet::addLayersControl(
          map,
          options = leaflet::layersControlOptions(collapsed = collapse.control),
          overlayGroups = as.character(unique(data[[type]]))
        )
    }

    map
  }

#' Trajectory line plots in `ggplot2`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function plots back trajectories using `ggplot2`. The function
#'   requires that data are imported using [openair::importTraj()]. It is a
#'   `ggplot2` implementation of [openair::trajPlot()] with many of the same
#'   arguments, which should be more flexible for post-hoc changes.
#'
#' @family static trajectory maps
#'
#' @inheritParams trajMap
#'
#' @param type *A method to condition the `data` for separate plotting.*
#'
#'  *default:* `NULL`
#'
#'   Used for splitting the trajectories into different groups which will appear
#'   as different panels. Passed to [openair::cutData()].
#'
#' @param group *Column to use to distinguish different trajectory paths.*
#'
#'  *default:* `NULL`
#'
#'   By default, trajectory paths are distinguished using the arrival date.
#'   `group` allows for additional columns to be used (e.g., `"receptor"` if
#'   multiple receptors are being plotted).
#'
#' @param xlim,ylim *The x- and y-limits of the plot.*
#'
#'  *default:* `NULL`
#'
#'   A numeric vector of length two defining the x-/y-limits of the map, passed
#'   to [ggplot2::coord_sf()]. If `NULL`, limits will be estimated based on the
#'   lat/lon ranges of the input data.
#'
#' @param crs *The coordinate reference system (CRS) into which all data should
#'   be projected before plotting.*
#'
#'   *default:* `sf::st_crs(3812)`
#'
#'   This argument defaults to the Lambert projection, but can take any
#'   coordinate reference system to pass to the `crs` argument of
#'   [ggplot2::coord_sf()]. Alternatively, `crs` can be set to `NULL`, which
#'   will typically render the map quicker but may cause countries far from the
#'   equator or large areas to appear distorted.
#'
#' @param map *Draw a base map?*
#'
#'  *default:* `TRUE`
#'
#'   Draws the geometries of countries under the trajectory paths.
#'
#' @param map.fill *Colour to use to fill the polygons of the base map.*
#'
#'  *default:* `"grey85"`
#'
#'   See `colors()` for colour options. Alternatively, a hexadecimal color code
#'   can be provided.
#'
#' @param map.colour *Colour to use for the polygon borders of the base map.*
#'
#'  *default:* `"grey75"`
#'
#'   See `colors()` for colour options. Alternatively, a hexadecimal color code
#'   can be provided.
#'
#' @param map.alpha *Transparency of the base map polygons.*
#'
#'  *default:* `0.8`
#'
#'   Must be between `0` (fully transparent) and `1` (fully opaque).
#'
#' @param map.lwd *Line width of the base map polygon borders.*
#'
#'  *default:* `0.5`
#'
#'   Any numeric value.
#'
#' @param map.lty *Line type of the base map polygon borders.*
#'
#'  *default:* `1`
#'
#'   See [ggplot2::scale_linetype()] for common examples. The default, `1`,
#'   draws solid lines.
#'
#' @param origin *Draw the receptor point as a circle?*
#'
#'  *default:* `TRUE`
#'
#'  When `TRUE`, the receptor point(s) are marked with black circles.
#'
#' @param facet Deprecated. Please use `type`.
#'
#' @inheritDotParams ggplot2::coord_sf -xlim -ylim -crs -default_crs
#'
#' @returns a `ggplot2` plot
#' @export
#'
#' @seealso [openair::trajPlot()]
#' @seealso [trajMap()] for the interactive `leaflet` equivalent of
#'   [trajMapStatic()]
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
           type = NULL,
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
           facet = NULL,
           ...) {
    # handle deprecated argument
    if (!is.null(facet)) {
      lifecycle::deprecate_soft(
        when = "0.9.0",
        what = "trajMapStatic(facet)",
        with = "trajMapStatic(type)"
      )
    }
    type <- type %||% facet

    # cut data
    data <- quick_cutdata(data, type)

    # make plot
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

    plt <- plt +
      ggplot2::geom_path(mapping = plt_aes) +
      ggplot2::geom_point(data = points_df, mapping = plt_aes) +
      theme_static() +
      coords

    if (!is.null(type)) {
      plt <-
        plt + ggplot2::facet_wrap(ggplot2::vars(.data[[type]]))
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
