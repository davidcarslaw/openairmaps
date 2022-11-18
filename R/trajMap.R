#' Trajectory line plots in leaflet
#'
#' This function plots back trajectories on a \code{leaflet} map. This function
#' requires that data are imported using the [openair::importTraj()] function.
#' Options are provided to colour the individual trajectories (e.g., by
#' pollutant concentrations) or create "layer control" menus to show/hide
#' different layers.
#'
#' @seealso Trajectory maps: [trajMap()], [trajLevelMap()].
#'
#' @param data Data frame, the result of importing a trajectory file using
#'   [openair::importTraj()].
#' @param longitude Column containing the longitude, as a decimal.
#' @param latitude Column containing the latitude, as a decimal.
#' @param colour Column to be used for colouring each trajectory. This column
#'   may be numeric, character or factor. This will commonly be a pollutant
#'   concentration which has been joined (e.g., by [dplyr::left_join()]) to the
#'   trajectory data by \dQuote{date}.
#' @param control Column to be used for splitting the trajectories into
#'   different groups which can be selected between using a "layer control"
#'   menu.
#' @param cols Colours to be used for plotting. Options include
#'   \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet} and
#'   \code{RColorBrewer} colours — see the [openair::openColours()] function for
#'   more details. For user defined the user can supply a list of colour names
#'   recognised by R (type [grDevices::colours()] to see the full list). An
#'   example would be \code{cols = c("yellow", "green", "blue")}. If the
#'   \code{"colour"} argument was not used, a single colour can be named which
#'   will be used consistently for all lines/points (e.g., \code{cols = "red"}).
#' @param alpha Opacity of lines/points. Must be between \code{0} and \code{1}.
#' @param npoints A dot is placed every \code{npoints} along each full
#'   trajectory. For hourly back trajectories points are plotted every
#'   \code{npoints} hours. This helps to understand where the air masses were at
#'   particular times and get a feel for the speed of the air (points closer
#'   together correspond to slower moving air masses). Defaults to \code{12}.
#' @param provider The base map to be used. See
#'   \url{http://leaflet-extras.github.io/leaflet-providers/preview/} for a list
#'   of all base maps that can be used.
#' @param collapse.control Should the "layer control" interface be collapsed?
#'   Defaults to \code{FALSE}.
#'
#' @return A leaflet object.
#' @export
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
    if (control == "default") data$default <- "default"

    # initialise map
    map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(provider = provider)

    # if "colour", create colour palette
    if (!missing(colour)) {
      if (colour %in% names(data)) {
        data$datef <- forcats::fct_reorder(data$datef, data[[colour]], .desc = F, na.rm = T)
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
