#' Add trajectory paths to leaflet map
#'
#' This function is similar (but not identical to) the [leaflet::addMarkers()]
#' function in \code{leaflet}, which allows users to add trajectory paths to any
#' leaflet map and have more control over groups and layerIds than in
#' "all-in-one" functions like [trajMap()].
#'
#' @param map a map widget object created from [leaflet::leaflet()].
#' @param lng The decimal longitude.
#' @param lat The decimal latitude.
#' @param layerId The layer id.
#' @param group the name of the group the newly created layers should belong to
#'  (for [leaflet::clearGroup()] and [leaflet::addLayersControl()] purposes).
#'  Human-friendly group names are permitted–they need not be short,
#'  identifier-style names. Any number of layers and even different types of
#'  layers (e.g. markers and polygons) can share the same group name.
#' @param data Data frame, the result of importing a trajectory file using
#'  [openair::importTraj()].
#' @param npoints A dot is placed every \code{npoints} along each full
#'  trajectory. For hourly back trajectories points are plotted every
#'  \code{npoints} hours. This helps to understand where the air masses were at
#'  particular times and get a feel for the speed of the air (points closer
#'  together correspond to slower moving air masses). Defaults to \code{12}.
#' @param ... Other arguments to pass to both [leaflet::addCircleMarkers()] and
#'  [leaflet::addPolylines()]. If you use the \code{color} argument, it is
#'  recommended to index the input to avoid issues with plotting the lines
#'  (i.e., do \code{color = ~ pal(nox)[1]}). Note that \code{opacity} controls
#'  the opacity of the lines and \code{fillOpacity} the opacity of the markers.
#' @return A leaflet object.
#' @import leaflet
#' @importFrom rlang .data
#' @importFrom grDevices dev.off png
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' \dontrun{
#' library(leaflet)
#' library(openair)
#'
#' france <- openair::importTraj("paris", year = 2009) %>%
#'   openair::selectByDate(
#'     start = "15/4/2009",
#'     end = "21/4/2009"
#'   )
#'
#' uk <- openair::importTraj(year = 2009) %>%
#'   openair::selectByDate(
#'     start = "15/4/2009",
#'     end = "21/4/2009"
#'   )
#'
#' leaflet() %>%
#'   addTiles() %>%
#'   addTrajPaths(data = uk, color = "blue", group = "London, UK") %>%
#'   addTrajPaths(data = france, color = "red", group = "Paris, France") %>%
#'   addLayersControl(overlayGroups = c("Paris, France", "London, UK"))
#' }
addTrajPaths <-
  function(map,
           lng = "lon",
           lat = "lat",
           layerId = NULL,
           group = NULL,
           data,
           npoints = 12,
           ...) {

    # get factor version of date to reorder by "colour"
    data$datef <- factor(data$date)

    # labels
    data <- dplyr::mutate(
      data,
      lab = stringr::str_glue("<b>Arrival Date:</b> {date}<br>
                             <b>Trajectory Date:</b> {date2}<br>
                             <b>Lat:</b> {lat} | <b>Lon:</b> {lon}<br>
                             <b>Height:</b> {height} m | <b>Pressure:</b> {pressure} Pa")
    )

    for (i in seq(length(unique(data$datef)))) {
      # get line/points data
      ldata <- dplyr::filter(data, .data$datef == unique(data$datef)[[i]])
      pdata <- dplyr::filter(ldata, .data$hour.inc %% npoints == 0)

      # add points/lines to plot
      map <-
        leaflet::addPolylines(
          map = map,
          data = ldata,
          lng = ldata[[lng]],
          lat = ldata[[lat]],
          weight = 2,
          group = group,
          layerId = layerId,
          ...
        ) %>%
        leaflet::addCircleMarkers(
          data = pdata,
          radius = 3,
          stroke = F,
          lng = pdata[[lng]],
          lat = pdata[[lat]],
          group = group,
          layerId = layerId,
          popup = pdata[["lab"]],
          ...
        )
    }

    map
  }
