#' Function for back-compatibility with the facet/control args
#' @param ... inherited from parent function
#' @noRd
check_facet_control <- function(...) {
  dots <- rlang::list2(...)

  if ("control" %in% names(dots)) {
    lifecycle::deprecate_warn(
      env = rlang::caller_env(),
      user_env = rlang::caller_env(2),
      when = "0.9.0",
      what = "polarMap(control)",
      with = "polarMap(type)",
      details = "This change has been made for better consistency with openair, and between dynamic and static maps."
    )
    return(dots$control)
  }

  if ("facet" %in% names(dots)) {
    lifecycle::deprecate_warn(
      env = rlang::caller_env(),
      user_env = rlang::caller_env(2),
      when = "0.9.0",
      what = "polarMapStatic(facet)",
      with = "polarMap(type)",
      details = "This change has been made for better consistency with openair, and between dynamic and static maps. Note that static maps can now be produced using the 'static' argument of polarMap()"
    )
    return(dots$facet)
  }

  return(NULL)
}

#' Deprecated static directional analysis functions
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#'  Static direction analysis mapping functions have been deprecated in favour
#'  of combined functions (e.g., `polarMap()`), which present a more consistent,
#'  unified API for users to simply swap between the two output formats.
#'
#' @family deprecated functions
#' @rdname deprecated-static-polar-maps
#' @inheritParams polarMap
#' @param facet Passed to the `type` argument of the relevant `polarMap()` family
#'  function.
#' @param ... Passed to the polar plotting function
#' @order 1
#' @seealso [polarMap()]
#'
#' @returns a `ggplot2` object using [ggplot2::coord_sf()] coordinates with a
#'  `ggspatial` basemap
#' @export
polarMapStatic <- function(data,
                           pollutant = NULL,
                           x = "ws",
                           limits = "free",
                           upper = "fixed",
                           latitude = NULL,
                           longitude = NULL,
                           crs = 4326,
                           provider = "osm",
                           facet = NULL,
                           cols = "turbo",
                           alpha = 1,
                           key = FALSE,
                           facet.nrow = NULL,
                           d.icon = 150,
                           d.fig = 3,
                           ...) {
  lifecycle::deprecate_soft(
    when = "0.9.0",
    what = "polarMapStatic()",
    with = "polarMap()",
    details = "The `static` argument in `polarMap()` can now create static outputs."
  )

  polarMap(
    data = data,
    pollutant = pollutant,
    x = x,
    limits = limits,
    upper = upper,
    latitude = latitude,
    longitude = longitude,
    crs = crs,
    provider = provider,
    type = facet,
    cols = cols,
    alpha = alpha,
    key = key,
    static.nrow = facet.nrow,
    d.icon = d.icon,
    d.fig = d.fig,
    static = TRUE,
    ...
  )
}

#' @family deprecated functions
#' @rdname deprecated-static-polar-maps
#' @inheritParams annulusMap
#' @export
annulusMapStatic <- function(data,
                             pollutant = NULL,
                             period = "hour",
                             facet = NULL,
                             limits = "free",
                             latitude = NULL,
                             longitude = NULL,
                             crs = 4326,
                             provider = "osm",
                             cols = "turbo",
                             alpha = 1,
                             key = FALSE,
                             facet.nrow = NULL,
                             d.icon = 150,
                             d.fig = 3,
                             ...) {
  lifecycle::deprecate_soft(
    when = "0.9.0",
    what = "annulusMapStatic()",
    with = "annulusMap()",
    details = "The `static` argument in `annulusMap()` can now create static outputs."
  )

  annulusMap(
    data = data,
    pollutant = pollutant,
    period = period,
    type = facet,
    limits = limits,
    latitude = latitude,
    longitude = longitude,
    crs = crs,
    provider = provider,
    cols = cols,
    alpha = alpha,
    key = key,
    static.nrow = facet.nrow,
    d.icon = d.icon,
    d.fig = d.fig,
    static = TRUE,
    ...
  )
}

#' @family deprecated functions
#' @rdname deprecated-static-polar-maps
#' @inheritParams windroseMap
#' @export
windroseMapStatic <- function(data,
                              ws.int = 2,
                              breaks = 4,
                              facet = NULL,
                              latitude = NULL,
                              longitude = NULL,
                              crs = 4326,
                              provider = "osm",
                              cols = "turbo",
                              alpha = 1,
                              key = FALSE,
                              facet.nrow = NULL,
                              d.icon = 150,
                              d.fig = 3,
                              ...) {
  lifecycle::deprecate_soft(
    when = "0.9.0",
    what = "windroseMapStatic()",
    with = "windroseMap()",
    details = "The `static` argument in `windroseMap()` can now create static outputs."
  )

  windroseMap(
    data = data,
    ws.int = ws.int,
    breaks = breaks,
    type = facet,
    latitude = latitude,
    longitude = longitude,
    crs = crs,
    provider = provider,
    cols = cols,
    alpha = alpha,
    key = key,
    static.nrow = facet.nrow,
    d.icon = d.icon,
    d.fig = d.fig,
    static = TRUE,
    ...
  )
}

#' @family deprecated functions
#' @rdname deprecated-static-polar-maps
#' @inheritParams pollroseMap
#' @export
pollroseMapStatic <- function(data,
                              pollutant = NULL,
                              statistic = "prop.count",
                              breaks = NULL,
                              facet = NULL,
                              latitude = NULL,
                              longitude = NULL,
                              crs = 4326,
                              provider = "osm",
                              cols = "turbo",
                              alpha = 1,
                              key = FALSE,
                              facet.nrow = NULL,
                              d.icon = 150,
                              d.fig = 3,
                              ...) {
  lifecycle::deprecate_soft(
    when = "0.9.0",
    what = "pollroseMapStatic()",
    with = "pollroseMap()",
    details = "The `static` argument in `pollroseMap()` can now create static outputs."
  )

  pollroseMap(
    data = data,
    pollutant = pollutant,
    statistic = statistic,
    breaks = breaks,
    type = facet,
    latitude = latitude,
    longitude = longitude,
    crs = crs,
    provider = provider,
    cols = cols,
    alpha = alpha,
    key = key,
    static.nrow = facet.nrow,
    d.icon = d.icon,
    d.fig = d.fig,
    static = TRUE,
    ...
  )
}

#' @family deprecated functions
#' @rdname deprecated-static-polar-maps
#' @inheritParams percentileMap
#' @export
percentileMapStatic <- function(data,
                                pollutant = NULL,
                                percentile = c(25, 50, 75, 90, 95),
                                intervals = "fixed",
                                latitude = NULL,
                                longitude = NULL,
                                crs = 4326,
                                provider = "osm",
                                facet = NULL,
                                cols = "turbo",
                                alpha = 1,
                                key = FALSE,
                                facet.nrow = NULL,
                                d.icon = 150,
                                d.fig = 3,
                                ...) {
  lifecycle::deprecate_soft(
    when = "0.9.0",
    what = "percentileMapStatic()",
    with = "percentileMap()",
    details = "The `static` argument in `percentileMap()` can now create static outputs."
  )

  percentileMap(
    data = data,
    pollutant = pollutant,
    percentile = percentile,
    intervals = intervals,
    latitude = latitude,
    longitude = longitude,
    crs = crs,
    provider = provider,
    type = facet,
    cols = cols,
    alpha = alpha,
    key = key,
    static.nrow = facet.nrow,
    d.icon = d.icon,
    d.fig = d.fig,
    static = TRUE,
    ...
  )
}

#' @family deprecated functions
#' @rdname deprecated-static-polar-maps
#' @inheritParams freqMap
#' @export
freqMapStatic <- function(data,
                          pollutant = NULL,
                          statistic = "mean",
                          breaks = "free",
                          latitude = NULL,
                          longitude = NULL,
                          crs = 4326,
                          provider = "osm",
                          facet = NULL,
                          cols = "turbo",
                          alpha = 1,
                          key = FALSE,
                          facet.nrow = NULL,
                          d.icon = 150,
                          d.fig = 3,
                          ...) {
  lifecycle::deprecate_soft(
    when = "0.9.0",
    what = "freqMapStatic()",
    with = "freqMap()",
    details = "The `static` argument in `freqMap()` can now create static outputs."
  )

  freqMap(
    data = data,
    pollutant = pollutant,
    statistic = statistic,
    breaks = breaks,
    latitude = latitude,
    longitude = longitude,
    crs = crs,
    provider = provider,
    type = facet,
    cols = cols,
    alpha = alpha,
    key = key,
    static.nrow = facet.nrow,
    d.icon = d.icon,
    d.fig = d.fig,
    static = TRUE,
    ...
  )
}

#' @family deprecated functions
#' @rdname deprecated-static-polar-maps
#' @inheritParams diffMap
#' @order 7
#' @export
diffMapStatic <- function(before,
                          after,
                          pollutant = NULL,
                          limits = "free",
                          x = "ws",
                          latitude = NULL,
                          longitude = NULL,
                          crs = 4326,
                          provider = "osm",
                          facet = NULL,
                          cols = c(
                            "#002F70",
                            "#3167BB",
                            "#879FDB",
                            "#C8D2F1",
                            "#F6F6F6",
                            "#F4C8C8",
                            "#DA8A8B",
                            "#AE4647",
                            "#5F1415"
                          ),
                          alpha = 1,
                          key = FALSE,
                          facet.nrow = NULL,
                          d.icon = 150,
                          d.fig = 3,
                          ...) {
  diffMap(
    before = before,
    after = after,
    pollutant = pollutant,
    limits = limits,
    x = x,
    latitude = latitude,
    longitude = longitude,
    crs = crs,
    provider = provider,
    type = facet,
    cols = cols,
    alpha = alpha,
    key = key,
    static.nrow = facet.nrow,
    d.icon = d.icon,
    d.fig = d.fig,
    static = TRUE,
    ...
  )
}
