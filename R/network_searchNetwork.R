#' Geographically search the air quality networks made available by
#' [openair::importMeta()]
#'
#' While [networkMap()] visualises entire UK air quality networks,
#' [searchNetwork()] can subset specific networks to find air quality sites near
#' to a specific site of interest (for example, the location of known industrial
#' activity, or the centroid of a specific urban area).
#'
#' Data subsetting progresses in the order in which the arguments are given;
#' first `source` and `year`, then `site_type` and `variable`, then `max_dist`,
#' and finally `n`.
#'
#' @inheritParams openair::importMeta
#'
#' @param lat,lng The decimal latitude/longitude (or other Y/X coordinate if
#'   using a different `crs`) for the location of interest. If not provided,
#'   will be automatically inferred from data by looking for a column named
#'   "lat"/"latitude" or "lon"/"lng"/"long"/"longitude" (case-insensitively).
#' @param crs The coordinate reference system (CRS) of `lat`/`lng`, passed to
#'   [sf::st_crs()]. By default this is [EPSG:4326](https://epsg.io/4326), the
#'   CRS associated with the commonly used latitude and longitude coordinates.
#'   Different coordinate systems can be specified using `crs` (e.g., `crs =
#'   27700` for the [British National Grid](https://epsg.io/27700)). Note that
#'   non-lat/lng coordinate systems will be re-projected to EPSG:4326 for
#'   plotting on the map.
#' @param site_type Optional. One or more site types with which to subset the
#'   site metadata. For example, `site_type = "urban background"` will only
#'   search urban background sites.
#' @param variable Optional. One or more variables of interest with which to
#'   subset the site metadata. For example, `variable = c("pm10", "co")` will
#'   search sites that measure PM10 and/or CO.
#' @param max_dist Optional. A maximum distance from the location of interest in
#'   kilometres.
#' @param n Optional. The maximum number of sites to return.
#' @param map If `TRUE`, the default, [searchNetwork()] will return a `leaflet`
#'   map. If `FALSE`, it will instead return a [tibble][tibble::tibble-package].
#'
#' @order 2
#'
#' @returns Either a [tibble][tibble::tibble-package] or `leaflet` map.
#' @export
#' @family uk air quality network mapping functions
#'
#' @examples
#' \dontrun{
#' # get all AURN sites open in 2020 within 20 km of Buckingham Palace
#' palace <- convertPostcode("SW1A1AA")
#' searchNetwork(lat = palace$lat, lng = palace$lng, max_dist = 20, year = 2020)
#' }
searchNetwork <-
  function(lat,
           lng,
           source = "aurn",
           year = NULL,
           site_type = NULL,
           variable = NULL,
           max_dist = NULL,
           n = NULL,
           crs = 4326,
           map = TRUE) {
    # swap NULL to NA - to pass to openair
    if (is.null(year)) {
      year <- NA
    }
    # import chosen metadata
    meta <-
      openair::importMeta(
        source = source,
        all = TRUE,
        year = year
      ) %>%
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs = 4326, remove = FALSE
      )

    # get target SF object
    target <-
      dplyr::tibble(latitude = lat, longitude = lng) %>%
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs = crs
      ) %>%
      sf::st_transform(crs = 4326)

    # filter for site_type
    if (!is.null(site_type)) {
      meta <-
        dplyr::filter(meta, tolower(site_type) %in% tolower({{ site_type }}))
    }

    # filter for variable
    if (!is.null(variable)) {
      meta <-
        dplyr::filter(meta, tolower(variable) %in% tolower({{ variable }}))
    }

    # drop repeats from multiple variables
    meta <-
      dplyr::distinct(
        meta,
        .data$source,
        .data$code,
        .data$site,
        .data$latitude,
        .data$longitude,
        .data$site_type,
        .data$zone,
        .data$agglomeration,
        .data$geometry
      )

    # find distance
    meta$dist <- as.vector(sf::st_distance(target, meta)) / 1000

    # filter for max_dist
    if (!is.null(max_dist)) {
      buffer <-
        sf::st_buffer(target, dist = max_dist * 1000)

      meta <-
        dplyr::filter(meta, .data$dist <= max_dist)
    }

    # filter for n
    if (!is.null(n)) {
      meta <-
        dplyr::arrange(meta, .data$dist) %>%
        dplyr::slice_head(n = n)
    }

    pal <-
      leaflet::colorNumeric("viridis",
        reverse = TRUE,
        c(0, meta$dist)
      )

    leafmap <-
      leaflet::leaflet() %>%
      leaflet::addProviderTiles("CartoDB.Positron")

    if (!is.null(max_dist)) {
      leafmap <-
        leaflet::addPolygons(
          leafmap,
          data = buffer,
          weight = 1,
          fillOpacity = 0,
          color = "black"
        )
    }

    # construct html tooltip
    html <- stringr::str_glue("Showing <b>{nrow(meta)}</b> sites.<details><summary>View Constraints</summary><ul>")
    vars <- stringr::str_c(source, collapse = ", ")
    html <- stringr::str_glue("{html}<li><b>Source(s):</b> {vars}</li>")

    if (all(!is.na(year))) {
      vars <- stringr::str_glue("{min(year)} - {max(year)}")
      html <- stringr::str_glue("{html}<li><b>Year(s):</b> {vars}</li>")
    }

    if (!is.null(variable)) {
      vars <- stringr::str_c(variable, collapse = ", ") %>% quickTextHTML()
      html <- stringr::str_glue("{html}<li><b>Variables:</b> {vars}</li>")
    }

    if (!is.null(site_type)) {
      vars <- stringr::str_c(site_type, collapse = ", ") %>% quickTextHTML()
      html <- stringr::str_glue("{html}<li><b>Site Type(s):</b> {vars}</li>")
    }

    if (!is.null(max_dist)) {
      html <- stringr::str_glue("{html}<li><b>Maximum Dist:</b> {as.character(max_dist)} km</li>")
    }

    if (!is.null(n)) {
      html <- stringr::str_glue("{html}<li><b>Max Sites:</b> {n}</li>")
    }

    html <- stringr::str_glue("{html}</ul></details>")

    html <- stringr::str_wrap(html, 20)

    leafmap <-
      leafmap %>%
      leaflet::addCircleMarkers(
        data = meta,
        radius = 6,
        color = "white",
        weight = 2,
        fillColor = pal(meta$dist),
        opacity = 1,
        fillOpacity = 0.8,
        label = meta$site,
        popup = stringr::str_glue(
          "<u><b>{toupper(stringr::str_to_title(meta$site))}</b> ({meta$code})</u><br>
      <b>Lat:</b> {meta$latitude} | <b>Lon:</b> {meta$longitude}<br>
      <b>Network:</b> {toupper(meta$source)}<br>
      <b>Site Type:</b> {meta$site_type}"
        )
      ) %>%
      leaflet::addMarkers(
        data = target,
        label = "Target",
        popup = stringr::str_glue("<b><u>TARGET</u></b><br> <b>Latitude:</b> {sf::st_coordinates(target$geometry)[1,'Y']}<br> <b>Longitude:</b> {sf::st_coordinates(target$geometry)[1,'X']}")
      ) %>%
      leaflet::addControl(
        position = "bottomright",
        html = html
      ) %>%
      leaflet::addLegend(
        pal = pal,
        values = c(0, meta$dist),
        title = "Distance<br>from marker<br>(km)"
      ) %>%
      leaflet::addScaleBar(position = "bottomleft")

    if (map) {
      return(leafmap)
    } else {
      meta <-
        meta %>%
        dplyr::tibble() %>%
        dplyr::select(-"geometry")

      return(meta)
    }
  }
