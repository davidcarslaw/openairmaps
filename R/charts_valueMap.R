
#' WIP
#' @author Jack Davison
#' @examples
#'
#'valueMap(
#' polar_data,
#' pollutant = c("no2", "pm10", "pm2.5"),
#' cols = "turbo",
#' opacity = 0.85,
#' normalise = "none"
#')
valueMap <-
  function(data,
           pollutant = "no2",
           latitude = NULL,
           longitude = NULL,
           statistic = mean,
           normalise = "none",
           cols = "Spectral",
           opacity = 1,
           ymax = NULL) {
    normalise <- rlang::arg_match(normalise, c("none", "mean", "rescale"))

    latlng <- assume_latlon(data, latitude, longitude)
    latitude <- latlng$latitude
    longitude <- latlng$longitude

    names(data)[names(data) == latitude] <- "latitude"
    names(data)[names(data) == longitude] <- "longitude"

    make_bars <- function(data, ymax) {
      ggplot2::ggplot(data, ggplot2::aes(x = name, y = value)) +
        ggplot2::geom_col(ggplot2::aes(fill = name), na.rm = TRUE) +
        ggplot2::theme_void() +
        ggplot2::theme(aspect.ratio = 1, legend.position = "none") +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::scale_fill_manual(values = pal) +
        ggplot2::scale_y_continuous(limits = c(0, ymax))
    }

    value_tbl <-
      data |>
      # dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x) statistic(x, na.rm = TRUE)),
      #                  .by = c("latitude", "longitude")) |>
      tidyr::pivot_longer(dplyr::all_of(pollutant)) |>
      dplyr::arrange(name)

    if (normalise == "rescale") {
      value_tbl <-
        dplyr::mutate(value_tbl,
                      value = scales::rescale(value, c(0, 1), c(0, max(value, na.rm = TRUE))),
                      .by = name)
    }

    if (normalise == "mean") {
      value_tbl <-
        dplyr::mutate(value_tbl,
                      value = value / mean(value, na.rm = TRUE),
                      .by = name)
    }

    pal <-
      setNames(openair::openColours(cols, n = length(unique(value_tbl$name))),
               unique(value_tbl$name))

    map_data <-
      value_tbl |>
      dplyr::nest_by(.data$latitude, .data$longitude) |>
      dplyr::mutate(plot = list(make_bars(data, ymax = ymax %||% max(value_tbl$value, na.rm = TRUE))))

    t <- tempdir()

    purrr::pmap(map_data,
                \(latitude, longitude, data, plot) {
                  ggplot2::ggsave(dpi = 300,
                                  height = 0.75,
                                  width = 0.75,
                                  filename = paste0(latitude, longitude, ".svg"),
                                  path = t,
                                  plot = plot
                  )
                })

    map <-
      leaflet::leaflet() |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::addMarkers(
        data = map_data,
        lng = map_data[["longitude"]],
        lat = map_data[["latitude"]],
        icon = ~ leaflet::makeIcon(
          iconUrl = paste0(t, "/", latitude, longitude, ".svg"),
          iconWidth = 70,
          iconHeight = 70,
          iconAnchorY = 70,
          iconAnchorX = 70 / 2
        ),
        options = leaflet::markerOptions(opacity = opacity)
      )

    if (length(pal) > 1) {
      map <-
        leaflet::addLegend(
          map = map,
          labels = quickTextHTML(names(pal)),
          colors = pal
        )
    }

    return(map)
  }
