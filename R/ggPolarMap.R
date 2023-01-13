
ggPolarMap <- function(data,
                       pollutant = NULL,
                       x = "ws",
                       facet = NULL,
                       limits = NULL,
                       latitude = NULL,
                       longitude = NULL,
                       cols = "turbo",
                       alpha = 1,
                       key = FALSE,
                       d.icon = 150,
                       d.fig = 3,
                       ...) {
  # assume lat/lon
  latlon <- assume_latlon(
    data = data,
    latitude = latitude,
    longitude = longitude
  )
  latitude <- latlon$latitude
  longitude <- latlon$longitude

  # deal with limits
  theLimits <- limits
  if (is.null(limits)) theLimits <- NA

  # if no "facet", create a dummy column
  if (is.null(facet)) {
    facet <- "default"
    data$default <- "default"
  }

  # create plots
  plots_df <-
    data %>%
    dplyr::nest_by(.data[[latitude]], .data[[longitude]], .data[[facet]]) %>%
    dplyr::mutate(plot = list(
      openair::polarPlot(
        .data$data,
        pollutant = pollutant,
        plot = FALSE,
        limits = theLimits,
        cols = cols,
        alpha = alpha,
        key = key,
        ...,
        par.settings = list(axis.line = list(col = "transparent"))
      )$plot
    ))

  dir <- tempdir()

  purrr::pwalk(list(plots_df[[latitude]], plots_df[[longitude]], plots_df[[facet]], plots_df$plot),
               .f = ~ {
                 grDevices::png(
                   filename = paste0(dir, "/", ..1, "_", ..2, "_", ..3, ".png"),
                   width = d.fig * 300,
                   height = d.fig * 300,
                   res = 300,
                   bg = "transparent"
                 )

                 plot(..4)

                 grDevices::dev.off()
               })

  lat_d <- abs(diff(range(plots_df[[latitude]])) / 5)
  minlat <- min(plots_df[[latitude]]) - 0.05
  maxlat <- max(plots_df[[latitude]]) + 0.05

  lon_d <- abs(diff(range(plots_df[[longitude]])) / 5)
  minlon <- min(plots_df[[longitude]]) - 0.05
  maxlon <- max(plots_df[[longitude]]) + 0.05

  map <-
    ggmap::get_stamenmap(bbox = c(minlon, minlat, maxlon, maxlat),
                         zoom = 13)

  plt <-
    ggmap::ggmap(map) +
    ggtext::geom_richtext(
      data = dplyr::mutate(
        plots_df,
        url = paste0(
          dir, "/", .data[[latitude]], "_", .data[[longitude]], "_", .data[[facet]], ".png"
        ),
        url = stringr::str_glue("<img src='{url}' width='{d.icon}'/>")),
      ggplot2::aes(.data[[longitude]], .data[[latitude]], label = .data$url),
      fill = NA,
      color = NA
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(
      panel.ontop = TRUE,
      panel.border = ggplot2::element_rect(fill = NA, color = "black")
    )

  if (facet != "default") {
    plt <- plt + ggplot2::facet_wrap(ggplot2::vars(.data[[facet]]))
  }

  if (!is.null(limits)) {
    plt <-
      plt  +
      ggplot2::geom_point(ggplot2::aes(.data[[longitude]], .data[[latitude]], color = 0),
                          alpha = 0) +
      ggplot2::scale_color_gradientn(limits = theLimits,
                                     colours = openair::openColours(scheme = cols)) +
      ggplot2::labs(color = openair::quickText(pollutant))
  }

  return(plt)
}
