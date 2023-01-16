
#' theme for static maps
#' @noRd
theme_static <- function() {
  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(),
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, color = "black"))
  )
}

#' Create markers for the static plots
#' @param fun function of "data" to create plot
#' @param dir directory (created in function)
#' @param latitude,longitude,split_col,d.fig inherited from parent
#' @noRd
create_static_markers <-
  function(fun,
           data = data,
           dir = tempdir,
           latitude = latitude,
           longitude = longitude,
           split_col = split_col,
           d.fig) {
    # create plots
    plots_df <-
      data %>%
      tidyr::drop_na(.data$conc) %>%
      dplyr::nest_by(.data[[latitude]], .data[[longitude]], .data[[split_col]]) %>%
      dplyr::mutate(plot = list(try(silent = TRUE, fun(.data$data))
      ),
      plot = dplyr::if_else(
        inherits(plot, "try-error"),
        list(ggplot2::ggplot() +
               ggplot2::theme_minimal()),
        list(plot)
      ))

    purrr::pwalk(list(plots_df[[latitude]], plots_df[[longitude]], plots_df[[split_col]], plots_df$plot),
                 .f = ~ {
                   grDevices::png(
                     filename = paste0(dir, "/", ..1, "_", ..2, "_", ..3, ".png"),
                     width = d.fig * 300,
                     height = d.fig * 300,
                     res = 300,
                     bg = "transparent",
                     type = "cairo",
                     antialias = "none"
                   )

                   plot(..4)

                   grDevices::dev.off()
                 })

    return(plots_df)
  }

#' if ggmap is not provided, have a guess
#' @param data `plots_df` input
#' @param ggmap,latitude,longitude,zoom inherited from parent
#' @noRd
estimate_ggmap <-
  function(ggmap = ggmap,
           data,
           latitude = latitude,
           longitude = longitude,
           zoom = zoom) {
    if (is.null(ggmap)) {
      lat_d <- abs(diff(range(data[[latitude]])) / 2)
      minlat <- min(data[[latitude]]) - lat_d
      maxlat <- max(data[[latitude]]) + lat_d

      lon_d <- abs(diff(range(data[[longitude]])) / 2)
      minlon <- min(data[[longitude]]) - lon_d
      maxlon <- max(data[[longitude]]) + lon_d

      ggmap <-
        ggmap::get_stamenmap(bbox = c(minlon, minlat, maxlon, maxlat),
                             zoom = zoom)
    }

    return(ggmap)
  }

#' Create static map
#' @param ggmap:facet.nrow inherited from parent
#' @param plots_df `plots_df`
#' @noRd
create_static_map <-
  function(ggmap,
           plots_df,
           dir,
           latitude,
           longitude,
           split_col,
           pollutant,
           d.icon,
           facet,
           facet.nrow) {
    plt <-
      ggmap::ggmap(ggmap) +
      ggtext::geom_richtext(
        data = dplyr::mutate(
          plots_df,
          url = paste0(dir, "/", .data[[latitude]], "_", .data[[longitude]], "_", .data[[split_col]], ".png"),
          url = stringr::str_glue("<img src='{url}' width='{d.icon}'/>")
        ),
        ggplot2::aes(.data[[longitude]], .data[[latitude]], label = .data$url),
        fill = NA,
        color = NA
      ) +
      ggplot2::labs(x = NULL, y = NULL) +
      theme_static()

    if (length(pollutant) > 1 | !is.null(facet)) {
      plt <-
        plt + ggplot2::facet_wrap(ggplot2::vars(quickTextHTML(.data[[split_col]])), nrow = facet.nrow) +
        ggplot2::theme(strip.text = ggtext::element_markdown())
    }

    return(plt)
  }


