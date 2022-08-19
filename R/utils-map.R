#' Save an openair plot as a temp image to use as an icon
#' @noRd

save_icon_image <-
  function(data,
           fun,
           dir,
           pollutant,
           type,
           cols,
           alpha,
           key,
           fig.width,
           fig.height,
           ...) {
    png(
    filename = paste0(dir, "/", data[[type]][1], "_", pollutant, ".png"),
    width = fig.width * 300,
    height = fig.height * 300,
    res = 300,
    bg = "transparent"
  )

  plt <- fun(
    data,
    pollutant = pollutant,
    key = key,
    cols = cols,
    par.settings = list(axis.line = list(col = "transparent")),
    alpha = alpha,
    ...
  )

  dev.off()
}

#' Save all openair plots as images and read as leaflet icons
#' @noRd

create_icons <-
  function(data,
           fun,
           dir,
           pollutant,
           type,
           cols,
           alpha,
           key,
           fig.width,
           fig.height,
           iconWidth,
           iconHeight,
           ...) {

    # go through all sites and make some plot
    data %>%
      dplyr::group_split(across(type)) %>%
      purrr::walk(
        .f = ~ save_icon_image(
          fun = fun,
          dir = dir,
          pollutant = pollutant,
          type = type,
          cols = cols,
          alpha = alpha,
          key = key,
          fig.width = fig.width,
          fig.height = fig.height,
          ...
        )
      )

    # definition of 'icons' aka the openair plots
    leafIcons <- lapply(sort(paste0(dir, "/", unique(data[[type]]), "_", pollutant, ".png")),
      leaflet::makeIcon,
      iconWidth = iconWidth,
      iconHeight = iconHeight
    )
    names(leafIcons) <- unique(data[[type]])
    class(leafIcons) <- "leaflet_icon_set"

    leafIcons
  }
