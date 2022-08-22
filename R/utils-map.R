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
    # where to write files
    icon_dir <- tempdir()

    # go through all sites and make some plot
    data %>%
      dplyr::group_split(across(type)) %>%
      purrr::walk(
        .f = ~ save_icon_image(
          fun = fun,
          dir = icon_dir,
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
    leafIcons <-
      lapply(sort(paste0(
        icon_dir, "/", unique(data[[type]]), "_", pollutant, ".png"
      )),
      leaflet::makeIcon,
      iconWidth = iconWidth,
      iconHeight = iconHeight
      )
    names(leafIcons) <- unique(data[[type]])
    class(leafIcons) <- "leaflet_icon_set"

    leafIcons
  }

#' Prep data for mapping
#' @noRd

prepMapData <- function(data, type, ...) {

  ## extract variables of interest
  vars <- c(...)

  if (type != "default") {
    vars <- c(vars, type)
  }

  # check and select variables
  data <- openair:::checkPrep(data, vars, type = type)

  # cut data
  data <- openair::cutData(data, type)

  # remove missing data
  data <- na.omit(data)

  # check to see if variables exist in data
  if (length(intersect(vars, names(data))) != length(vars)) {
    stop(paste(vars[which(!vars %in% names(data))], "not found in data"), call. = FALSE)
  }

  return(data)
}

#' Make a leaflet map
#' @noRd

makeMap <- function(data, icons, provider, longitude, latitude, type, pollutant) {

  provider = unique(provider)

  # data for plotting
  plot_data <-
    dplyr::group_by(data, .data[[type]]) %>%
    dplyr::slice(n = 1) %>%
    dplyr::arrange(.data[[type]])

  # create leaflet map
  m <- leaflet::leaflet(data = plot_data)

  # add tiles
  for (j in seq(length(provider))) {
    m <- leaflet::addProviderTiles(
      map = m,
      provider = provider[j],
      group = provider[j]
    )
  }

  # add markers
  for (i in seq(length(icons))) {
    m <- leaflet::addMarkers(
      m,
      data = plot_data,
      lng = plot_data[[longitude]],
      lat = plot_data[[latitude]],
      icon = icons[[i]],
      popup = plot_data[[type]],
      group = sort(pollutant)[[i]] %>% quickTextHTML()
    )
  }

  # add layer control for pollutants/providers
  if (length(pollutant) > 1 & length(provider) > 1) {
    m <-
      leaflet::addLayersControl(
        m,
        baseGroups = sort(pollutant) %>% purrr::map_chr(quickTextHTML),
        overlayGroups = provider
      )
  } else if (length(pollutant) > 1 & length(provider) == 1) {
    m <- leaflet::addLayersControl(
      m,
      baseGroups = sort(pollutant) %>% purrr::map_chr(quickTextHTML)
    )
  } else if (length(provider) > 1 & length(pollutant) == 1) {
    m <- leaflet::addLayersControl(
      m,
      baseGroups = provider
    )
  }

  # return
  return(m)
}
