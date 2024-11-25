#' Polar annulus plots on dynamic and static maps
#'
#' The [annulusMap()] function creates a map using polar annulus plots as
#' markers. Any number of pollutants can be specified using the `pollutant`
#' argument, and multiple layers of markers can be created using `type`. By
#' default, these maps are dynamic and can be panned, zoomed, and otherwise
#' interacted with. Using the `static` argument allows for static images to be
#' produced instead.
#'
#' @inheritSection polarMap Customisation of static maps using ggplot2
#' @family directional analysis maps
#'
#' @inheritParams polarMap
#' @param period *Temporal period for radial axis.*
#'
#'   *default:* `"hour"` | *scope:* dynamic & static
#'
#'   Options are "hour" (the default, to plot diurnal variations), "season" to
#'   plot variation throughout the year, "weekday" to plot day of the week
#'   variation and "trend" to plot the trend by wind direction.
#' @inheritDotParams openair::polarAnnulus -mydata -pollutant -period -limits
#'   -type -cols -key -plot
#' @returns Either:
#'
#'  - *Dynamic:* A leaflet object
#'  - *Static:* A `ggplot2` object using [ggplot2::coord_sf()] coordinates with a `ggspatial` basemap
#' @export
#'
#' @seealso [openair::polarAnnulus()]
#'
#' @examples
#' \dontrun{
#' annulusMap(polar_data,
#'   pollutant = "nox",
#'   period = "hour",
#'   provider = "CartoDB.Voyager"
#' )
#' }
annulusMap <- function(data,
                       pollutant = NULL,
                       period = "hour",
                       limits = "free",
                       latitude = NULL,
                       longitude = NULL,
                       crs = 4326,
                       type = NULL,
                       popup = NULL,
                       label = NULL,
                       provider = "OpenStreetMap",
                       cols = "turbo",
                       alpha = 1,
                       key = FALSE,
                       legend = TRUE,
                       legend.position = NULL,
                       legend.title = NULL,
                       legend.title.autotext = TRUE,
                       control.collapsed = FALSE,
                       control.position = "topright",
                       control.autotext = TRUE,
                       d.icon = 200,
                       d.fig = 3.5,
                       static = FALSE,
                       static.nrow = NULL,
                       progress = TRUE,
                       n.core = 1L,
                       ...,
                       control = NULL) {
  if (static) {
    rlang::check_installed("ggplot2")
  }

  # check basemap providers are valid
  provider <- check_providers(provider, static)
  legend.position <- check_legendposition(legend.position, static)

  # check for old facet/control opts
  type <- type %||% check_facet_control(control = control, ...)

  # assume lat/lon
  latlon <- assume_latlon(
    data = data,
    latitude = latitude,
    longitude = longitude
  )
  latitude <- latlon$latitude
  longitude <- latlon$longitude

  # auto limits
  limits <- check_multipoll(limits, pollutant)

  if ("fixed" %in% limits) {
    data <-
      dplyr::mutate(data, latlng = paste(.data[[latitude]], .data[[longitude]]))

    type_fixed <- type
    if (is.null(type)) {
      type_fixed <- "default"
    }

    testplots <-
      openair::polarAnnulus(
        data,
        pollutant = pollutant,
        period = period,
        type = c("latlng", type_fixed),
        plot = FALSE,
        ...
      )$data

    theLimits <- range(testplots$z, na.rm = TRUE)
  } else if ("free" %in% limits) {
    theLimits <- NA
  } else if (is.numeric(limits)) {
    theLimits <- limits
  } else {
    cli::cli_abort(
      c(
        "!" = "Do not recognise {.field limits} value of {.code {limits}}",
        "i" = "{.field limits} should be one of {.code 'fixed'}, {.code 'free'} or a numeric vector of length 2."
      )
    )
  }

  # cut data
  data <- openair::cutData(x = data, type = type %||% "default", ...)

  # deal with popups
  if (length(popup) > 1) {
    data <-
      quick_popup(
        data = data,
        popup = popup,
        latitude = latitude,
        longitude = longitude,
        control = type
      )
    popup <- "popup"
  }

  # prepare data for mapping
  data <-
    prepMapData(
      data = data,
      pollutant = pollutant,
      control = type,
      "wd",
      "date",
      latitude,
      longitude,
      popup,
      label
    )

  # identify splitting column (defaulting to pollutant)
  if (length(pollutant) > 1) {
    split_col <- "pollutant_name"
  } else if (!is.null(type)) {
    data[type] <- as.factor(data[[type]])
    split_col <- type
  } else {
    split_col <- "pollutant_name"
  }

  # define function
  fun <- function(data) {
    if (!"free" %in% limits) {
      openair::polarAnnulus(
        data,
        pollutant = "conc",
        period = period,
        plot = FALSE,
        limits = theLimits,
        cols = cols,
        alpha = alpha,
        key = key,
        ...,
        par.settings = list(axis.line = list(col = "transparent"))
      )$plot
    } else {
      openair::polarAnnulus(
        data,
        pollutant = "conc",
        period = period,
        plot = FALSE,
        cols = cols,
        alpha = alpha,
        key = key,
        ...,
        par.settings = list(axis.line = list(col = "transparent"))
      )$plot
    }
  }

  # plot and save static markers
  plots_df <-
    create_polar_markers(
      fun = fun,
      data = data,
      latitude = latitude,
      longitude = longitude,
      split_col = split_col,
      d.fig = d.fig,
      popup = popup,
      label = label,
      progress = progress,
      ncores = n.core
    )

  if (static) {
    # create static map - deals with basics & facets
    map <-
      create_static_map(
        plots_df = plots_df,
        latitude = latitude,
        longitude = longitude,
        split_col = split_col,
        pollutant = pollutant,
        facet = type,
        facet.nrow = static.nrow,
        d.icon = d.icon,
        crs = crs,
        provider = provider
      )

    # create colorbar if limits specified
    if (!all(is.na(theLimits)) & legend) {
      legend.title <-
        create_legend_title(
          static = static,
          legend.title.autotext = legend.title.autotext,
          legend.title = legend.title,
          str = paste(pollutant, collapse = ", ")
        )

      map <-
        map +
        ggplot2::geom_point(
          data = plots_df,
          ggplot2::aes(.data[[longitude]], .data[[latitude]], color = 0),
          alpha = 0
        ) +
        ggplot2::scale_color_gradientn(
          limits = theLimits,
          colours = openair::openColours(scheme = cols)
        ) +
        ggplot2::labs(color = legend.title) +
        ggplot2::theme(legend.position = legend.position)
    }
  }

  if (!static) {
    # create leaflet map
    map <-
      make_leaflet_map(
        plots_df,
        latitude,
        longitude,
        crs,
        provider,
        d.icon,
        popup,
        label,
        split_col,
        control.collapsed,
        control.position,
        control.autotext
      )

    # add legend if limits are set
    if (!all(is.na(theLimits)) & legend) {
      legend.title <-
        create_legend_title(
          static = static,
          legend.title.autotext = legend.title.autotext,
          legend.title = legend.title,
          str = paste(pollutant, collapse = ",<br>")
        )

      map <-
        leaflet::addLegend(
          map,
          title = legend.title,
          pal = leaflet::colorNumeric(
            palette = openair::openColours(scheme = cols),
            domain = theLimits
          ),
          position = legend.position,
          values = theLimits
        )
    }
  }

  # return map
  return(map)
}
