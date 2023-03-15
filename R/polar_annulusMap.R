#' Polar annulus plots on interactive leaflet maps
#'
#' [annulusMap()] creates a `leaflet` map using polar annulus plots as markers.
#' Any number of pollutants can be specified using the `pollutant` argument, and
#' multiple layers of markers can be added and toggled between using `control`.
#'
#' @family interactive directional analysis maps
#'
#' @inheritParams polarMap
#' @param period This determines the temporal period to consider. Options are
#'   "hour" (the default, to plot diurnal variations), "season" to plot
#'   variation throughout the year, "weekday" to plot day of the week variation
#'   and "trend" to plot the trend by wind direction.
#' @inheritDotParams openair::polarAnnulus -mydata -pollutant -period -limits
#'   -type -cols -key -plot
#' @return A leaflet object.
#' @export
#'
#' @seealso the original [openair::polarAnnulus()]
#' @seealso [annulusMapStatic()] for the static `ggmap` equivalent of
#'   [annulusMap()]
#'
#' @examples
#' \dontrun{
#' annulusMap(polar_data,
#'   pollutant = "nox",
#'   period = "hour",
#'   provider = "Stamen.Toner"
#' )
#' }
annulusMap <- function(data,
                       pollutant = NULL,
                       period = "hour",
                       limits = NULL,
                       latitude = NULL,
                       longitude = NULL,
                       control = NULL,
                       popup = NULL,
                       label = NULL,
                       provider = "OpenStreetMap",
                       cols = "turbo",
                       alpha = 1,
                       key = FALSE,
                       draw.legend = TRUE,
                       collapse.control = FALSE,
                       d.icon = 200,
                       d.fig = 3.5,
                       type = deprecated(),
                       ...) {
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_soft(
      when = "0.5.0",
      what = "openairmaps::annulusMap(type)",
      details = c("Different sites are now automatically detected based on latitude and longitude",
                  "Please use the `popup` argument to create popups.")
    )
  }

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

  # cut data
  data <- quick_cutdata(data = data, type = control)

  # deal with popups
  if (length(popup) > 1) {
    data <-
      quick_popup(
        data = data,
        popup = popup,
        latitude = latitude,
        longitude = longitude,
        control = control
      )
    popup <- "popup"
  }

  # prepare data for mapping
  data <-
    prepMapData(
      data = data,
      pollutant = pollutant,
      control = control,
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
  } else if (!is.null(control)) {
    data[control] <- as.factor(data[[control]])
    split_col <- control
  } else {
    split_col <- "pollutant_name"
  }

  # define function
  fun <- function(data) {
    if (!is.null(limits)) {
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
      label = label
    )

  # create leaflet map
  map <-
    make_leaflet_map(plots_df, latitude, longitude, provider, d.icon, popup, label, split_col, collapse.control)

  # add legend if limits are set
  if (!is.null(limits) & all(!is.na(limits)) & draw.legend) {
    map <-
      leaflet::addLegend(
        map,
        title = quickTextHTML(paste(pollutant, collapse = ",<br>")),
        pal = leaflet::colorNumeric(
          palette = openair::openColours(scheme = cols),
          domain = theLimits
        ),
        values = theLimits
      )
  }

  # return map
  return(map)
}

#' Bivariate polar plots on a static ggmap
#'
#' [annulusMapStatic()] creates a `ggplot2` map using polar annulus plots as
#' markers. As this function returns a `ggplot2` object, further customisation
#' can be achieved using functions like [ggplot2::theme()] and
#' [ggplot2::guides()].
#'
#' @inheritSection polarMapStatic Further customisation using ggplot2
#'
#' @family static directional analysis maps
#'
#' @inheritParams polarMapStatic
#' @param period This determines the temporal period to consider. Options are
#'   "hour" (the default, to plot diurnal variations), "season" to plot
#'   variation throughout the year, "weekday" to plot day of the week variation
#'   and "trend" to plot the trend by wind direction.
#' @inheritDotParams openair::polarAnnulus -mydata -pollutant -period -limits
#'   -type -cols -key -plot
#'
#' @seealso the original [openair::polarAnnulus()]
#' @seealso [annulusMap()] for the interactive `leaflet` equivalent of
#'   [annulusMapStatic()]
#'
#' @return a `ggplot2` plot with a `ggmap` basemap
#' @export
annulusMapStatic <- function(data,
                             pollutant = NULL,
                             period = "hour",
                             facet = NULL,
                             limits = NULL,
                             latitude = NULL,
                             longitude = NULL,
                             zoom = 13,
                             ggmap = NULL,
                             cols = "turbo",
                             alpha = 1,
                             key = FALSE,
                             facet.nrow = NULL,
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
  if (is.null(limits)) {
    theLimits <- NA
  }

  # cut data
  data <- quick_cutdata(data = data, type = facet)

  # prep data
  data <-
    prepMapData(
      data = data,
      pollutant = pollutant,
      control = facet,
      "wd",
      latitude,
      longitude,
      "date"
    )

  # identify splitting column (defaulting to pollutant)
  if (length(pollutant) > 1) {
    split_col <- "pollutant_name"
  } else if (!is.null(facet)) {
    data[facet] <- as.factor(data[[facet]])
    split_col <- facet
  } else {
    split_col <- "pollutant_name"
  }

  # define function
  fun <- function(data) {
    if (!is.null(limits)) {
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
      d.fig = d.fig
    )

  # load ggmap if not provided
  ggmap <-
    estimate_ggmap(
      ggmap = ggmap,
      data = plots_df,
      latitude = latitude,
      longitude = longitude,
      zoom = zoom
    )

  # create static map - deals with basics & facets
  plt <-
    create_static_map(
      ggmap = ggmap,
      plots_df = plots_df,
      latitude = latitude,
      longitude = longitude,
      split_col = split_col,
      pollutant = pollutant,
      facet = facet,
      facet.nrow = facet.nrow,
      d.icon = d.icon
    )

  # create colorbar if limits specified
  if (!is.null(limits)) {
    plt <-
      plt +
      ggplot2::geom_point(
        data = plots_df,
        ggplot2::aes(.data[[longitude]], .data[[latitude]], color = 0),
        alpha = 0
      ) +
      ggplot2::scale_color_gradientn(
        limits = theLimits,
        colours = openair::openColours(scheme = cols)
      ) +
      ggplot2::labs(color = openair::quickText(paste(pollutant, collapse = ", ")))
  }

  # return plot
  return(plt)
}
