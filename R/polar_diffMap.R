#' Bivariate polar plots on interactive leaflet maps
#'
#' [diffMap()] creates a `leaflet` map using bivariate polar "difference" plots
#' as markers. Any number of pollutants can be specified using the `pollutant`
#' argument, and multiple layers of markers can be added and toggled between
#' using `control`.
#'
#' @family interactive directional analysis maps
#'
#' @inheritParams openair::polarDiff
#' @inheritParams polarMap
#'
#' @param limits By default, each individual polar marker has its own colour
#'   scale. The `limits` argument will force all markers to use the same colour
#'   scale. The limits are set in the form `c(lower, upper)`, so `limits = c(-5,
#'   5)` would force the plot limits to span -5 to 5. It is recommended to use a
#'   symmetrical limit scale (along with a "diverging" colour palette) for
#'   effective visualisation.
#' @param cols The colours used for plotting. It is recommended to use a
#'   "diverging" colour palette (along with a symmetrical limit scale) for
#'   effective visualisation.
#' @inheritDotParams openair::polarPlot -mydata -pollutant -x -limits -type
#'   -cols -key -key.footer -key.header -key.position -units -angle.scale -alpha
#'   -plot
#' @return A leaflet object.
#' @export
#'
#' @seealso the original [openair::polarDiff()]
#' @seealso [diffMapStatic()] for the static `ggmap` equivalent of [diffMap()]
#'
#' @examples
#' \dontrun{
#' # NB: "after" is some dummy data to demonstrate functionality
#' diffMap(
#'   before = polar_data,
#'   after = dplyr::mutate(polar_data, nox = jitter(nox, factor = 5)),
#'   pollutant = "nox",
#'   provider = "Stamen.Toner"
#' )
#' }
diffMap <- function(before,
                    after,
                    pollutant = NULL,
                    x = "ws",
                    limits = "free",
                    latitude = NULL,
                    longitude = NULL,
                    crs = 4326,
                    control = NULL,
                    popup = NULL,
                    label = NULL,
                    provider = "OpenStreetMap",
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
                    draw.legend = TRUE,
                    collapse.control = FALSE,
                    d.icon = 200,
                    d.fig = 3.5,
                    type = deprecated(),
                    ...) {
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_soft(
      when = "0.5.0",
      what = "openairmaps::diffMap(type)",
      details = c(
        "Different sites are now automatically detected based on latitude and longitude",
        "Please use the `popup` argument to create popups."
      )
    )
  }

  # assume lat/lon
  latlon <- assume_latlon(
    data = before,
    latitude = latitude,
    longitude = longitude
  )
  latitude <- latlon$latitude
  longitude <- latlon$longitude

  # auto limits
  if ("fixed" %in% limits) {
    cli::cli_abort("{.code limits = 'fixed'} is currently not supported for {.fun diffMap} and {.fun diffMapStatic}.")
    # if (length(pollutant) == 1) {
    #   before <-
    #     dplyr::mutate(before, latlng = paste(.data[[latitude]], .data[[longitude]]))
    #   after <-
    #     dplyr::mutate(after, latlng = paste(.data[[latitude]], .data[[longitude]]))
    #
    #   type <- control
    #   if (is.null(control)) {
    #     type <- "default"
    #   }
    #
    #   testplots <-
    #     openair::polarDiff(
    #       before = before, after = after,
    #       pollutant = pollutant,
    #       x = x,
    #       type = c("latlng", type),
    #       plot = FALSE,
    #       ...
    #     )$data
    #
    #   theLimits <- range(testplots[[pollutant]], na.rm = TRUE)
    # } else {
    #   cli::cli_warn("{.code limits == 'auto'} only works with a single given {.field pollutant}")
    # }
  } else if ("free" %in% limits) {
    theLimits <- NA
  } else if (is.numeric(limits)){
    theLimits <- limits
  } else {
    cli::cli_abort(
      c("!" = "Do not recognise {.field limits} value of {.code {limits}}",
        "i" = "{.field limits} should be one of {.code 'fixed'}, {.code 'free'} or a numeric vector of length 2.")
    )
  }

  # deal with popups
  if (length(popup) > 1) {
    data <-
      quick_popup(
        data = before,
        popup = popup,
        latitude = latitude,
        longitude = longitude,
        control = control
      )
    popup <- "popup"
  }

  # cut data
  before <- quick_cutdata(data = before, type = control)
  after <- quick_cutdata(data = after, type = control)

  # prep data
  before <-
    prepMapData(
      data = before,
      pollutant = pollutant,
      control = control,
      "wd",
      x,
      latitude,
      longitude,
      popup,
      label
    )

  after <-
    prepMapData(
      data = after,
      pollutant = pollutant,
      control = control,
      "wd",
      x,
      latitude,
      longitude
    )

  # identify splitting column (defaulting to pollutant)
  if (length(pollutant) > 1) {
    split_col <- "pollutant_name"
  } else if (!is.null(control)) {
    before[control] <- as.factor(before[[control]])
    after[control] <- as.factor(after[[control]])
    split_col <- control
  } else {
    split_col <- "pollutant_name"
  }

  # define function
  fun <- function(before, after) {
    openair::polarDiff(
      before = before,
      after = after,
      pollutant = "conc",
      x = x,
      limits = theLimits,
      cols = cols,
      alpha = alpha,
      key = key,
      plot = FALSE,
      ...,
      par.settings = list(axis.line = list(col = "transparent"))
    )$plot
  }

  # plot and save static markers
  plots_df <-
    create_polar_diffmarkers(
      fun = fun,
      before = before,
      after = after,
      latitude = latitude,
      longitude = longitude,
      split_col = split_col,
      d.fig = d.fig,
      popup = popup,
      label = label
    )

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
      collapse.control
    )

  # add legend if limits are set
  if (!all(is.na(theLimits)) & draw.legend) {
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
#' [diffMapStatic()] creates a `ggplot2` map using bivariate "difference" polar
#' plots as markers. As this function returns a `ggplot2` object, further
#' customisation can be achieved using functions like [ggplot2::theme()] and
#' [ggplot2::guides()].
#'
#' @inheritSection polarMapStatic Further customisation using ggplot2
#' @family static directional analysis maps
#'
#' @inheritParams polarMapStatic
#' @inheritParams diffMap
#' @inheritDotParams openair::polarPlot -mydata -pollutant -x -limits -type
#'   -cols -key -key.footer -key.header -key.position -units -angle.scale -alpha
#'   -plot
#'
#' @seealso the original [openair::polarDiff()]
#' @seealso [diffMap()] for the interactive `leaflet` equivalent of
#'   [diffMapStatic()]
#'
#' @return a `ggplot2` plot with a `ggmap` basemap
#' @export
diffMapStatic <- function(before,
                          after,
                          pollutant = NULL,
                          ggmap,
                          limits = "free",
                          x = "ws",
                          latitude = NULL,
                          longitude = NULL,
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
  # check that there is a ggmap
  check_ggmap(missing(ggmap))

  # assume lat/lon
  latlon <- assume_latlon(
    data = before,
    latitude = latitude,
    longitude = longitude
  )
  latitude <- latlon$latitude
  longitude <- latlon$longitude

  # auto limits
  if ("fixed" %in% limits) {
    cli::cli_abort("{.code limits = 'fixed'} is currently not supported for {.fun diffMap} and {.fun diffMapStatic}.")
    # if (length(pollutant) == 1) {
    #   before <-
    #     dplyr::mutate(before, latlng = paste(.data[[latitude]], .data[[longitude]]))
    #   after <-
    #     dplyr::mutate(after, latlng = paste(.data[[latitude]], .data[[longitude]]))
    #
    #   type <- facet
    #   if (is.null(facet)) {
    #     type <- "default"
    #   }
    #
    #   testplots <-
    #     openair::polarDiff(
    #       before = before, after = after,
    #       pollutant = pollutant,
    #       x = x,
    #       type = c("latlng", type),
    #       plot = FALSE,
    #       ...
    #     )$data
    #
    #   theLimits <- range(testplots[[pollutant]], na.rm = TRUE)
    # } else {
    #   cli::cli_warn("{.code limits == 'auto'} only works with a single given {.field pollutant}")
    # }
  } else if ("free" %in% limits) {
    theLimits <- NA
  } else if (is.numeric(limits)){
    theLimits <- limits
  } else {
    cli::cli_abort(
      c("!" = "Do not recognise {.field limits} value of {.code {limits}}",
        "i" = "{.field limits} should be one of {.code 'fixed'}, {.code 'free'} or a numeric vector of length 2.")
    )
  }

  # cut data
  before <- quick_cutdata(data = before, type = facet)
  after <- quick_cutdata(data = after, type = facet)

  # prep data
  before <-
    prepMapData(
      data = before,
      pollutant = pollutant,
      control = facet,
      "wd",
      x,
      latitude,
      longitude
    )

  after <-
    prepMapData(
      data = after,
      pollutant = pollutant,
      control = facet,
      "wd",
      x,
      latitude,
      longitude
    )

  # identify splitting column (defaulting to pollutant)
  if (length(pollutant) > 1) {
    split_col <- "pollutant_name"
  } else if (!is.null(facet)) {
    before[facet] <- as.factor(before[[facet]])
    after[facet] <- as.factor(after[[facet]])
    split_col <- facet
  } else {
    split_col <- "pollutant_name"
  }

  # define function
  fun <- function(before, after) {
    openair::polarDiff(
      before = before,
      after = after,
      pollutant = "conc",
      x = x,
      limits = theLimits,
      cols = cols,
      alpha = alpha,
      key = key,
      plot = FALSE,
      ...,
      par.settings = list(axis.line = list(col = "transparent"))
    )$plot
  }

  # plot and save static markers
  plots_df <-
    create_polar_diffmarkers(
      fun = fun,
      before = before,
      after = after,
      latitude = latitude,
      longitude = longitude,
      split_col = split_col,
      d.fig = d.fig
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
  if (!all(is.na(theLimits))) {
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

#' create diff markers
#' @noRd
create_polar_diffmarkers <-
  function(fun,
           before = before,
           after = after,
           latitude = latitude,
           longitude = longitude,
           split_col = split_col,
           popup = NULL,
           label = NULL,
           d.fig,
           dropcol = "conc") {
    # make temp directory
    dir <- tempdir()

    # unique id
    id <- gsub(" |:|-", "", as.character(Sys.time()))

    # sort out popups/labels
    if (is.null(popup)) {
      before$popup <- "NA"
      popup <- "popup"
    }
    if (is.null(label)) {
      before$label <- "NA"
      label <- "label"
    }

    # drop missing data
    before <- tidyr::drop_na(before, dplyr::all_of(dropcol))
    after <- tidyr::drop_na(after, dplyr::all_of(dropcol))

    # get number of rows
    valid_rows <-
      nrow(dplyr::distinct(before, .data[[latitude]], .data[[longitude]], .data[[split_col]]))

    # nest data
    nested_before <- before %>%
      tidyr::nest(before = -dplyr::all_of(c(
        latitude, longitude, split_col, popup, label
      )))
    nested_after <- after %>%
      tidyr::nest(after = -dplyr::all_of(c(
        latitude, longitude, split_col
      )))

    # warn if missing
    if (nrow(nested_before) != nrow(nested_after)) {
      warn_df <-
        dplyr::bind_rows(
          dplyr::anti_join(nested_before, nested_after, by = c(latitude, longitude, split_col)),
          dplyr::anti_join(nested_after, nested_before, by = c(latitude, longitude, split_col))
        ) %>%
        tidyr::unite("warning", dplyr::any_of(c(latitude, longitude, split_col)), sep = "/") %>%
        dplyr::distinct(.data$warning)

      cli::cli_warn(c(
        "!" = "Not all {.code latitude}/{.code longitude}/{.code control} combinations in {.code before} matched in {.code after}.",
        "i" = "Not matched: {.field {warn_df$warning}}"
      ))
    }

    # check for popup issues
    if (nrow(nested_before) > valid_rows) {
      cli::cli_abort(
        c(
          "x" = "Multiple popups/labels per {.code latitude}/{.code longitude}/{.code control} combination.",
          "i" = "Have you used a numeric column, e.g., a pollutant concentration?",
          "i" = "Consider using {.fun buildPopup} to easily create distinct popups per marker."
        )
      )
    }

    # create plots
    plots_df <-
      dplyr::inner_join(nested_before,
        nested_after,
        by = c(latitude, longitude, split_col)
      ) %>%
      dplyr::mutate(
        plot = purrr::map2(before, after, fun, .progress = "Creating Polar Markers"),
        url = paste0(dir, "/", .data[[latitude]], "_", .data[[longitude]], "_", .data[[split_col]], "_", id, ".png")
      )

    # work out w/h
    if (length(d.fig) == 1) {
      width <- height <- d.fig
    }
    if (length(d.fig) == 2) {
      width <- d.fig[[1]]
      height <- d.fig[[2]]
    }

    purrr::pwalk(list(plots_df[[latitude]], plots_df[[longitude]], plots_df[[split_col]], plots_df$plot),
      .f = ~ {
        grDevices::png(
          filename = paste0(dir, "/", ..1, "_", ..2, "_", ..3, "_", id, ".png"),
          width = width * 300,
          height = height * 300,
          res = 300,
          bg = "transparent",
          type = "cairo",
          antialias = "none"
        )

        plot(..4)

        grDevices::dev.off()
      }
    )

    return(plots_df)
  }
