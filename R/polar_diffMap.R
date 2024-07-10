#' Bivariate polar 'difference' plots on dynamic and static maps
#'
#' The [diffMap()] function creates a map using bivariate polar plots as
#' markers. Any number of pollutants can be specified using the `pollutant`
#' argument, and multiple layers of markers can be created using `type`. By
#' default, these maps are dynamic and can be panned, zoomed, and otherwise
#' interacted with. Using the `static` argument allows for static images to be
#' produced instead.
#'
#' @inheritSection polarMap Customisation of static maps using ggplot2
#' @family directional analysis maps
#'
#' @inheritParams openair::polarDiff
#' @inheritParams polarMap
#'
#' @param limits *Limits for the plot colour scale.*
#'
#'  *default:* `"free"` | *scope:* dynamic & static
#'
#'  One of:
#'  - `"free"` (the default) which allows all of the markers to use different
#'  colour scales.
#'  - A numeric vector in the form `c(lower, upper)` used to define the colour
#'  scale. For example, `limits = c(-10, 10)` would force the plot limits to
#'  span -10 to 10. It is recommended to use a symmetrical limit scale (along
#'  with a "diverging" colour palette) for effective visualisation.
#'
#'  Note that the `"fixed"` option is not supported in [diffMap()].
#'
#' @param cols *Colours to use for plotting.*
#'
#'  *default:* `rev(openair::openColours("RdBu", 10))` | *scope:* dynamic & static
#'
#'  The colours used for plotting, passed to [openair::openColours()].  It is
#'  recommended to use a "diverging" colour palette (along with a symmetrical
#'  `limit` scale) for effective visualisation.
#' 
#' @inheritDotParams openair::polarPlot -mydata -pollutant -x -limits -type
#'   -cols -key -key.footer -key.header -key.position -units -angle.scale -alpha
#'   -plot
#' @returns Either:
#'
#'  - *Dynamic:* A leaflet object
#'  - *Static:* A `ggplot2` object using [ggplot2::coord_sf()] coordinates with a `ggspatial` basemap
#' @export
#'
#' @seealso [openair::polarDiff()]
#'
#' @examples
#' \dontrun{
#' # NB: "after" is some dummy data to demonstrate functionality
#' diffMap(
#'   before = polar_data,
#'   after = dplyr::mutate(polar_data, nox = jitter(nox, factor = 5)),
#'   pollutant = "nox"
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
                    type = NULL,
                    popup = NULL,
                    label = NULL,
                    provider = "OpenStreetMap",
                    cols = rev(openair::openColours("RdBu", 10)),
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
                    ...,
                    control = NULL) {
  # check basemap providers are valid
  provider <- check_providers(provider, static)
  legend.position <- check_legendposition(legend.position, static)

  # check for old facet/control opts
  type <- type %||% check_facet_control(control = control, ...)

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
    cli::cli_abort("{.code limits = 'fixed'} is currently not supported for {.fun diffMap}.")
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

  # deal with popups
  if (length(popup) > 1) {
    data <-
      quick_popup(
        data = before,
        popup = popup,
        latitude = latitude,
        longitude = longitude,
        control = type
      )
    popup <- "popup"
  }

  # cut data
  before <- openair::cutData(x = before, type = type %||% "default", ...)
  after <- openair::cutData(x = after, type = type %||% "default", ...)

  # prep data
  before <-
    prepMapData(
      data = before,
      pollutant = pollutant,
      control = type,
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
      control = type,
      "wd",
      x,
      latitude,
      longitude
    )

  # identify splitting column (defaulting to pollutant)
  if (length(pollutant) > 1) {
    split_col <- "pollutant_name"
  } else if (!is.null(type)) {
    before[type] <- as.factor(before[[type]])
    after[type] <- as.factor(after[[type]])
    split_col <- type
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
      label = label,
      progress = progress
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
          position = legend.position,
          pal = leaflet::colorNumeric(
            palette = openair::openColours(scheme = cols),
            domain = theLimits
          ),
          values = theLimits
        )
    }
  }

  # return map
  return(map)
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
           dropcol = "conc",
           progress = TRUE) {
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
      tidyr::nest(after = -dplyr::all_of(c(latitude, longitude, split_col)))

    # warn if missing
    if (nrow(nested_before) != nrow(nested_after)) {
      warn_df <-
        dplyr::bind_rows(
          dplyr::anti_join(
            nested_before,
            nested_after,
            by = c(latitude, longitude, split_col)
          ),
          dplyr::anti_join(
            nested_after,
            nested_before,
            by = c(latitude, longitude, split_col)
          )
        ) %>%
        tidyr::unite("warning", dplyr::any_of(c(latitude, longitude, split_col)), sep = "/") %>%
        dplyr::distinct(.data$warning)

      cli::cli_warn(
        c(
          "!" = "Not all {.code latitude}/{.code longitude}/{.code control} combinations in {.code before} matched in {.code after}.",
          "i" = "Not matched: {.field {warn_df$warning}}"
        )
      )
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
        plot = purrr::map2(before, after, fun, .progress = ifelse(progress, "Creating Polar Markers", FALSE)),
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
