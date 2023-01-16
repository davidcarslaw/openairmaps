#' Check input & prep data
#' @author David Carslaw
#' @noRd
checkMapPrep <-
  function(mydata,
           Names,
           remove.calm = TRUE,
           remove.neg = TRUE,
           wd = "wd") {
    ## deal with conditioning variable if present, if user-defined, must exist in data
    ## pre-defined types
    ## existing conditioning variables that only depend on date (which is checked)
    conds <- c(
      "default",
      "year",
      "hour",
      "month",
      "season",
      "weekday",
      "week",
      "weekend",
      "monthyear",
      "gmtbst",
      "bstgmt",
      "dst",
      "daylight",
      "yearseason",
      "seasonyear"
    )
    all.vars <- unique(c(names(mydata), conds))

    varNames <- c(Names) ## names we want to be there
    matching <- varNames %in% all.vars

    if (any(!matching)) {
      ## not all variables are present
      stop(
        "Can't find the variable(s): ",
        paste(varNames[!matching], collapse = ", "),
        "\n"
      )
    }

    ## just select data needed
    mydata <- mydata[, Names]

    ## if site is in the data set, check none are missing
    ## seems to be a problem for some KCL data...
    if ("site" %in% names(mydata)) {
      ## split by site

      ## remove any NA sites
      if (anyNA(mydata$site)) {
        id <- which(is.na(mydata$site))
        mydata <- mydata[-id, ]
      }
    }


    ## sometimes ratios are considered which can results in infinite values
    ## make sure all infinite values are set to NA
    mydata[] <- lapply(mydata, function(x) {
      replace(x, x == Inf | x == -Inf, NA)
    })

    if ("ws" %in% Names) {
      if ("ws" %in% Names & is.numeric(mydata$ws)) {
        ## check for negative wind speeds
        if (any(sign(mydata$ws[!is.na(mydata$ws)]) == -1)) {
          if (remove.neg) {
            ## remove negative ws only if TRUE
            warning("Wind speed <0; removing negative data")
            mydata$ws[mydata$ws < 0] <- NA
          }
        }
      }
    }

    ## round wd to make processing obvious
    ## data already rounded to nearest 10 degress will not be affected
    ## data not rounded will be rounded to nearest 10 degrees
    ## assumes 10 is average of 5-15 etc
    if (wd %in% Names) {
      if (wd %in% Names & is.numeric(mydata[, wd])) {
        ## check for wd <0 or > 360
        if (any(sign(mydata[[wd]][!is.na(mydata[[wd]])]) == -1 |
          mydata[[wd]][!is.na(mydata[[wd]])] > 360)) {
          warning("Wind direction < 0 or > 360; removing these data")
          mydata[[wd]][mydata[[wd]] < 0] <- NA
          mydata[[wd]][mydata[[wd]] > 360] <- NA
        }

        if (remove.calm) {
          if ("ws" %in% names(mydata)) {
            mydata[[wd]][mydata$ws == 0] <-
              NA ## set wd to NA where there are calms
            mydata$ws[mydata$ws == 0] <- NA ## remove calm ws
          }
          mydata[[wd]][mydata[[wd]] == 0] <-
            360 ## set any legitimate wd to 360

          ## round wd for use in functions - except windRose/pollutionRose
          mydata[[wd]] <- 10 * ceiling(mydata[[wd]] / 10 - 0.5)
          mydata[[wd]][mydata[[wd]] == 0] <-
            360 # angles <5 should be in 360 bin
        }
        mydata[[wd]][mydata[[wd]] == 0] <-
          360 ## set any legitimate wd to 360
      }
    }


    ## make sure date is ordered in time if present
    if ("date" %in% Names) {
      if ("POSIXlt" %in% class(mydata$date)) {
        stop("date should be in POSIXct format not POSIXlt")
      }

      ## try and work with a factor date - but probably a problem in original data
      if (is.factor(mydata$date)) {
        warning("date field is a factor, check date format")
        mydata$date <- as.POSIXct(mydata$date, "GMT")
      }

      mydata <- dplyr::arrange(mydata, date)

      ## make sure date is the first field
      if (names(mydata)[1] != "date") {
        mydata <- mydata[c("date", setdiff(names(mydata), "date"))]
      }

      ## check to see if there are any missing dates, stop if there are
      ids <- which(is.na(mydata$date))
      if (length(ids) > 0) {
        mydata <- mydata[-ids, ]
        warning(
          paste(
            "Missing dates detected, removing",
            length(ids), "lines"
          ),
          call. = FALSE
        )
      }

      ## daylight saving time can cause terrible problems - best avoided!!

      if (any(lubridate::dst(mydata$date))) {
        message("Detected data with Daylight Saving Time.")
      }
    }

    ## return data frame
    return(mydata)
  }

#' Prep data for mapping
#' @noRd
prepMapData <- function(data, pollutant, control, ..., .to_narrow = TRUE) {
  # check pollutant is there
  if (is.null(pollutant)) {
    cli::cli_abort(c(
      "x" = "{.code pollutant} is missing with no default.",
      "i" = "Please provide a column of {.code data} which represents the pollutant(s) of interest."
    ))
  }

  ## extract variables of interest
  vars <- unique(c(pollutant, control, ...))

  # check and select variables
  data <- checkMapPrep(data, vars)

  # check to see if variables exist in data
  if (length(intersect(vars, names(data))) != length(vars)) {
    stop(paste(vars[which(!vars %in% names(data))], "not found in data"), call. = FALSE)
  }

  # check if more than one pollutant & is.null split
  if (length(pollutant) > 1 & !is.null(control)) {
    cli::cli_warn(c(
      "!" = "Multiple pollutants {.emph and} {.code control/facet} option specified",
      "i" = "Please only specify multiple pollutants {.emph or} a {.code control/facet} option",
      "i" = "Defaulting to splitting by {.code pollutant}"
    ))
  }

  if (.to_narrow) {
    # pollutants to long
    data <-
      tidyr::pivot_longer(
        data = data,
        cols = dplyr::all_of(pollutant),
        names_to = "pollutant_name",
        values_to = "conc"
      )

    # make pollutant names factors
    data <-
      dplyr::mutate(
        .data = data,
        pollutant_name = as.factor(.data$pollutant_name)
      )
  }

  return(data)
}


#' Save an openair plot as a temp image to use as an icon
#' @noRd
save_icon_image <-
  function(data,
           fun,
           dir,
           pollutant,
           split,
           lat,
           lon,
           cols,
           key,
           fig.width,
           fig.height,
           ...) {
    id <- paste0(data[[lat]][1], data[[lon]][1])

    grDevices::png(
      type = "cairo-png",
      filename = paste0(dir, "/", id, "_", split, ".png"),
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
      ...
    )

    grDevices::dev.off()
  }

#' Save all openair plots as images and read as leaflet icons
#' @noRd
create_icons <-
  function(data,
           fun,
           pollutant,
           split,
           lat,
           lon,
           cols,
           key,
           fig.width,
           fig.height,
           iconWidth,
           iconHeight,
           ...) {
    # where to write files
    icon_dir <- tempdir()

    # drop missing data
    data <- tidyr::drop_na(data, .data[[pollutant]])

    # go through all sites and make some plot
    data %>%
      dplyr::arrange(.data[[lat]], .data[[lon]]) %>%
      dplyr::group_split(.data[[lat]], .data[[lon]]) %>%
      purrr::walk(
        .f = ~ save_icon_image(
          fun = fun,
          dir = icon_dir,
          pollutant = pollutant,
          split = split,
          lat = lat,
          lon = lon,
          cols = cols,
          key = key,
          fig.width = fig.width,
          fig.height = fig.height,
          ...
        )
      )

    dat2 <- data %>%
      dplyr::arrange(.data[[lat]], .data[[lon]]) %>%
      dplyr::mutate(id = paste0(.data[[lat]], .data[[lon]]))

    # definition of 'icons' aka the openair plots
    leafIcons <-
      lapply(
        paste0(
          icon_dir, "/", unique(dat2$id), "_", split, ".png"
        ),
        leaflet::makeIcon,
        iconWidth = iconWidth,
        iconHeight = iconHeight
      )
    names(leafIcons) <- unique(dat2$id)
    class(leafIcons) <- "leaflet_icon_set"

    leafIcons
  }


#' Make a leaflet map
#' @noRd
makeMap <-
  function(data,
           icons,
           provider,
           longitude,
           latitude,
           split_col,
           popup,
           label,
           collapse) {
    provider <- unique(provider)

    # data for plotting
    plot_data <-
      data %>%
      dplyr::group_by(.data[[latitude]], .data[[longitude]], .data[[split_col]]) %>%
      dplyr::mutate(dc = mean(!is.na(.data[["conc"]]))) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(.data[[latitude]], .data[[longitude]], .data[[split_col]], .keep_all = TRUE) %>%
      dplyr::arrange(.data[[latitude]], .data[[longitude]])

    # create leaflet map
    m <- leaflet::leaflet()

    # add tiles
    for (j in seq(length(provider))) {
      m <- leaflet::addProviderTiles(
        map = m,
        provider = provider[j],
        group = provider[j]
      )
    }

    # add markers
    for (i in names(icons)) {
      plot_data_i <-
        dplyr::filter(plot_data, .data[[split_col]] == i) %>%
        dplyr::filter(.data$dc != 0)

      if (!is.null(popup)) {
        thePopup <- plot_data_i[[popup]]
      } else {
        thePopup <- popup
      }

      if (!is.null(label)) {
        theLabel <- plot_data_i[[label]]
      } else {
        theLabel <- label
      }

      # only plot markers where there is data
      m <- leaflet::addMarkers(
        m,
        data = plot_data_i,
        lng = plot_data_i[[longitude]],
        lat = plot_data_i[[latitude]],
        icon = icons[[i]],
        popup = thePopup,
        label = theLabel,
        group = i %>% quickTextHTML()
      )
    }

    # add layer control for pollutants/providers
    if (length(icons) > 1 & length(provider) > 1) {
      m <-
        leaflet::addLayersControl(
          m,
          options = leaflet::layersControlOptions(collapsed = collapse),
          baseGroups = names(icons) %>% purrr::map_vec(quickTextHTML),
          overlayGroups = provider
        )
    } else if (length(icons) > 1 & length(provider) == 1) {
      m <- leaflet::addLayersControl(m,
        options = leaflet::layersControlOptions(collapsed = collapse),
        baseGroups = names(icons) %>% purrr::map_vec(quickTextHTML)
      )
    } else if (length(provider) > 1 & length(icons) == 1) {
      m <- leaflet::addLayersControl(m,
        options = leaflet::layersControlOptions(collapsed = collapse),
        baseGroups = provider
      )
    }

    # return
    return(m)
  }

#' guess latlon
#' @noRd
assume_latlon <- function(data, latitude, longitude) {
  guess_latlon <- function(data, latlon = c("lat", "lon")) {
    x <- names(data)
    if (latlon == "lat") {
      name <- "latitude"
      str <- c("latitude", "latitud", "lat")
    } else if (latlon == "lon") {
      name <- "longitude"
      str <- c("longitude", "longitud", "lon", "long", "lng")
    }
    str <-
      c(
        str,
        toupper(str),
        tolower(str),
        stringr::str_to_title(str)
      )
    id <- x %in% str
    out <- x[id]
    len <- length(out)
    if (len > 1) {
      cli::cli_abort("Cannot identify {name}: Multiple possible matches ({out})",
        call = NULL
      )
      return(NULL)
    } else if (len == 0) {
      cli::cli_abort("Cannot identify {name}: No clear match.", call = NULL)
      return(NULL)
    } else {
      cli::cli_alert_info("Assuming {name} is '{out}'")
      return(out)
    }
  }

  if (is.null(latitude) | is.null(longitude)) {
    cli::cli_h1("Assuming Latitude and/or Longitude")
    if (is.null(latitude)) {
      latitude <- guess_latlon(data, "lat")
    } else {
      cli::cli_alert_success("Latitude provided as '{latitude}'")
    }
    if (is.null(longitude)) {
      longitude <- guess_latlon(data, "lon")
    } else {
      cli::cli_alert_success("Latitude provided as '{longitude}'")
    }
  }

  out <- list(
    latitude = latitude,
    longitude = longitude
  )
}

#' get breaks for the "rose" functions
#' @param breaks as given by windrose
#' @param ws.int as given by windrose
#' @param vec the vector to calc max/min/q90
#' @param polrose use pollutionrose method? T/F
#' @noRd
getBreaks <- function(breaks, ws.int, vec, polrose) {
  if (is.numeric(breaks) & length(breaks) == 1 & polrose) {
    breaks <- unique(pretty(
      c(
        min(vec, na.rm = TRUE),
        stats::quantile(vec, probs = 0.9, na.rm = TRUE)
      ),
      breaks
    ))
  }
  if (length(breaks) == 1) {
    breaks <- 0:(breaks - 1) * ws.int
  }
  if (max(breaks) < max(vec, na.rm = T)) {
    breaks <- c(breaks, max(vec, na.rm = T))
  }
  breaks <- unique(breaks)
  breaks <- sort(breaks)
  breaks
}
