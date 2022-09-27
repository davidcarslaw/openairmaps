#' Check input & prep data
#' @author David Carslaw
#' @importFrom lubridate dst
#' @noRd

checkMapPrep <- function(mydata, Names, type, remove.calm = TRUE, remove.neg = TRUE, wd = "wd") {

  ## deal with conditioning variable if present, if user-defined, must exist in data
  ## pre-defined types
  ## existing conditioning variables that only depend on date (which is checked)
  conds <- c(
    "default", "year", "hour", "month", "season", "weekday", "week",
    "weekend", "monthyear", "gmtbst", "bstgmt", "dst", "daylight",
    "yearseason", "seasonyear"
  )
  all.vars <- unique(c(names(mydata), conds))

  varNames <- c(Names, type) ## names we want to be there
  matching <- varNames %in% all.vars

  if (any(!matching)) {
    ## not all variables are present
    stop(
      "Can't find the variable(s): ",
      paste(varNames[!matching], collapse = ", "), "\n"
    )
  }

  ## add type to names if not in pre-defined list
  if (any(type %in% conds == FALSE)) {
    ids <- which(type %in% conds == FALSE)
    Names <- c(Names, type[ids])
  }

  ## if type already present in data frame
  if (any(type %in% names(mydata))) {
    ids <- which(type %in% names(mydata))
    Names <- unique(c(Names, type[ids]))
  }

  ## just select data needed
  mydata <- mydata[, Names]

  ## if site is in the data set, check none are missing
  ## seems to be a problem for some KCL data...
  if ("site" %in% names(mydata)) { ## split by site

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
        if (remove.neg) { ## remove negative ws only if TRUE
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
          mydata[[wd]][mydata$ws == 0] <- NA ## set wd to NA where there are calms
          mydata$ws[mydata$ws == 0] <- NA ## remove calm ws
        }
        mydata[[wd]][mydata[[wd]] == 0] <- 360 ## set any legitimate wd to 360

        ## round wd for use in functions - except windRose/pollutionRose
        mydata[[wd]] <- 10 * ceiling(mydata[[wd]] / 10 - 0.5)
        mydata[[wd]][mydata[[wd]] == 0] <- 360 # angles <5 should be in 360 bin
      }
      mydata[[wd]][mydata[[wd]] == 0] <- 360 ## set any legitimate wd to 360
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
      warning(paste(
        "Missing dates detected, removing",
        length(ids), "lines"
      ), call. = FALSE)
    }

    ## daylight saving time can cause terrible problems - best avoided!!

    if (any(dst(mydata$date))) {
      message("Detected data with Daylight Saving Time.")
    }
  }

  ## return data frame
  return(mydata)
}

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

    # drop missing data
    data <- tidyr::drop_na(data, .data[[pollutant]])

    # go through all sites and make some plot
    data %>%
      dplyr::group_split(dplyr::across(dplyr::all_of(type))) %>%
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
  data <- checkMapPrep(data, vars, type = type)

  # cut data
  data <- openair::cutData(data, type)

  # remove missing data
  # data <- na.omit(data)

  # check to see if variables exist in data
  if (length(intersect(vars, names(data))) != length(vars)) {
    stop(paste(vars[which(!vars %in% names(data))], "not found in data"), call. = FALSE)
  }

  return(data)
}

#' Make a leaflet map
#' @noRd

makeMap <- function(data, icons, provider, longitude, latitude, type, pollutant) {
  provider <- unique(provider)

  # data for plotting
  plot_data <-
    dplyr::group_by(data, .data[[type]]) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(pollutant), ~ mean(is.na(.x)))) %>%
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
    # only plot markers where there is data
    plotdat <- dplyr::filter(
      plot_data,
      .data[[sort(pollutant)[[i]]]] != 1
    )

    m <- leaflet::addMarkers(
      m,
      data = plotdat,
      lng = plotdat[[longitude]],
      lat = plotdat[[latitude]],
      icon = icons[[i]],
      popup = plotdat[[type]],
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

#' guess latlon
#' @noRd
assume_latlon <- function(data, latitude, longitude) {
  guess_latlon <- function(data, latlon = c("lat", "lon")) {
    x <- names(data)
    if (latlon == "lat") {
      name <- "latitude"
      str <- c("latitude", "lat")
    } else if (latlon == "lon") {
      name <- "longitude"
      str <- c("longitude", "lon", "long", "lng")
    }
    str <- c(str, toupper(str), tolower(str), stringr::str_to_title(str))
    id <- x %in% str
    out <- x[id]
    len <- length(out)
    if (len > 1) {
      cli::cli_abort("Cannot identify {name}: Multiple possible matches ({out})", call = NULL)
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
