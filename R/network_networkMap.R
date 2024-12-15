#' Create a leaflet map of air quality measurement network sites
#'
#' This function uses [openair::importMeta()] to obtain metadata for measurement
#' sites and uses it to create an attractive `leaflet` map. By default a map
#' will be created in which readers may toggle between a vector base map and a
#' satellite/aerial image, although users can further customise the control menu
#' using the `provider` and `control` parameters.
#'
#' When selecting multiple data sources using `source`, please be mindful that
#' there can be overlap between the different networks. For example, an air
#' quality site in Scotland may be part of the AURN *and* the SAQN.
#' [networkMap()] will only show one marker for such sites, and uses the order
#' in which `source` arguments are provided as the hierarchy by which to assign
#' sites to networks. The aforementioned AURN & SAQN site will therefore have
#' its SAQN code displayed if `source = c("saqn", "aurn")`, and its AURN code
#' displayed if `source = c("aurn", "saqn")`.
#'
#' This hierarchy is also reflected when `control = "network"` is used. As
#' `leaflet` markers cannot be part of multiple groups, the AURN & SAQN site
#' will be part of the "SAQN" layer control group when `source = c("saqn",
#' "aurn")` and the "AURN" layer control group when `source = c("aurn",
#' "saqn")`.
#'
#' @param source *One or more UK or European monitoring networks.*
#'
#'    *default:* `"aurn"`
#'
#'   One or more air quality networks for which data is available through
#'   openair. Available networks include:
#'    - `"aurn"`, The UK Automatic Urban and Rural Network.
#'    - `"aqe"`, The Air Quality England Network.
#'    - `"saqn"`, The Scottish Air Quality Network.
#'    - `"waqn"`, The Welsh Air Quality Network.
#'    - `"ni"`, The Northern Ireland Air Quality Network.
#'    - `"local"`, Locally managed air quality networks in England.
#'    - `"kcl"`, King's College London networks.
#'    - `"europe"`, European AirBase/e-reporting data.
#'
#'   There are two additional options provided for convenience:
#'    - `"ukaq"` will return metadata for all networks for which data is imported by importUKAQ() (i.e., AURN, AQE, SAQN, WAQN, NI, and the local networks).
#'    - `"all"` will import all available metadata (i.e., "ukaq" plus "kcl" and "europe").
#'
#' @param control *Option to create a 'layer control' menu.*
#'
#'  *default*: `NULL`
#'
#'   A string to specify categories in a "layer control" menu, to allow readers
#'   to select between different site categories. Choices include:
#'   - `"variable"` to toggle between different pollutants
#'   - `"site_type"` for different site classifications
#'   - `"agglomeration"`, `"zone"` or `"local_authority"` for different regions of the UK
#'   - `"network"` for different monitoring networks, if more than one `source` is provided.
#'
#' @param year *A year, or range of years, with which to filter data.*
#'
#'  *default*: `NULL`
#'
#'   By default, [networkMap()] visualises sites which are currently
#'   operational. `year` allows users to show sites open in a specific year, or
#'   over a range of years. See [openair::importMeta()] for more information.
#'
#' @param cluster *Cluster markers together when zoomed out?*
#'
#'  *default:* `TRUE`
#'
#'   When `cluster = TRUE`, markers are clustered together. This may be useful
#'   for sources like "kcl" where there are many markers very close together.
#'   Defaults to `TRUE`, and is forced to be `TRUE` when `source = "europe"` due
#'   to the large number of sites.
#'
#' @param provider *The basemap(s) to be used.*
#'
#'  *default:* `c("Default" = "OpenStreetMap", "Satellite" = "Esri.WorldImagery")`
#'
#'   Any number of [leaflet::providers]. See
#'   <http://leaflet-extras.github.io/leaflet-providers/preview/> for a list of
#'   all base maps that can be used. If multiple base maps are provided, they
#'   can be toggled between using a "layer control" interface. By default, the
#'   interface will use the provider names as labels, but users can define their
#'   own using a named vector (e.g., `c("Default" = "OpenStreetMap", "Satellite"
#'   = "Esri.WorldImagery")`)
#'
#' @param legend *Draw a shared legend?*
#'
#'  *default:* `TRUE`
#'
#'   When multiple `source`s are defined, should a shared legend be created at
#'   the side of the map?
#'
#' @param legend.position *Position of the legend*
#'
#'  *default:* `"topright"`
#'
#'   Where should the shared legend be placed? One of "topleft", "topright",
#'   "bottomleft" or "bottomright". Passed to the `position` argument of
#'   [leaflet::addLayersControl()].
#'
#' @param control.collapsed *Show the layer control as a collapsed?*
#'
#'  *default:* `FALSE`
#'
#'   Should the "layer control" interface be collapsed? If `TRUE`, users will
#'   have to hover over an icon to view the options.
#'
#' @param control.position *Position of the layer control menu*
#'
#'  *default:* `"topright"`
#'
#'   Where should the "layer control" interface be placed? One of "topleft",
#'   "topright", "bottomleft" or "bottomright". Passed to the `position`
#'   argument of [leaflet::addLayersControl()].
#'
#' @returns A leaflet object.
#' @export
#' @family uk air quality network mapping functions
#'
#' @order 1
#'
#' @examples
#' \dontrun{
#' # view one network, grouped by site type
#' networkMap(source = "aurn", control = "site_type")
#'
#' # view multiple networks, grouped by network
#' networkMap(source = c("aurn", "waqn", "saqn"), control = "network")
#' }
#'
networkMap <-
  function(source = "aurn",
           control = NULL,
           year = NULL,
           cluster = TRUE,
           provider = c(
             "Default" = "OpenStreetMap",
             "Satellite" = "Esri.WorldImagery"
           ),
           legend = TRUE,
           legend.position = "topright",
           control.collapsed = FALSE,
           control.position = "topright") {
    # if year isn't provided, use current year
    if (is.null(year)) {
      year <- lubridate::year(Sys.Date())
      cli::cli_inform(c("i" = "{.code year} not specified. Showing sites open in {.field {year}}."))
    }
    source <- unique(source)

    cols <-
      dplyr::tibble(
        network = c(
          "AURN",
          "SAQN",
          "AQE",
          "WAQN",
          "NI",
          "Locally Managed",
          "KCL",
          "Europe"
        ),
        colour = c(
          "red",
          "orange",
          "blue",
          "green",
          "purple",
          "lightgray",
          "black",
          "pink"
        ),
        realcolour = c(
          "#d33d29",
          "#f49630",
          "#36a5d7",
          "#70ad25",
          "#cf51b6",
          "#a3a3a3",
          "#303030",
          "#ff8ee9"
        )
      ) %>%
      dplyr::mutate(colour2 = ifelse(.data$colour == "#FFFFFF", "#303030", "#FFFFFF"))

    # read in data
    meta <-
      purrr::map(
        .x = source,
        .f = ~ prepNetworkData(source = .x, year = year)
      ) %>%
      purrr::list_rbind() %>%
      dplyr::left_join(cols, by = "network")

    # prep for legend
    cols <- dplyr::filter(cols, .data$network %in% meta$network)

    meta <-
      meta %>%
      dplyr::group_by(.data$site, .data$latitude, .data$longitude) %>%
      dplyr::mutate(
        network2 = paste(unique(.data$network), collapse = " <i>(& "),
        network2 = dplyr::if_else(
          stringr::str_detect(.data$network2, "&"),
          paste0(.data$network2, ")</i>"),
          .data$network2
        ),
        lab = stringr::str_replace(.data$lab, .data$network, .data$network2)
      ) %>%
      dplyr::select(-"network2")

    # get unique pollutants if control = pollutant
    if (!is.null(control)) {
      if (control %in% c("Parameter_name", "variable")) {
        meta <-
          dplyr::group_by(
            meta,
            .data$site,
            .data$latitude,
            .data$longitude,
            .data$variable
          )
      }
    }

    # get unique sites (& pollutants if appropriate)
    meta <- dplyr::slice_head(meta, n = 1)

    # build maps
    # initialise map
    map <- leaflet::leaflet()

    # add provider tiles
    if (is.null(names(provider)) | "" %in% names(provider)) {
      names(provider) <- provider
    }
    for (i in seq_along(provider)) {
      map <-
        leaflet::addProviderTiles(map, provider = provider[[i]], group = names(provider)[[i]])
    }

    # cluster options
    if (cluster | "europe" %in% source) {
      clusteropts <- leaflet::markerClusterOptions()
    } else {
      clusteropts <- NA
    }

    # sort out control
    if (!is.null(control)) {
      control.position <- check_legendposition(control.position, static = FALSE)
      if (!control %in% names(meta)) {
        trycols <- names(meta)[!names(meta) %in%
          c(
            "code",
            "site",
            "latitude",
            "longitude",
            "country_iso_code",
            "elevation",
            "ratified_to",
            "Address",
            "la_id",
            "eu_code",
            "eoi_code",
            "data_source",
            "os_grid_x",
            "os_grid_y",
            "start_date",
            "end_date",
            "observation_count",
            "start_date2",
            "end_date2",
            "lab",
            "pcode",
            "colour",
            "colour2"
          )]

        cli::cli_abort(
          c(
            "x" = "'{control}' is not an appropriate {.coed control} option.",
            "i" = "Suggested control options: {.emph {trycols}}"
          )
        )
      }

      if (!control %in% c("Parameter_name", "variable")) {
        meta <-
          dplyr::distinct(meta, .data$code, .data$site, .keep_all = TRUE)
      }

      # ensure "control" is always present, and that "other" category is at the end
      if (control != "network") {
        meta[[control]][is.na(meta[[control]])] <- "Other"
        meta[[control]] <- factor(meta[[control]])
        if ("Other" %in% levels(meta[[control]])) {
          cur_levels <- levels(meta[[control]])
          levels(meta[[control]]) <- c(cur_levels[cur_levels != "Other"], "Other")
        }
      }

      # get control variables
      control_vars <- sort(unique(meta[[control]]))
      control_vars <- control_vars[control_vars != "NO"]

      # add markers
      for (i in seq(length(control_vars))) {
        dat <- dplyr::filter(meta, .data[[control]] == control_vars[[i]])

        map <- map %>%
          leaflet::addAwesomeMarkers(
            data = dat,
            lat = dat[["latitude"]],
            lng = dat[["longitude"]],
            group = quickTextHTML(control_vars[[i]]),
            popup = dat[["lab"]],
            label = dat[["site"]],
            clusterOptions = clusteropts,
            icon = leaflet::makeAwesomeIcon(
              library = "fa",
              icon = "info-circle",
              markerColor = dat$colour,
              iconColor = dat$colour2
            )
          )
      }

      if (control %in% c("Parameter_name", "variable")) {
        if (length(provider) > 1) {
          map <-
            leaflet::addLayersControl(
              map,
              position = control.position,
              options = leaflet::layersControlOptions(collapsed = control.collapsed, autoZIndex = FALSE),
              baseGroups = sort(quickTextHTML(control_vars)),
              overlayGroups = names(provider)
            ) %>%
            leaflet::hideGroup(group = names(provider)[[-1]])
        } else {
          map <-
            leaflet::addLayersControl(
              map,
              position = control.position,
              options = leaflet::layersControlOptions(collapsed = control.collapsed, autoZIndex = FALSE),
              baseGroups = quickTextHTML(sort(control_vars))
            )
        }
      } else {
        if (length(provider) > 1) {
          map <-
            leaflet::addLayersControl(
              map,
              position = control.position,
              options = leaflet::layersControlOptions(collapsed = control.collapsed, autoZIndex = FALSE),
              overlayGroups = quickTextHTML(sort(control_vars)),
              baseGroups = names(provider)
            ) %>%
            leaflet::hideGroup(group = names(provider)[[-1]])
        } else {
          map <-
            leaflet::addLayersControl(
              map,
              position = control.position,
              options = leaflet::layersControlOptions(collapsed = control.collapsed, autoZIndex = FALSE),
              overlayGroups = quickTextHTML(sort(control_vars))
            )
        }
      }
    } else {
      meta <-
        dplyr::distinct(meta, .data$code, .data$site, .keep_all = TRUE)

      dat <- dplyr::distinct(meta, .data$site, .keep_all = TRUE)

      map <- map %>%
        leaflet::addAwesomeMarkers(
          data = dat,
          lat = dat[["latitude"]],
          lng = dat[["longitude"]],
          popup = dat[["lab"]],
          label = dat[["site"]],
          clusterOptions = clusteropts,
          icon = leaflet::makeAwesomeIcon(
            library = "fa",
            icon = "info-circle",
            markerColor = dat$colour,
            iconColor = dat$colour2
          )
        )

      if (length(provider) > 1) {
        map <-
          leaflet::addLayersControl(
            map,
            position = control.position,
            options = leaflet::layersControlOptions(collapsed = control.collapsed, autoZIndex = FALSE),
            baseGroups = names(provider)
          ) %>%
          leaflet::hideGroup(group = names(provider)[[-1]])
      }
    }

    # multiple sources - add legend
    if (length(source) > 1 & legend) {
      map <-
        leaflet::addLegend(
          map,
          opacity = 1,
          position = check_legendposition(legend.position, static = FALSE),
          title = "Network",
          colors = cols$realcolour,
          labels = paste0("<span style='line-height:1.6'>", cols$network, "</span>")
        )
    }

    map
  }

#' Function to prep network data
#' @param source source (from parent)
#' @param year year (from parent)
#' @noRd
prepNetworkData <- function(source, year) {
  if (source == "saqd") {
    source <- "saqn"
  }
  # import metadata
  meta <-
    openair::importMeta(
      source = source,
      all = TRUE,
      year = year
    ) %>%
    dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude)) %>%
    dplyr::mutate(
      network = dplyr::case_when(
        source %in% c("local", "lmam") ~ "Locally Managed",
        source == "europe" ~ "Europe",
        TRUE ~ toupper(source)
      )
    )

  names(meta)[names(meta) %in% c("date_start", "OpeningDate")] <-
    "start_date"
  names(meta)[names(meta) %in% c("date_end", "ClosingDate")] <-
    "end_date"

  # drop HC vars
  if ("variable" %in% names(meta)) {
    hc_vars <- c(
      "ETHANE",
      "ETHENE",
      "ETHYNE",
      "PROPANE",
      "PROPENE",
      "iBUTANE",
      "nBUTANE",
      "1BUTENE",
      "t2BUTENE",
      "c2BUTENE",
      "iPENTANE",
      "nPENTANE",
      "13BDIENE",
      "t2PENTEN",
      "1PENTEN",
      "2MEPENT",
      "ISOPRENE",
      "nHEXANE",
      "nHEPTANE",
      "iOCTANE",
      "nOCTANE",
      "BENZENE",
      "TOLUENE",
      "ETHBENZ",
      "mpXYLENE",
      "oXYLENE",
      "123TMB",
      "124TMB",
      "135TMB",
      "c2PENTEN",
      "MEPENT",
      "3MEPENT",
      "NAPHTH"
    )

    meta <- dplyr::filter(
      meta, !.data$variable %in% hc_vars, !.data$variable %in% c(
        "ws",
        "wd",
        "temp",
        "NV10",
        "V10",
        "NV2.5",
        "V2.5",
        "PM1",
        "BC"
      )
    )
  }

  # network-specific manipulations
  if (!source %in% c("kcl", "europe")) {
    if (source %in% c("local", "lmam")) {
      meta <-
        prepManagedNetwork(
          meta,
          c(
            "code",
            "site",
            "site_type",
            "latitude",
            "longitude",
            "network",
            "zone",
            "agglomeration",
            "provider"
          )
        ) %>%
        dplyr::mutate(
          provider = stringr::str_trim(.data$provider),
          pcode = dplyr::case_when(
            .data$provider == "Norfolk Air Quality" ~ "norfolk",
            .data$provider == "Nottingham Air Quality" ~ "notts",
            .data$provider == "Wolverhampton Air Quality" ~ "wolverhampton",
            .data$provider == "Liverpool Air Quality" ~ "liverpool",
            .data$provider == "Heathrow Airwatch" ~ "heathrow",
            .data$provider == "Hertfordshire and Bedfordshire Air Quality Network" ~ "hertsbeds",
            .data$provider == "Wiltshire Air Quality" ~ "wiltshire",
            .default = .data$pcode
          ),
          lab = stringr::str_glue(
            "<u><a href='https://uk-air.defra.gov.uk/networks/site-info?uka_id={code}&provider={pcode}'><b>{toupper(stringr::str_to_title(site))}</b> ({code})</a></u><br>
            <b>Lat:</b> {latitude} | <b>Lon:</b> {longitude}<br>
            <b>Network:</b> {network}<br>
            <b>Site Type:</b> {site_type}<br>
            <b>Zone:</b> {zone}<br>
            <b>Agglomeration:</b> {agglomeration}<br>
            <b>Provider:</b> {provider}<br>
            <hr>{lab}"
          )
        ) %>%
        dplyr::mutate(
          lab = stringr::str_remove_all(.data$lab, "<b>Agglomeration:</b> NA<br>"),
          lab = stringr::str_remove_all(.data$lab, "<b>Site Type:</b> unknown unknown<br>")
        )
    } else {
      domain <- switch(source,
        "aurn" = "https://uk-air.defra.gov.uk/networks/site-info?site_id=",
        "saqn" = "https://www.scottishairquality.scot/latest/site-info/",
        "saqd" = "https://www.scottishairquality.scot/latest/site-info/",
        "waqn" = "https://airquality.gov.wales/air-pollution/site/",
        "ni" = "https://www.airqualityni.co.uk/site/",
        "aqe" = "https://www.airqualityengland.co.uk/site/latest?site_id="
      )

      meta <-
        prepManagedNetwork(
          meta,
          c(
            "code",
            "site",
            "site_type",
            "network",
            "latitude",
            "longitude",
            "zone",
            "agglomeration",
            "local_authority"
          )
        ) %>%
        dplyr::mutate(
          lab = stringr::str_glue(
            "<u><a href='{domain}{code}'><b>{toupper(stringr::str_to_title(site))}</b> ({code})</a></u><br>
      <b>Lat:</b> {latitude} | <b>Lon:</b> {longitude}<br>
      <b>Network:</b> {network}<br>
      <b>Site Type:</b> {site_type}<br>
      <b>Zone:</b> {zone}<br>
      <b>Agglomeration:</b> {agglomeration}<br>
      <b>Local Authority:</b> {local_authority}<br>
      <hr>{lab}"
          )
        ) %>%
        dplyr::mutate(
          lab = stringr::str_remove_all(.data$lab, "<b>Agglomeration:</b> NA<br>"),
          lab = stringr::str_remove_all(.data$lab, "<b>Local Authority:</b> NA<br>")
        )
    }
  }

  if (source == "kcl") {
    # create labels
    meta <-
      dplyr::mutate(
        meta,
        url = paste0("https://www.londonair.org.uk/london/asp/publicdetails.asp?site=", .data$code),
        start_date = lubridate::as_date(.data$start_date),
        end_date = lubridate::as_date(.data$end_date),
        end_date = dplyr::if_else(
          is.na(.data$end_date),
          "ongoing",
          as.character(.data$end_date)
        ),
        lab = stringr::str_glue(
          "<u><a href='{url}'><b>{toupper(stringr::str_to_title(site))}</b> ({code})</a></u><br>
          <b>Lat:</b> {round(latitude, 6)} | <b>Lon:</b> {round(longitude, 6)}<br>
          <b>Network:</b> {network}<br>
          <b>Address:</b> {Address}<br>
          <b>Site Type:</b> {site_type}<br>
          <b>Authority:</b> {Authority} ({la_id})<hr>
          {start_date} - {end_date}"
        )
      )
  }

  if (source == "europe") {
    # create labels
    meta <-
      dplyr::mutate(
        meta,
        site = dplyr::if_else(is.na(.data$site), "Unknown Name", .data$site),
        start_date = lubridate::as_date(.data$start_date),
        start_date = dplyr::if_else(
          is.na(.data$start_date),
          "unknown start",
          as.character(.data$start_date)
        ),
        end_date = lubridate::as_date(.data$end_date),
        end_date = dplyr::if_else(
          is.na(.data$end_date),
          "ongoing",
          as.character(.data$end_date)
        ),
        lab = stringr::str_glue(
          "<u><b>{toupper(stringr::str_to_title(site))}</b> ({code})</u><br>
          <b>Lat:</b> {round(latitude, 6)} | <b>Lon:</b> {round(longitude, 6)}<br>
          <b>Network:</b> {network}<br>
          <b>Country:</b> {stringr::str_to_title(country)} ({country_iso_code})<br>
          <b>Site Type:</b> {stringr::str_to_title(site_type)}<br>
          <b>Site Area:</b> {stringr::str_replace(site_area, '_', ' ') %>% stringr::str_to_title()}<hr>
          {start_date} - {end_date}"
        )
      )
  }

  return(meta)
}

#' function to prep AURN and LMAM data for plotting
#' not used for Europe and KCL
#' @param data metadata
#' @param vec char vector of columns - used for grouping/joining
#' @param date from parent func
#' @noRd
prepManagedNetwork <- function(data, vec) {
  # get variable names
  vars <- unique(data$variable)
  vars[vars == "NO"] <- "NOx"

  # create labels
  data <- data %>%
    dplyr::filter(.data$variable != "NO") %>%
    dplyr::mutate(
      lab = stringr::str_glue(
        "<b>{quickTextHTML(Parameter_name)} ({quickTextHTML(variable)})</b><br>{start_date} - {end_date}"
      )
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vec))) %>%
    dplyr::summarise(
      lab = paste(.data$lab, collapse = "<br>"),
      .groups = "drop"
    ) %>%
    dplyr::right_join(data,
      by = vec
    )

  return(data)
}
