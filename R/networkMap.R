#' Create a leaflet map of air quality measurement network sites
#'
#' This function uses [openair::importMeta()] to obtain metadata for measurement
#' sites and creates an attractive \code{leaflet} map.
#'
#' @param source The data source for the meta data to be passed to
#'   [openair::importMeta()]. Can be \dQuote{aurn}, \dQuote{saqn} (or
#'   \dQuote{saqd}), \dQuote{aqe}, \dQuote{waqn}, \dQuote{ni}, \dQuote{kcl} or
#'   \dQuote{europe}.
#' @param control Option to add a "layer control" menu to allow readers to
#'   select between different site types. Can choose between effectively any
#'   column in the [openair::importMeta()] output, such as \dQuote{variable},
#'   \dQuote{site_type}, or \dQuote{agglomeration}.
#' @param date By default, [networkMap()] visualises sites and pollutants which
#'   are currently operational. Specifying \code{date} will visualise sites
#'   which were operational at the chosen date. Dates should be provided in the
#'   \dQuote{YYYY-MM-DD} format.
#' @param cluster When \code{cluster = TRUE}, markers are clustered together.
#'   This may be useful for sources like \dQuote{kcl} where there are many
#'   markers very close together. Defaults to \code{TRUE}, and is forced to be
#'   \code{TRUE} when \code{source = "europe"} due to the large number of sites.
#' @param provider The base map to be used. See
#'   \url{http://leaflet-extras.github.io/leaflet-providers/preview/} for a list
#'   of all base maps that can be used.
#'
#' @return A leaflet object.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' networkMap(source = "aurn", control = "site_type")
#' }
#'
networkMap <-
  function(source = "aurn", control, date = Sys.Date(), cluster = TRUE, provider = "OpenStreetMap") {

  date = as.character(date)
  date = lubridate::ymd(date, tz = "GMT")
  if(source == "europe") date = lubridate::force_tz(date, "UTC")

  # import metadata
  meta <- openair::importMeta(source = source, all = TRUE) %>%
    dplyr::filter(
      !is.na(.data$latitude),
      !is.na(.data$longitude)
    )

  names(meta)[names(meta) %in% c("date_start", "OpeningDate")] <- "start_date"
  names(meta)[names(meta) %in% c("date_end", "ClosingDate")] <- "end_date"

  # drop HC vars
  if("variable" %in% names(meta)){
    hc_vars <- c(
      "ETHANE", "ETHENE", "ETHYNE", "PROPANE", "PROPENE", "iBUTANE",
      "nBUTANE", "1BUTENE", "t2BUTENE", "c2BUTENE", "iPENTANE",
      "nPENTANE", "13BDIENE", "t2PENTEN", "1PENTEN", "2MEPENT",
      "ISOPRENE", "nHEXANE", "nHEPTANE", "iOCTANE", "nOCTANE",
      "BENZENE", "TOLUENE", "ETHBENZ", "mpXYLENE", "oXYLENE",
      "123TMB", "124TMB", "135TMB", "c2PENTEN", "MEPENT", "3MEPENT"
    )

    meta <- dplyr::filter(
      meta,
      !.data$variable %in% hc_vars,
      !.data$variable %in% c("ws", "wd", "temp", "NV10", "V10", "NV2.5", "V2.5", "PM1")
    )
  }

  # cluster option
  if (cluster | source == "europe") {
    clusteropts <- leaflet::markerClusterOptions()
  } else {
    clusteropts <- NA
  }

  # initialise map
  map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(provider = provider)

  # network-specific manipulations
  if (!source %in% c("kcl", "europe")) {

    # get variable names
    vars <- unique(meta$variable)
    vars[vars == "NO"] <- "NOx"

    # create labels
    meta <- meta %>%
      dplyr::filter(.data$variable != "NO") %>%
      dplyr::mutate(
        lab = stringr::str_glue(
          "<b>{quickTextHTML(Parameter_name)} ({quickTextHTML(variable)})</b><br>{start_date} - {end_date}"
        )
      ) %>%
      dplyr::group_by(
        .data$code,
        .data$site,
        .data$site_type,
        .data$latitude,
        .data$longitude,
        .data$zone,
        .data$agglomeration,
        .data$local_authority
      ) %>%
      dplyr::summarise(
        lab = paste(.data$lab, collapse = "<br>"),
        .groups = "drop"
      ) %>%
      dplyr::right_join(
        meta,
        by = c(
          "code",
          "site",
          "site_type",
          "latitude",
          "longitude",
          "zone",
          "agglomeration",
          "local_authority"
        )
      ) %>%
      dplyr::mutate(
        lab = stringr::str_glue(
          "<u><b>{toupper(stringr::str_to_title(site))}</b> ({code})</u><br>
      <b>Lat:</b> {latitude} | <b>Lon:</b> {longitude}<br>
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

    # format and filter dates
    meta <- dplyr::mutate(
      meta,
      end_date2 = dplyr::if_else(
        .data$end_date == "ongoing" | is.na(.data$end_date),
        as.character(Sys.Date()),
        .data$end_date
      ),
      end_date2 = lubridate::ymd(.data$end_date2, tz = "GMT"),
      start_date = lubridate::with_tz(.data$start_date, tz = "GMT")) %>%
      dplyr::filter(date >= .data$start_date,
                    date <= .data$end_date2)

  }

  if (source == "kcl") {

    # format and filter dates
    meta <- dplyr::mutate(
      meta,
      end_date2 = dplyr::if_else(
        is.na(.data$end_date),
        Sys.Date(),
        lubridate::as_date(.data$end_date)
      ),
      start_date = lubridate::with_tz(.data$start_date, tz = "GMT"),
      end_date2 = lubridate::with_tz(.data$end_date2, tz = "GMT")) %>%
      dplyr::filter(date >= .data$start_date,
                    date <= .data$end_date2,
                    .data$`os_grid_x` != 0,
                    .data$`os_grid_y` != 0)

    # create labels
    meta <-
      dplyr::mutate(
        meta,
        start_date = lubridate::as_date(.data$start_date),
        end_date = lubridate::as_date(.data$end_date),
        end_date = dplyr::if_else(is.na(.data$end_date), "ongoing", as.character(.data$end_date)),
        lab = stringr::str_glue(
          "<u><b>{toupper(stringr::str_to_title(site))}</b> ({code})</u><br>
          <b>Lat:</b> {round(latitude, 6)} | <b>Lon:</b> {round(longitude, 6)}<br>
          <b>Address:</b> {Address}<br>
          <b>Site Type:</b> {site_type}<br>
          <b>Authority:</b> {Authority} ({la_id})<hr>
          {start_date} - {end_date}"
        )
      )
  }

  if (source == "europe") {
    # format and filter dates
    meta <- dplyr::mutate(
      meta,
      start_date2 = dplyr::if_else(
        is.na(.data$start_date),
        lubridate::as_date("1900-01-01"),
        lubridate::as_date(.data$start_date)
      ),
      end_date2 = dplyr::if_else(
        is.na(.data$end_date),
        Sys.Date(),
        lubridate::as_date(.data$end_date)
      )) %>%
      dplyr::filter(date >= .data$start_date2,
                    date <= .data$end_date2)

    # create labels
    meta <-
      dplyr::mutate(
        meta,
        site = dplyr::if_else(is.na(.data$site), "Unknown Name", .data$site),
        start_date = lubridate::as_date(.data$start_date),
        start_date = dplyr::if_else(is.na(.data$start_date), "unknown start", as.character(.data$start_date)),
        end_date = lubridate::as_date(.data$end_date),
        end_date = dplyr::if_else(is.na(.data$end_date), "ongoing", as.character(.data$end_date)),
        lab = stringr::str_glue(
          "<u><b>{toupper(stringr::str_to_title(site))}</b> ({code})</u><br>
          <b>Lat:</b> {round(latitude, 6)} | <b>Lon:</b> {round(longitude, 6)}<br>
          <b>Country:</b> {stringr::str_to_title(country)} ({country_iso_code})<br>
          <b>Site Type:</b> {stringr::str_to_title(site_type)}<br>
          <b>Site Area:</b> {stringr::str_replace(site_area, '_', ' ') %>% stringr::str_to_title()}<hr>
          {start_date} - {end_date}"
        )
      )
  }

  # build maps
  if (!missing(control)) {
    if (!control %in% names(meta)) {

      trycols <- names(meta)[!names(meta) %in%
                            c("code", "site", "latitude", "longitude", "country_iso_code",
                              "elevation", "ratified_to", "Address", "la_id",
                              "eu_code", "eoi_code", "data_source",
                              "os_grid_x", "os_grid_y", "start_date", "end_date",
                              "observation_count", "start_date2", "end_date2", "lab")]

      stop(paste0('"', control, '"', " is not an appropriate 'control' option.\n\n"),
           "Suggested control options: ", paste(trycols, collapse = ", "))
    }

    if (!control %in% c("Parameter_name", "variable")) {
      meta <- dplyr::distinct(meta, .data$code, .data$site, .keep_all = TRUE)
    }

    # ensure "control" is always present, and that "other" category is at the end
    meta[[control]][is.na(meta[[control]])] <- "Other"
    meta[[control]] <- factor(meta[[control]])
    if ("Other" %in% levels(meta[[control]])) {
      meta[[control]] <- forcats::fct_relevel(meta[[control]], "Other", after = Inf)
    }

    # get control variables
    control_vars <- sort(unique(meta[[control]]))
    control_vars <- control_vars[control_vars != "NO"]

    # add markers
    for (i in seq(length(control_vars))) {
      dat <- dplyr::filter(meta, .data[[control]] == control_vars[[i]])

      map <- map %>%
        leaflet::addMarkers(
          data = dat,
          lat = dat[["latitude"]],
          lng = dat[["longitude"]],
          group = quickTextHTML(control_vars[[i]]),
          popup = dat[["lab"]],
          label = dat[["site"]],
          clusterOptions = clusteropts
        )
    }

    if (control %in% c("Parameter_name", "variable")) {
      map <- leaflet::addLayersControl(map, baseGroups = quickTextHTML(sort(control_vars)))
    } else {
      map <- leaflet::addLayersControl(map, overlayGroups = quickTextHTML(sort(control_vars)))
    }
  } else {
    meta <- dplyr::distinct(meta, .data$code, .data$site, .keep_all = TRUE)

    dat <- dplyr::distinct(meta, .data$site, .keep_all = TRUE)

    map <- map %>%
      leaflet::addMarkers(
        data = dat,
        lat = dat[["latitude"]],
        lng = dat[["longitude"]],
        popup = dat[["lab"]],
        label = dat[["site"]],
        clusterOptions = clusteropts
      )
  }


  map
}
