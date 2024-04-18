#' Automatic text formatting for openairmaps
#'
#' Workhorse function that automatically applies routine text formatting to
#' common pollutant names which may be used in the HTML widgets produced by
#' `openairmaps`.
#'
#' [quickTextHTML()] is routine formatting lookup table. It screens the supplied
#' character vector `text` and automatically applies formatting to any
#' recognised character sub-series to properly render in HTML.
#'
#' @param text *A character vector.*
#'
#'  **required**
#'
#'   A character vector containing common pollutant names to be formatted.
#'   Commonly, this will insert super- and subscript HTML tags, e.g., "NO2" will
#'   be replaced with "NO<sub>2</sub>".
#'
#' @export
#' @returns a character vector
#' @author Jack Davison.
#' @keywords methods
#' @seealso [openair::quickText()], useful for non-HTML/static maps and plots
#'
#' @examples
#' labs <- c("no2", "o3", "so2")
#' quickTextHTML(labs)
#'
quickTextHTML <- function(text) {
  text <- gsub("NO2|no2|No2", "NO<sub>2</sub>", text)
  text <- gsub("\\bnox\\b|\\bNOx\\b|\\bNox\\b|\\bNOX\\b", "NO<sub>x</sub>", text)
  text <- gsub("CO2|co2|Co2", "CO<sub>2</sub>", text)
  text <- gsub("o3|O3", "O<sub>3</sub>", text)
  text <- gsub("SO2|so2|So2", "SO<sub>2</sub>", text)
  text <- gsub("\\bno\\b|\\bNO\\b|\\bNo\\b", "NO", text)
  text <- gsub("pm25|PM25|pm25|pm2.5|PM2.5|Pm2.5", "PM<sub>2.5</sub>", text)
  text <- gsub("pm10|PM10|Pm10", "PM<sub>10</sub>", text)
  text <- gsub("pm1|PM1|Pm1", "PM<sub>1</sub>", text)
  text <- gsub("nh3|NH3|Nh3", "NH<sub>3</sub>", text)
  text <- gsub("nv10|NV10|Nv10", "NV<sub>10</sub>", text)
  text <- gsub("v10|V10", "V<sub>10</sub>", text)
  text <- gsub("nv25|NV25|Nv25|nv2.5|NV2.5|Nv2.5", "NV<sub>2.5</sub>", text)
  text <- gsub("v25|V25|v2.5|V2.5", "V<sub>2.5</sub>", text)
  text <- gsub("ug/m3", "\u00B5g m<sup>-3</sup>", text)
  text <- gsub("m/s", "m s<sup>-1</sup>", text)

  return(text)
}



#' Build a Complex Popup for a Leaflet Map
#'
#' Group a dataframe together by latitude/longitude columns and create a HTML
#' popup with user-defined columns. By default, the unique values of character
#' columns are collapsed into comma-separated lists, numeric columns are
#' averaged, and date columns are presented as a range. This function returns
#' the input dataframe appended with a "popup" column, which can then be used in
#' the `popup` argument of a function like [polarMap()].
#'
#' @inheritParams polarMap
#'
#' @param data *Input data table with geo-spatial information.*
#'
#'   **required**
#'
#'   A data frame containing latitude and longitude information that will go on
#'   to be used in a function such as [polarMap()].
#'
#' @param columns *A character vector of column names to include in the popup.*
#'
#'   **required**
#'
#'   Summaries of the selected columns will appear in the popup. If a named
#'   vector is provided, the names of the vector will be used in place of the
#'   raw column names. See the Examples for more information.
#'
#' @param type *A column to be passed to the `type` argument of another
#'   function.*
#'
#'  *default:* `NULL`
#'
#'   Column which will be used for the `type` argument of other mapping
#'   functions. This only needs to be used if `type` is going to be used in
#'   [polarMap()] or another similar function, and you'd expect different values
#'   for the different map layers (for example, if you are calculating a mean
#'   pollutant concentration).
#'
#' @param fun.character *A function to summarise character and factor columns.*
#'
#'  *default:* `function(x) paste(unique(x), collapse = ", ")`
#'
#'   The default collapses unique values into a comma-separated list.
#'
#' @param fun.numeric *A function to summarise numeric columns.*
#'
#'  *default:* `function(x) signif(mean(x, na.rm = TRUE), 3)`
#'
#'   The default takes the mean to three significant figures. Other numeric
#'   summaries may be of interest, such as the maximum, minimum, standard
#'   deviation, and so on.
#'
#' @param fun.dttm *A function to summarise date columns.*
#'
#'  *default:* `function(x) paste(lubridate::floor_date(range(x, na.rm = TRUE), "day"), collapse = " to ")`
#'
#'   The default presents the date as a range. Other statistics of interest
#'   could be the start or end of the dates.
#'
#' @param ... **Not currently used.**
#'
#' @returns a [tibble::tibble()]
#' @export
#'
#' @examples
#' \dontrun{
#' buildPopup(
#'   data = polar_data,
#'   columns = c(
#'     "Site" = "site",
#'     "Site Type" = "site_type",
#'     "Date Range" = "date"
#'   )
#' ) %>%
#'   polarMap("nox", popup = "popup")
#' }
buildPopup <-
  function(data,
           columns,
           latitude = NULL,
           longitude = NULL,
           type = NULL,
           fun.character = function(x) paste(unique(x), collapse = ", "),
           fun.numeric = function(x) signif(mean(x, na.rm = TRUE), 3),
           fun.dttm = function(x) paste(lubridate::floor_date(range(x, na.rm = TRUE), "day"), collapse = " to "),
           ...) {
    # check for old facet/control opts
    type <- type %||% check_facet_control(...)

    # assume latitude/longitude
    latlon <- assume_latlon(
      data = data,
      latitude = latitude,
      longitude = longitude
    )
    latitude <- latlon$latitude
    longitude <- latlon$longitude

    make_popup <- function(data) {
      # multiple columns
      summary <-
        data %>%
        dplyr::select(dplyr::all_of(c(latitude, longitude, as.vector(columns)))) %>%
        dplyr::group_by(.data[[latitude]], .data[[longitude]]) %>%
        dplyr::summarise(dplyr::across(dplyr::where(is.character) | dplyr::where(is.factor), fun.character),
          dplyr::across(dplyr::where(is.numeric), fun.numeric),
          dplyr::across(dplyr::where(lubridate::is.POSIXct), fun.dttm),
          .groups = "drop"
        )

      if (!is.null(names(columns))) {
        names(columns)[names(columns) == ""] <- columns[names(columns) == ""]
        summary <- dplyr::rename(summary, dplyr::all_of(columns))
      }

      out <-
        summary %>%
        dplyr::select(-dplyr::all_of(c(latitude, longitude))) %>%
        purrr::imodify(.f = ~ stringr::str_glue("<b>{.y}</b>: {.x}"))

      if (typeof(out) == "list") {
        out <- as.data.frame(out)
        names(out) <- as.vector(columns)
      }

      out <-
        out %>%
        dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = quickTextHTML)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          popup = paste(dplyr::c_across(cols = dplyr::everything()), collapse = "<br>"),
          .keep = "unused"
        )

      out[[latitude]] <- summary[[latitude]]
      out[[longitude]] <- summary[[longitude]]

      out <-
        dplyr::left_join(data, out, by = c(latitude, longitude))

      return(out)
    }

    if (!is.null(type)) {
      out <- dplyr::group_split(data, .data[[type]]) %>%
        purrr::map(make_popup) %>%
        purrr::list_rbind()
    } else {
      out <- make_popup(data)
    }

    out
  }

#' Convert a UK postcode to a latitude/longitude pair
#'
#' This is a much simpler implementation of the tools found in the
#' `PostcodesioR` R package, intended for use with the [searchNetwork()]
#' function.
#'
#' @param postcode *A valid UK postcode.*
#'
#'    **required**
#'
#'    A string containing a single valid UK postcode, e.g., `"SW1A 1AA"`.
#'
#' @returns A list containing the latitude, longitude, and input postcode.
#' @export
#' @examples
#' # convert a UK postcode
#' convertPostcode("SW1A1AA")
#'
#' \dontrun{
#' # use with `searchNetwork()`
#' palace <- convertPostcode("SW1A1AA")
#' searchNetwork(lat = palace$lat, lng = palace$lng, max_dist = 10)
#' }
#'
#' @seealso The `PostcodesioR` package at
#'   <https://github.com/ropensci/PostcodesioR/>
#' @source <https://postcodes.io/>
convertPostcode <- function(postcode) {
  if (!requireNamespace("httr", quietly = TRUE) |
      !requireNamespace("jsonlite", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "x" = "Please install the {.pkg httr} and {.pkg jsonlite} packages to use {.fun convertPostcode}.",
        "i" = 'To do so, run {.code install.packages(c("httr", "jsonlite"))}'
      )
    )
  }


  postcode <- stringr::str_remove_all(postcode, " ")
  api <- stringr::str_glue("api.postcodes.io/postcodes/{postcode}")
  get <- httr::GET(api)
  out <- rawToChar(get$content) %>%
    jsonlite::parse_json()

  if (out$status == 404L) {
    cli::cli_abort("'{postcode}' is not a valid UK postcode.")
  }

  list(
    lat = out$result$latitude,
    lng = out$result$longitude,
    postcode = out$result$postcode
  )
}
