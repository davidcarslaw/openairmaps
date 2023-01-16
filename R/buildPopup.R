#' Build a Complex Popup for a Leaflet Map
#'
#' Group a dataframe together by latitude/longitude columns and create a HTML
#' popup with user-defined columns. By default, the unique values of character
#' columns are collapsed into comma-separated lists, numeric columns are
#' averaged, and date columns are presented as a range. This function returns
#' the input dataframe appended with a "popup" column, which can then be used in
#' the `popup` argument of a function like [polarMap()].
#'
#' @param data A data frame containing latitude and longitude information that
#'   will go on to be used in a function such as [polarMap()].
#' @param cols A character vector of column names, the data from which will
#'   appear in the popup.
#' @param latitude,longitude The decimal latitude/longitude. If not provided,
#'   will be automatically inferred from data by looking for a column named
#'   "lat"/"latitude" or "lon"/"lng"/"long"\"longitude". (case-insensitively).
#' @param names Optional. A named vector used to rename certain columns in the
#'   popups. See the Example for more information.
#' @param control Optional. Column which will be used for the `control` argument
#'   of other mapping functions. This only needs to be used if `control` is
#'   going to be used in [polarMap()] or another similar function, and you'd
#'   expect different values for the different map layers (for example, if you
#'   are calculating a mean pollutant concentration).
#' @param fun.character A function to summarise character and factor columns.
#'   Defaults to collapsing unique values into a comma-separated list.
#' @param fun.numeric A function to summarise numeric columns. Defaults to
#'   taking the mean to three significant figures.
#' @param fun.dttm A function to summarise date columns. Defaults to presenting
#'   the date as a range.
#'
#' @return a [tibble::tibble()]
#' @export
#'
#' @examples
#' \dontrun{
#' buildPopup(
#'   data = openairmaps::polar_data,
#'   cols = c("site", "site_type", "date", "nox"),
#'   names = c("Site" = "site", "Site Type" = "site_type", "Date Range" = "date")
#' ) %>%
#'   polarMap("nox", popup = "popup")
#' }
buildPopup <-
  function(data,
           cols,
           latitude = NULL,
           longitude = NULL,
           names = NULL,
           control = NULL,
           fun.character = function(x) paste(unique(x), collapse = ", "),
           fun.numeric = function(x) signif(mean(x, na.rm = TRUE), 3),
           fun.dttm = function(x) paste(lubridate::floor_date(range(x, na.rm = TRUE), "day"), collapse = " to ")) {
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
        dplyr::select(dplyr::all_of(c(latitude, longitude, cols))) %>%
        dplyr::group_by(.data[[latitude]], .data[[longitude]]) %>%
        dplyr::summarise(dplyr::across(tidyselect::where(is.character) | tidyselect::where(is.factor), fun.character),
          dplyr::across(tidyselect::where(is.numeric), fun.numeric),
          dplyr::across(tidyselect::where(lubridate::is.POSIXct), fun.dttm),
          .groups = "drop"
        )

      if (!is.null(names)) {
        summary <- dplyr::rename(summary, dplyr::all_of(names))
      }

      out <-
        summary %>%
        dplyr::select(-dplyr::all_of(c(latitude, longitude))) %>%
        purrr::imodify(.f = ~ stringr::str_glue("<b>{.y}</b>: {.x}"))

      if (typeof(out) == "list") {
        out <- as.data.frame(out)
        names(out) <- cols
      }

      out <-
        out %>%
        dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = quickTextHTML)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          popup = paste(dplyr::c_across(), collapse = "<br>"),
          .keep = "unused"
        )

      out[[latitude]] <- summary[[latitude]]
      out[[longitude]] <- summary[[longitude]]

      out <-
        dplyr::left_join(data, out, by = c(latitude, longitude))

      return(out)
    }

    if (!is.null(control)) {
      out <- dplyr::group_split(data, .data[[control]]) %>%
        purrr::map(make_popup) %>%
        purrr::list_rbind()
    } else {
      out <- make_popup(data)
    }

    out
  }
