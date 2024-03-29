% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{buildPopup}
\alias{buildPopup}
\title{Build a Complex Popup for a Leaflet Map}
\usage{
buildPopup(
  data,
  cols,
  latitude = NULL,
  longitude = NULL,
  names = NULL,
  control = NULL,
  fun.character = function(x) paste(unique(x), collapse = ", "),
  fun.numeric = function(x) signif(mean(x, na.rm = TRUE), 3),
  fun.dttm = function(x) paste(lubridate::floor_date(range(x, na.rm = TRUE), "day"),
    collapse = " to ")
)
}
\arguments{
\item{data}{A data frame containing latitude and longitude information that
will go on to be used in a function such as \code{\link[=polarMap]{polarMap()}}.}

\item{cols}{A character vector of column names, the data from which will
appear in the popup.}

\item{latitude, longitude}{The decimal latitude/longitude. If not provided,
will be automatically inferred from data by looking for a column named
"lat"/"latitude" or "lon"/"lng"/"long"\"longitude". (case-insensitively).}

\item{names}{Optional. A named vector used to rename certain columns in the
popups. See the Example for more information.}

\item{control}{Optional. Column which will be used for the \code{control} argument
of other mapping functions. This only needs to be used if \code{control} is
going to be used in \code{\link[=polarMap]{polarMap()}} or another similar function, and you'd
expect different values for the different map layers (for example, if you
are calculating a mean pollutant concentration).}

\item{fun.character}{A function to summarise character and factor columns.
Defaults to collapsing unique values into a comma-separated list.}

\item{fun.numeric}{A function to summarise numeric columns. Defaults to
taking the mean to three significant figures.}

\item{fun.dttm}{A function to summarise date columns. Defaults to presenting
the date as a range.}
}
\value{
a \code{\link[tibble:tibble]{tibble::tibble()}}
}
\description{
Group a dataframe together by latitude/longitude columns and create a HTML
popup with user-defined columns. By default, the unique values of character
columns are collapsed into comma-separated lists, numeric columns are
averaged, and date columns are presented as a range. This function returns
the input dataframe appended with a "popup" column, which can then be used in
the \code{popup} argument of a function like \code{\link[=polarMap]{polarMap()}}.
}
\examples{
\dontrun{
buildPopup(
  data = openairmaps::polar_data,
  cols = c("site", "site_type", "date", "nox"),
  names = c("Site" = "site", "Site Type" = "site_type", "Date Range" = "date")
) \%>\%
  polarMap("nox", popup = "popup")
}
}
