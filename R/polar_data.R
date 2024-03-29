#' Example data for polar mapping functions
#'
#' The `polar_data` dataset is provided as an example dataset as part of
#' the `openairmaps` package. The dataset contains hourly measurements of
#' air pollutant concentrations, location and meteorological data.
#'
#' `polar_data` is supplied with the `openairmaps` package as an
#' example dataset for use with documented examples.
#'
#' @name polar_data
#' @docType data
#' @format Data frame with example data from four sites in London in 2009.
#' \describe{
#'   \item{date}{The date and time of the measurement}
#'   \item{nox, no2, pm2.5, pm10}{Pollutant concentrations}
#'   \item{site}{The site name. Useful for use with the `popup` and `label` arguments in `openairmaps` functions.}
#'   \item{latitude, longitude}{Decimal latitude and longitude of the sites.}
#'   \item{site.type}{Site type of the site (either "Urban Traffic" or "Urban Background").}
#'   \item{wd}{Wind direction, in degrees from North, as a numeric vector.}
#'   \item{ws}{Wind speed, in m/s, as numeric vector.}
#'   \item{visibility}{The visibility in metres.}
#'   \item{air_temp}{Air temperature in degrees Celcius.}
#' }
#' @source `polar_data` was compiled from data using the
#'   [openair::importAURN()] function from the `openair` package with
#'   meteorological data from the `worldmet` package.
#'
#' @keywords datasets
#' @examples
#'
#' # basic structure
#' head(polar_data)
#'
NULL
