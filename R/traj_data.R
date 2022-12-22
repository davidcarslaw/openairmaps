#' Example data for trajectory mapping functions
#'
#' The `traj_data` dataset is provided as an example dataset as part of the
#' `openairmaps` package. The dataset contains HYSPLIT back trajectory data for
#' air mass parcels arriving in London in 2009. It has been joined with air
#' quality pollutant concentrations from the "London N. Kensington" AURN urban
#' background monitoring site.
#'
#' `traj_data` is supplied with the `openairmaps` package as an example dataset
#' for use with documented examples.
#'
#' @format A data frame with 53940 rows and 10 variables: \describe{
#'   \item{date}{The arrival time of the air-mass} \item{receptor}{The receptor
#'   number} \item{year}{Trajectory year} \item{month}{Trajectory month}
#'   \item{day}{Trajectory day} \item{hour}{Trajectory hour}
#'   \item{hour.inc}{Trajectory hour offset from the arrival date}
#'   \item{lat}{Latitude} \item{lon}{Longitude} \item{height}{Height of
#'   trajectory in m} \item{pressure}{Pressure of the trajectory in Pa}
#'   \item{date2}{Date of the trajectory} \item{nox, no2, o3, pm10,
#'   pm2.5}{Pollutant concentrations} }
#' @source `traj_data` was compiled from data using the [openair::importTraj()]
#'   function from the `openair` package with air quality data from
#'   [openair::importAURN()] function.
#'
#' @keywords datasets
#' @examples
#'
#' # basic structure
#' head(traj_data)
"traj_data"
