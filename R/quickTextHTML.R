##' Automatic text formatting for openairmaps
##'
##' Workhorse function that automatically applies routine text formatting to
##' common pollutant names which may be used in the HTML widgets produced by
##' openairmaps.
##'
##' \code{quickTextHTML} is routine formatting lookup table. It screens the
##' supplied character vector \code{text} and automatically applies formatting
##' to any recognised character sub-series to properly render in HTML.
##'
##' @param text A character vector.
##' @export
##' @return The function returns an expression for HTML evaluation.
##' @author Jack Davison.
##' @keywords methods
##'

quickTextHTML <- function(text) {
  text <- gsub("NO2|no2", "NO<sub>2</sub>", text)
  text <- gsub("nox|NOx|NOX", "NO<sub>x</sub>", text)
  text <- gsub("CO2|co2", "CO<sub>2</sub>", text)
  text <- gsub("o3|O3", "O<sub>3</sub>", text)
  text <- gsub("SO2|so2", "SO<sub>2</sub>", text)
  text <- gsub("SO2|so2", "SO<sub>2</sub>", text)
  text <- gsub("\\bno\\b", "NO", text)
  text <- gsub("pm25|PM25|pm2.5|PM2.5", "PM<sub>2.5</sub>", text)
  text <- gsub("pm10|PM10", "PM<sub>10</sub>", text)
  text <- gsub("nh3|NH3", "NH<sub>3</sub>", text)

  return(text)
}
