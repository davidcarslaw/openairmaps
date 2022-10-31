##' Automatic text formatting for openairmaps
##'
##' Workhorse function that automatically applies routine text formatting to
##' common pollutant names which may be used in the HTML widgets produced by
##' \code{openairmaps}.
##'
##' [quickTextHTML()] is routine formatting lookup table. It screens the
##' supplied character vector \code{text} and automatically applies formatting
##' to any recognised character sub-series to properly render in HTML.
##'
##' @param text A character vector.
##' @export
##' @return The function returns an expression for HTML evaluation.
##' @author Jack Davison.
##' @keywords methods
##'
##' @examples
##' labs <- c("no2", "o3", "so2")
##' quickTextHTML(labs)

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
