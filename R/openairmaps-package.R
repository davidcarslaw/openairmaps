#' @details This is a companion package to `openair`, a UK NERC- and
#'   Defra-funded R package for the analysis of data pertaining to pollution
#'   monitoring and dispersion modelling.
#'
#'   As the R ecosystem has developed, R Markdown and, more recently, Quarto
#'   have emerged as capable tools for combining data analysis with document
#'   preparation. While these approaches can render typical .docx and .pdf
#'   outputs, one of their most common output formats is the HTML document. This
#'   format has many strengths, but a key one is interactivity; HTML widgets
#'   allow documents to be more informative and engaging. Numerous packages have
#'   been developed to easily develop these interactive widgets, such as
#'   `plotly` and `dygraphs` for plots, `DT` for tables, and `leaflet` for maps.
#'   The `openairmaps` package concerns itself with making `leaflet` maps.
#'
#'   Air quality data analysis — particularly as it pertains to long term
#'   monitoring data — naturally lends itself to being visualised spatially on a
#'   map. Monitoring networks are geographically distributed, and ignoring their
#'   geographical context may lead to incomplete insights at best and incorrect
#'   conclusions at worst! Furthermore, many air quality analysis tools are
#'   directional, asking questions of the data along the lines of “do elevated
#'   concentrations come from the North, South, East or West?” The natural
#'   question that follows is “well, what actually is it to the
#'   North/South/East/West that could be causing elevated concentrations?” — a
#'   map can help answer that question straightforwardly.
#'
#'   The `openairmaps` package contains functions to visualise UK air quality
#'   networks, and place "polar analysis" markers (like the `openair` [polar
#'   plot][openair::polarPlot()]) and airmass trajectory paths on maps. It uses
#'   a similar syntax to the `openair` package, which should make moving between
#'   the two relatively seamless.
#'
#' @seealso The `openair` package, from which `openairmaps` is based.
#' @seealso The `worldmet` package, which simplifies the access of
#'   meteorological data in R.
#' @seealso The [openair book](https://bookdown.org/david_carslaw/openair/) for
#'   more in-depth documentation of `openair` and `openairmaps`.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom tibble tibble
#' @importFrom rlang .data
## usethis namespace: end
NULL
