
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openairmaps: tools to create maps of air pollution data <img src="man/figures/logo.png" align="right" height="134" alt="the openairmaps logo. It shows a stylised pollution rose overlaid with a typical teardrop shaped map marker." />

<!-- badges: start -->

[![R-CMD-check](https://github.com/davidcarslaw/openairmaps/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/davidcarslaw/openairmaps/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/openairmaps)](https://CRAN.R-project.org/package=openairmaps)
<!-- badges: end -->

The main goal of `{openairmaps}` is to combine the robust analytical
methods found in [openair](https://davidcarslaw.github.io/openair/) with
the highly capable `{leaflet}` package. `{openairmaps}` is thoroughly
documented in the [openair
book](https://bookdown.org/david_carslaw/openair/sections/maps/maps-overview.html).

## Installation

You can install the release version of `{openairmaps}` from CRAN with:

``` r
install.packages("openairmaps")
```

You can install the development version of `{openairmaps}` from GitHub
with:

``` r
# install.packages("pak")
pak::pak("davidcarslaw/openairmaps")
```

## Overview

``` r
library(openairmaps)
```

The `openairmaps` package is thoroughly documented in the [openair
book](https://bookdown.org/david_carslaw/openair/sections/maps/maps-overview.html),
which goes into great detail about its various functions. Functionality
includes visualising UK AQ networks (`networkMap()`), putting “polar
directional markers” on maps (e.g., `polarMap()`) and overlaying HYSPLIT
trajectories on maps (e.g., `trajMap()`), all using the `{leaflet}`
package.

``` r
polar_data %>%
  openair::cutData("daylight") %>%
  buildPopup(
    c("site", "site_type"),
    names = c("Site" = "site", "Site Type" = "site_type"),
    control = "daylight"
  ) %>%
  polarMap(
    pollutant = "no2",
    limits = c(0, 180),
    control = "daylight",
    popup = "popup"
  )
```

<img src="man/figures/README-examplemap.png" alt="A screenshot of a leaflet map. It shows an OpenStreetMap map layer, overlaid with bivariate polar plots. Polar plots are visualisations on polar coordinates with wind direction on the spoke axes, wind speed on the radial axes, and a smooth surface showing pollutant concentrations. A menu is found at the top-right of the map, which allows users to swap between daylight and nighttime observations." width="100%" />

While an interactive map is preferred for exploratory directional
analysis, it is limited to the HTML format. Some applications (for
example, academic journals) demand “static” formats like .docx and .pdf.
For this reason, “static” versions of `{openairmaps}` polar marker
functions powered by `{ggplot2}` can be created using the `static`
argument. A benefit of being written in `{ggplot2}` is that additional
layers can be added (e.g., `ggplot2::geom_label_sf()` could be used to
label sites) and limited further customisation is available using
`ggplot2::theme()` and `ggplot2::guides()`.

``` r
polar_data %>%
  openair::cutData("daylight") %>%
  polarMap(
    pollutant = "no2",
    limits = c(0, 180),
    type = "daylight",
    static = TRUE,
    d.fig = 2.5,
    d.icon = 150,
    alpha = 0.75
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.key.width = ggplot2::unit(1, "null"),
    legend.title.position = "top",
    legend.title = ggplot2::element_text(hjust = .5)
  ) +
  ggplot2::labs(color = openair::quickText("NO2 (ug/m3)"))
```

<img src="man/figures/README-egstatic-1.png" alt="Two static maps. They show OpenStreetMap map layers, overlaid with bivariate polar plots. Polar plots are visualisations on polar coordinates with wind direction on the spoke axes, wind speed on the radial axes, and a smooth surface showing pollutant concentrations. The two maps represent daylight and nighttime observations separately." width="100%" />
