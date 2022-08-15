
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openairmaps

The goal of `openairmaps` is to combine the robust analytical methods
found in [openair](https://davidcarslaw.github.io/openair/) with the
highly capable `leaflet` package.

## Installation

You can install the development version of openairmaps from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("davidcarslaw/openairmaps")
```

## Example

`openairmaps` makes it simple to place polar plots on an interactive
`leaflet` map. A static screenshot is provided below, but if the below
code is run in your R session you’ll find the map can be scrolled,
zoomed and otherwise interacted with.

``` r
library(openairmaps)

polarMap(
  polar_data, 
  latitude = "latitude", 
  longitude = "longitude"
)
```

![A screenshot of `polarMap()` output.](man/figures/README-polarmap.png)
