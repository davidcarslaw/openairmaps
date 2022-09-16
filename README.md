
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openairmaps

<!-- badges: start -->

[![R-CMD-check](https://github.com/davidcarslaw/openairmaps/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/davidcarslaw/openairmaps/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `{openairmaps}` is to combine the robust analytical methods
found in [openair](https://davidcarslaw.github.io/openair/) with the
highly capable `{leaflet}` package.

## Installation

You can install the release version of `{openairmaps}` from CRAN with:

``` r
install.packages("openairmaps")
```

You can install the development version of `{openairmaps}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("davidcarslaw/openairmaps")
```

## Directional Analysis

### Pre-built Maps

`{openairmaps}` makes it simple to place “directional analysis” plots
(e.g., polar plots) on an interactive `{leaflet}` map. A static
screenshot is provided below, but if the below code is run in your R
session you’ll find the map can be scrolled, zoomed and otherwise
interacted with.

``` r
library(openairmaps)

polarMap(
  polar_data, 
  type = "site"
)
```

![A screenshot of `polarMap()` output.](man/figures/README-polarmap.png)

Multiple pollutants and/or base-maps can be provided, which uses the
`{leaflet}` package’s “layer control” interface to allow users to toggle
between them.

``` r
library(openairmaps)

annulusMap(
  polar_data,
  pollutant = c("no2", "nox"),
  provider = c("CartoDB.Positron", "OpenStreetMap"),
  type = "site"
)
```

![A screenshot of `annulusMap()` output, with a layer control
menu.](man/figures/README-annuluslayers.png)

As `{openairmaps}` outputs `{leaflet}` maps, you can use `{leaflet}`’s
own functions to further customise your `{openairmaps}` outputs.

``` r
library(openairmaps)

polarMap(
  polar_data,
  pollutant = c("no2", "nox"),
  provider = c("CartoDB.Positron", "OpenStreetMap"),
  type = "site"
) %>%
  leaflet::popupOptions(...) %>%
  leaflet::mapOptions(...) # etc.
```

### `{leaflet}` Tools

If you are comfortable with `{leaflet}`, `{openairmaps}` provides a more
flexible method of adding `{openair}` directional analysis functions;
the `addPolarMarkers()` function. This allows you to add a layer of
polar plots or other directional analysis plots in a similar way that
you add ordinary markers using `leaflet::addMarkers()`. Creating
`{leaflet}` plots in this way allows you better customisation of the
output. In the below example a map is generated that allows the user to
switch between wind roses and polar plots, for example.

``` r
library(openairmaps)
library(leaflet)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolarMarkers(
    lng = "lon",
    lat = "lat",
    data = polar_data,
    fun = openair::windRose,
    pollutant = "ws",
    type = "site",
    group = "Wind Rose"
  ) %>%
  addPolarMarkers(
    lng = "lon",
    lat = "lat",
    data = polar_data,
    fun = openair::polarPlot,
    pollutant = "nox",
    type = "site",
    group = quickTextHTML("NOx Polar Plot")
  ) %>%
  addLayersControl(baseGroups = quickTextHTML(c("Wind Rose", "NOx Polar Plot")))
```

![A more customised `{leaflet}` output using the `addPolarMakers()`
function.](man/figures/README-addMarkers.png)

## Trajectory Analysis

### Pre-built Maps

`{openairmaps}` makes it simple to plot air mass trajectories on
`{leaflet}` maps. `trajMap()` can plot the “raw” trajectories, while
`trajLevelMap()` wraps around `openair::trajLevel()` to plot trajectory
levels. The below level map shows the Potential Source Contribution
Function (PSCF) for PM<sub>2.5</sub>.

``` r
library(openairmaps)

trajLevelMap(
  traj_data,
  pollutant = "pm2.5",
  statistic = "pscf",
  min.bin = 10
)
```

![A `trajLevelMap()` output showing the PSCF for
PM.](man/figures/README-trajlevel.png)

Currently `{openairmaps}` can’t directly run `trajCluster()`, but
`trajMap()` is compatible with `trajCluster()` outputs as in the map
below. This uses the “colour” and “control” options of `trajMap()` to
distinguish between different clusters, and allow users to select
specific clusters of interest.

``` r
library(openairmaps)
library(openair)

clustdata <- trajCluster(traj_data)
trajMap(
  data = clustdata$data$traj,
  colour = "cluster",
  control = "cluster",
  provider = "CartoDB.Positron"
)
```

![Combining `openair::trajCluster()` and
`openairmaps::trajMap()`.](man/figures/README-trajcluster.png)

### `{leaflet}` Tools

Much like with the “directional analysis” plots, users more comfortable
with `{leaflet}` may choose to use a lower-level `addTrajPaths()`
function on a pre-existing `{leaflet}` map. The below code shows an
example of using `addTrajPaths()` to display multiple trajectories at
once – one with a receptor in the UK and the other in France.

``` r
library(openairmaps)
library(openair)
library(leaflet)

france <- importTraj("paris", year = 2009) %>%
  selectByDate(
    start = "15/4/2009",
    end = "21/4/2009"
  )

uk <- importTraj(year = 2009) %>%
  selectByDate(
    start = "15/4/2009",
    end = "21/4/2009"
  )

leaflet() %>%
  addTiles() %>%
  addTrajPaths(data = uk,
               color = "blue",
               group = "London, UK") %>%
  addTrajPaths(data = france,
               color = "red",
               group = "Paris, France") %>%
  addLayersControl(overlayGroups = c("Paris, France", "London, UK"))
```

![A more customised `{leaflet}` output using the `addTrajPaths()`
function.](man/figures/README-addtrajpath.png)
