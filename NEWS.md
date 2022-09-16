# openairmaps (development version)

* Fixed issue with `trajMap()` that would cause user-defined colours not to work.

# openairmaps 0.4.3

## Features

* `trajLevelMap()` now contains the `lat.inc` and `lon.inc` arguments.

## Refactoring

* `polar_data` column names changed from "latitude" to "longitude" to "lat" and "lon" to reflect the defaults of the `polarMap()` family.

* `trajMap()` and `trajLevelMap()` now use the argument names "latitude" and "longitude" to match those of the `polarMap()` family.

# openairmaps 0.4.2 (2022-09-12)

* `trajLevelMap()` now works where `statistic = "frequency"` without a "pollutant".

## Breaking Changes

# openairmaps 0.4.1 (2022-09-09)

## Features

* There are currently three streams of functionality in `openairmaps`:

  * `networkMap()` visualises any of the `openair::importMeta()` networks.

  * `polarMap()` and family allow for any of the `openair` directional analysis plots to be used as leaflet markers. 

  * `trajMap()` and family are leaflet equivalents to `openair::trajPlot()` and `openair::trajMap()`.

* There are two main classes of functions:

  * `*Map()` functions are easy-to-use functions which create leaflet maps from the ground-up. These are the most similar to `openair` functions.
  
  * `add*()` functions are more flexible and allow users to add layers to existing leaflet maps. These are designed to be similar to the `leaflet` "add" functions like `addMarkers()`.

## Package Management

* First submission to CRAN.

* Added a `NEWS.md` file to track changes to the package.
