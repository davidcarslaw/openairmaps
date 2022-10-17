# openairmaps (development version)

## Directional Analysis

* All functions now possess the "control" argument, which allows users to create a "layer control" menu with any arbitrary column. Appropriate columns may be those produced using `openair::cutData()`, `openair::splitByDate()`, or a user-defined `dplyr::case_when()`/`dplyr::if_else()` column transformation.

* All functions now possess the "popup" and "label" arguments, which control pop-up and hover-over labels, respectively. This allows users to define *any* popup or label column, even non-unique ones. For example, multiple sites can be labelled with identical site types.

* All functions now try to guess the latitude/longitude column if not provided, similar to `{leaflet}`.

* BREAKING: All functions now use latitude and longitude to distinguish between site types. Therefore, "type" is now deprecated. Maps using the old system will still render, but popups will not be displayed. For most users, to restore previous site labels simply rewrite `type = "site"` as `popup = "site"`.

* BREAKING: The default values for "pollutant" have all been removed. Any users relying on this default should update their code to explicitly state `pollutant = "nox"`.

## Trajectory Analysis

* Allowed `trajMap()` to be coloured by date.

* FIX: Fixed issue with `trajMap()` that would cause user-defined colours not to work.

## Other

* Updated many error messages and warnings to use `{cli}` and be broadly more descriptive.



# openairmaps 0.4.3 (2022-09-12)

## Features

* `trajLevelMap()` now contains the `lat.inc` and `lon.inc` arguments.

## Breaking Changes

* `polar_data` column names changed from "latitude" to "longitude" to "lat" and "lon" to reflect the defaults of the `polarMap()` family.

* `trajMap()` and `trajLevelMap()` now use the argument names "latitude" and "longitude" to match those of the `polarMap()` family.



# openairmaps 0.4.2 (2022-09-12)

* `trajLevelMap()` now works where `statistic = "frequency"` without a "pollutant".



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
