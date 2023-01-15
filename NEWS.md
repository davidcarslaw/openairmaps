# openairmaps 0.6.1

* Fixed issue with polar marker maps (e.g., `polarMap()`) and the generic `addPolarMarkers()` function where lat/lon info in the Southern Hemisphere would misalign markers. Hat tip to Deanna Tuxford and James/"@jenright-git" for noticing this issue.

* Fixed an issue with `networkMap()` where `control = "variable"` would fail to show all pollutants.

* Functions now use Google's "turbo" colour palette rather than "jet" by default. More about this palette and the advantages of using it over "jet" can be read at <https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html>.

# openairmaps 0.6.0 (29/11/2022)

## Directional Analysis

* All directional analysis maps can now have their limits provided (can be "limits", "breaks", "percentiles", etc., depending on function). This was always possible through `...`, but it is now explicitly listed as an option.

* If limits are defined in a directional analysis function, a shared legend will now be drawn at the top-right of the map. This functionality can be disabled by setting `draw.legend` to FALSE.

* Added the `buildPopup()` function, which allows users to easily construct HTML popups for use with the "popup" argument of directional analysis maps (or `leaflet` maps more widely).

* All directional analysis maps now possess the `collapse.control` argument, which controls whether the control menu starts collapsed or not. It defaults to `FALSE`, which means the control menu is not collapsed.

* The default options for fig.width and fig.height are now `3.5` rather than `4`. This appears to remove some visual artefacts and makes the axis labels more legible.

* The "alpha" option has been removed for all directional analysis functions except `polarMap()` as it only ever worked for `polarMap()`.

* All documentation has been improved; function parameters are more consistent between functions and arguments passed to `openair` via `...` are now explicitly listed.

## Network Visualisation

* `networkMap()` now supports multiple sources. For example, using `source = c("aurn", "saqn")` will show both the AURN and SAQN on one map. This may be useful if users are interested in air quality in a specific region of the UK (e.g., users may wish to locate AURN, AQE *and* locally managed sites near to a given urban centre).

* `networkMap()` now supports `source = "local"`.

* Multiple basemap providers can now be used with `networkMap()`.

* `networkMap()` has gained the `collapse.control` argument.

## Trajectory Analysis

* `trajMap()` has gained the `collapse.control` argument.



# openairmaps 0.5.1

* FIX: fixed an issue causing markers to be duplicated when pollutant information is missing for certain sites.



# openairmaps 0.5.0 (2022-10-19)

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
