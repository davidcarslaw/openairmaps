# openairmaps (development version)

## Breaking changes

* BREAKING: The `polarMapStatic()` family is now powered by `{ggspatial}` rather than `{ggmap}` as it does not require an API key. This means the `ggmap` argument has been removed and the `provider` argument has been added. Other benefits of this switch include a greater number of available base maps (see: `rosm::osm.types()`) and the ability to simply change the extent of the map axes using `ggplot2::coord_sf()`.

* BREAKING: The `control` and `facet` arguments have been deprecated in favour of `type` in the `polarMap()` family. These arguments will eventually be removed, but as of this version of `{openairmaps}` users will be warned away from their use.

## New features

* The `polarMapStatic()` family of functions have been combined with the `polarMap()` family, with static maps available to be accessed using the `static` argument. The `polarMapStatic()` family are therefore deprecated, and will later be removed from `{openairmaps}`. The justification for this is as follows:

    * The combined functions allows for a more simple, consistent API for users (e.g., avoiding needing to switch between `facet` and `control`).
    
    * The use of the `static` argument allows for simple switching between dynamic and static maps. For example, a researcher may wish to use the dynamic maps for data exploration, but then switch to a static map for placement into a PDF report.

    * Recent developments have meant that the arguments and capability of these functions have started to align regardless (e.g., `provider`, `crs`).

    * Combining these functions has reduced repetition in the source code of `{openairmaps}`, reducing the likelihood of oversights and bugs, and allowing for more rapid development.

* The `crs` argument has been added to the `polarMap()` and `polarMapStatic()` families and to `searchNetwork()`. This argument allows for users to specify that their data is using an alternative coordinate system to the standard longitude/latitude (e.g., the British National Grid CRS). Alternate CRS will be re-projected to longitude/latitude for plotting as this is expected by `{leaflet}` / `{ggspatial}`.

* Popups for the dynamic `polarMap()` family will now be near the top of the plot rather than the centre. This will obscure less of the plot itself while the marker is visible.

* Two examples of the use of `{openairmaps}` with `{shiny}` have been added to the package. Run `shiny::runExample(package = "openairmaps")` to view these.

## Bug fixes

* Legends drawn by the `polarMapStatic()` function should now render using more recent versions of `{ggplot2}`.

* The `addTrajPaths()` `layerId` argument is now implemented in a sensible way to ensure each geometry has a unique `layerId` and can therefore be interacted with in a `{shiny}` context. 

    * `layerId` is now the base on which the actual layerId is built, with each real layerId in the form BASE-LN-PN where LN is the line number and PN is the point number. For example, if `layerId = "traj"`, the first point of the first line has the ID `"traj-1-1"`, the second point of the first line has ID `"traj-1-2"`, the first point of the second line has ID `"traj-2-1"`, and so on.

# openairmaps 0.8.1

This is a minor release of `{openairmaps}`, released mainly to fix an issue with `{ggmap}` but also adding some new functionality for polar marker maps.

## Breaking changes

* BREAKING: The arguments of `addPolarMarkers()` have been rejigged to move "data" after "pollutant", owing to the new use of `leaflet::getMapData()`. (#45)

* BREAKING: The default arguments of some `polarMap()`-family functions have changed from, e.g., `NULL` to `"free"` or `"fixed"`. (#34)

* BREAKING: Due to changes in `{ggmap}`, all static polar plotting functions now require users to provide their own `ggmap` object. The `zoom` argument has also been removed. This is specifically related to the partnership of Stamen and Stadia which has put the stamen tiles behind an API. See <https://maps.stamen.com/stadia-partnership/> and <https://github.com/dkahle/ggmap/issues/353> for more information. (#52)

## New features

* Several "limit" arguments can now take one of three options: "fixed" (which forces all markers to share scales), "free" (which allows them to use different scales), or a numeric vector to define the scales. (#34) These arguments and their defaults include:

  * `polarMap()`: `upper` (fixed); `limits` (free)
  * `annulusMap()`: `limits` (free)
  * `freqMap()`: `breaks` (free)
  * `percentileMap()`: `intervals` (fixed)

* Added `searchNetwork()`, which allows users to find local air quality monitoring sites by specifying a target latitude and longitude. Function arguments allow the site metadata to be subset (for example, by site type, pollutants measured, or distance from the target).

* Added `convertPostcode()`, which converts a valid UK postcode to a latitude/longitude pair. This is intended to be used with `searchNetwork()`.

* The "data" argument of `addPolarMarkers()` and `addTrajPaths()` and both the "before" and "after" arguments of `addPolarDiffMarkers()` now default to `leaflet::getMapData(map)`. This makes their use less verbose when creating multiple polar plots with the same underlying data, which will likely be a common use-case. (#45)

* `networkMap()` popups now contain links to the associated network websites. For example, the popup for London Marylebone Road in `networkMap("aurn")` now contains a link to <https://uk-air.defra.gov.uk/networks/site-info?site_id=MY1>. All networks are supported with the exception of "europe". (#39)

* `addPolarMarkers()` and `addPolarDiffMarkers()` now have all of the "options" arguments of `leaflet::addMarkers()`. This means that, for example, polar markers can be clustered (<https://leafletjs.com/reference.html#marker>). (#38)

* The `polarMap()` family and `networkMap()` `provider` argument can now take a named vector. The names will be used in the layer control menu, if `length(provider) > 1`. (#42)

# openairmaps 0.8.0

This is a minor release adding a range of quality of life features, adding two new experimental functions, and fixing a few bugs.

## New features

* `trajMapStatic()` and `trajLevelMapStatic()` have been added as two new *experimental* functions to provide `{ggplot2}` equivalents of `openair::trajPlot()` and `openair::trajLevel()`. (#28)

  * These are experimental as the long term place for these functions is uncertain; there will definitely be need for a `{ggplot2}` incarnation of the trajectory plotting functions, but whether they will sit in `{openair}`, `{ggopenair}` or `{openairmaps}` and what they will be named is not clear.

* The `control` and `facet` arguments of all polar marker mapping functions (static and interactive) and trajectory mapping functions are now passed to `openair::cutData()`.

* The `popup` argument of all interactive polar marker mapping functions can now take a vector of column names. If more than one column is provided, it is automatically passed to `buildPopup()` using its default values.

* `trajLevelMap()` now has the `control` argument, which maps directly onto the `type` argument of `openair::trajLevel()`. Like other `control` arguments elsewhere in `{openairmaps}`, this creates a "layer control" menu.

* `networkMap()` now uses different coloured markers for different networks. If more than one network is specified, and `draw.legend` is set to `TRUE`, a legend will also be drawn for quick identification of different data sources. (#30)

* Deprecations are now managed by the `{lifecycle}` package. This currently only applies to the `type` argument.

## Bug fixes

* Fixed issues where multiple `addPolarMarkers()` chained together would all show the same plot.

* Fixed issue where `...` and `pollutant` weren't being passed to `addPolarMarkers()`. (#27)

* Fixed issue in `trajMap()` caused by recent updates to `{dplyr}` and `{forcats}`.

* Fixed issue where `polarMapStatic()` and others would turn factor facet levels into characters. 

  * Specifically, this meant that, for example, months of the year would be in alphabetical order. Now factor levels, including those resulting from a pass to `cutData()`, will now be honoured by the `facet` argument. (#31)

* Fixed issue where `polarMapStatic()` and others would error when trying to draw a legend.

# openairmaps 0.7.0

This is a minor release containing several important new features that expand the scope of the package. This also comes with several minor breaking changes to improve consistency within `{openairmaps}` and with `{openair}`.

## Breaking changes

* BREAKING: The `fig.width`, `fig.height`, `iconHeight` and `iconWidth` arguments have been replaced with `d.fig` and `d.icon`. There are two main justifications behind this:

  * This ensures consistency across all of `{openairmaps}`, making it easier to switch between the static and HTML map types.
  
  * Polar markers are almost always going to be circular (i.e., width = height) so  having one argument will streamline things. If users wish to have non-circular markers, a vector of length two in the form `c(width, height)` will provide the same functionality.

* BREAKING: The arguments in `addPolarMarkers()` have been put in a more sensible order, leading with `data`, `pollutant` and `fun`.

* BREAKING: The `date` argument from `networkMap()` has been replaced by `year`. (#26)

## New features

* Added "static" equivalents of all of the polar marker maps written in `{ggplot2}`. While interactive HTML maps are preferred, the static equivalents may be more appropriate for, e.g., academic publications. (#19)

  * The `{ggplot2}` functions can be identified by "Static" being appended to the function name. For example, `polarMap()` is the `{leaflet}` polar plot map, whereas `polarMapStatic()` is the `{ggplot2}` equivalent.
  
  * Currently, "static" versions of the trajectory maps are served by `openair::trajPlot()` and `openair::trajLevel()`, but there may be space in future to have `ggmap` equivalents of these in `openairmaps`.

* Added `diffMap()` and `diffMapStatic()` which are to `openair::polarDiff()` what `polarMap()` and `polarMapStatic()` are to `openair::polarPlot()` (#17). Also added `addPolarDiffMarkers()`, which is the equivalent of `addPolarMarkers()`. (#25)

* Added `alpha` as an argument to all of the directional analysis polar mapping functions, not just `polarMap()`. (#14)

* Fixed `alpha` to work on both Windows and MacOS by forcing the use of the "cairo" device to save plots. (#14)

* Polar marker maps and `addPolarMarkers()` now show a progress bar when creating the markers takes more than a few seconds (most commonly in `polarMap()` and `annulusMap()`, particularly with multiple pollutants/control groups).

* `networkMap()` can now pass the new `year` option to `importMeta()`.



# openairmaps 0.6.1

This is a patch release primarily to fix a few bugs in `{openairmaps}`, and implement a new default colour scheme after a recent `{openair}` update.

## New features

* Functions now use Google's "turbo" colour palette rather than "jet" by default. More about this palette and the advantages of using it over "jet" can be read at <https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html>.

## Bug fixes

* Fixed issue with polar marker maps (e.g., `polarMap()`) and the generic `addPolarMarkers()` function where lat/lon info in the Southern Hemisphere would misalign markers. Hat tip to Deanna Tuxford and James for noticing this issue. (#18)

* Fixed an issue with `networkMap()` where `control = "variable"` would fail to show all pollutants.



# openairmaps 0.6.0

This is a minor release, mainly focusing on enhancing the ability for polar markers to have shared colour scales, but also incorporating new features for network visualisation.

## New features

* All directional analysis maps can now have their limits provided (can be "limits", "breaks", "percentiles", etc., depending on function). This was always possible through `...`, but it is now explicitly listed as an option. (#12)

* If limits are defined in a directional analysis function, a shared legend will now be drawn at the top-right of the map. This functionality can be disabled by setting `draw.legend` to FALSE. (#12)

* Added the `buildPopup()` function, which allows users to easily construct HTML popups for use with the "popup" argument of directional analysis maps (or `leaflet` maps more widely).

* The default options for fig.width and fig.height are now `3.5` rather than `4`. This appears to remove some visual artefacts and makes the axis labels more legible.

* `networkMap()` now supports multiple sources. For example, using `source = c("aurn", "saqn")` will show both the AURN and SAQN on one map. This may be useful if users are interested in air quality in a specific region of the UK (e.g., users may wish to locate AURN, AQE *and* locally managed sites near to a given urban centre). (#16)

* `networkMap()` now supports `source = "local"`.

* Multiple basemap providers can now be used with `networkMap()`.

* `networkMap()`, `trajMap()` and all polar directional analysis maps have gained the `collapse.control` argument, which controls whether the control menu starts collapsed or not. It defaults to `FALSE`, which means the control menu is not collapsed.

* All documentation has been improved; function parameters are more consistent between functions and arguments passed to `openair` via `...` are now explicitly listed.

## Bug fixes

* The "alpha" option has been removed for all directional analysis functions except `polarMap()` as it only ever worked for `polarMap()`.



# openairmaps 0.5.1

This is a patch release designed to fix a major bug in v0.5.0.

## Bug fixes

* Fixed an issue causing markers to be duplicated when pollutant information is missing for certain sites.



# openairmaps 0.5.0

This is a minor release centred around the addition of the `control` argument, which allows for arbitrary columns to be used in "layer control" menus.

## Breaking changes

* All functions now use latitude and longitude to distinguish between site types. Therefore, "type" is now deprecated. Maps using the old system will still render, but popups will not be displayed. For most users, to restore previous site labels simply rewrite `type = "site"` as `popup = "site"`. (#10)

* The default values for "pollutant" have all been removed. Any users relying on this default should update their code to explicitly state `pollutant = "nox"`.

## New features

* All functions now possess the "control" argument, which allows users to create a "layer control" menu with any arbitrary column. Appropriate columns may be those produced using `openair::cutData()`, `openair::splitByDate()`, or a user-defined `dplyr::case_when()`/`dplyr::if_else()` column transformation. (#9)

* All functions now possess the "popup" and "label" arguments, which control pop-up and hover-over labels, respectively. This allows users to define *any* popup or label column, even non-unique ones. For example, multiple sites can be labelled with identical site types. (#10)

* All functions now try to guess the latitude/longitude column if not provided, similar to `{leaflet}`. (#10)

* Updated many error messages and warnings to use `{cli}` and be broadly more descriptive.

## Bug fixes

* `trajMap()` can now be coloured by date.

* Fixed issue with `trajMap()` that would cause user-defined colours not to work.



# openairmaps 0.4.3

This is a patch release adding a small number of refinements.

## Breaking changes

* `polar_data` column names changed from "latitude" to "longitude" to "lat" and "lon" to reflect the defaults of the `polarMap()` family.

* `trajMap()` and `trajLevelMap()` now use the argument names "latitude" and "longitude" to match those of the `polarMap()` family.

## New features

* `trajLevelMap()` now contains the `lat.inc` and `lon.inc` arguments.



# openairmaps 0.4.2

This is a patch release to fix a bug with `trajLevelMap()`.

## Bug fixes

* `trajLevelMap()` now works where `statistic = "frequency"` without a "pollutant".



# openairmaps 0.4.1

This is the first CRAN release of `{openairmaps}`.

## Features

* There are currently three streams of functionality in `openairmaps`:

  * `networkMap()` visualises any of the `openair::importMeta()` networks.

  * `polarMap()` and family allow for any of the `openair` directional analysis plots to be used as leaflet markers. 

  * `trajMap()` and family are leaflet equivalents to `openair::trajPlot()` and `openair::trajMap()`.

* There are two main classes of functions:

  * `*Map()` functions are easy-to-use functions which create leaflet maps from the ground-up. These are the most similar to `openair` functions.
  
  * `add*()` functions are more flexible and allow users to add layers to existing leaflet maps. These are designed to be similar to the `leaflet` "add" functions like `addMarkers()`.
