
## SCRIPT TO IMPORT "polar_data"

# find meta data
meta <- openair::importMeta(source = "aurn")

site_meta <-
  dplyr::filter(
    meta,
    grepl(
      pattern = "Marylebone|Kensington|Bloom|Cromwell Road 2",
      meta$site
    )
  )

# import aq data
raw <- openair::importAURN(site = site_meta$code, year = 2009)

raw <- dplyr::select(raw, date, nox, no2, pm2.5, pm10, site)

raw <- dplyr::left_join(raw, dplyr::select(meta, -code), by = "site")

# import met data
met <- worldmet::importNOAA("037720-99999", year = 2009)

met <- dplyr::select(met, date, wd, ws, visibility, air_temp)

# join
polar_data <- dplyr::left_join(raw, met, by = "date")

# rename
polar_data <- dplyr::rename(polar_data, lat = latitude, lon = longitude)

# "save"
usethis::use_data(polar_data, overwrite = TRUE)
