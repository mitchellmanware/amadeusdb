################################################################################
# Manual script for creating the central Coordinate Reference System (CRS)
# data list for {amadeusdb}-supported data sources and products.

################################################################################
# Set working directory, assuming amadeusdb/ is the starting point.
setwd("inst/extdata")

################################################################################
# Load libraries.
library(sf)
library(terra)

################################################################################
####################                  GEOS                  ####################
chr_geos <- paste0(
  "crs/geos/GEOS-CF.v01.rpl.aqc_tavg_1hr_g1440x721_v1.20240601_0030z.nc4"
)
rast_geos <- terra::rast(chr_geos)
crs_geos <- sf::st_crs(terra::crs(rast_geos))

####################                  HMS                   ####################
chr_hms <- "crs/hms/hms_smoke20190101.shp"
sf_hms <- sf::st_read(chr_hms)
crs_hms <- sf::st_crs(sf_hms)

################################################################################
# Merge CRS data into list.
list_crs <- list(
  geos = list(
    source = "geos",
    collection = c(
      "aqc_tavg_1hr_g1440x721_v1",
      "chm_tavg_1hr_g1440x721_v1",
      "met_tavg_1hr_g1440x721_x1",
      "xgc_tavg_1hr_g1440x721_x1",
      "chm_inst_1hr_g1440x721_p23",
      "met_inst_1hr_g1440x721_p23"
    ),
    crs = crs_geos,
    reference = "",
    notes = "Set based on manual inspection of raw .nc4 file."
  ),
  hms = list(
    source = "hms",
    collection = "",
    crs = crs_hms,
    reference = "",
    notes = "Set based on manual inspection of raw .shp file."
  )
)

################################################################################
# Save as base R .rds object.
saveRDS(list_crs, "list_crs.rds")
