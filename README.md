# amadeusdb <a><img src="man/figures/amadeusdb.svg" align="right" width="250" alt="amadeus website" /></a>

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/mitchellmanware/amadeusdb/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/mitchellmanware/amadeusdb/actions/workflows/check-standard.yaml)
[![lint](https://github.com/mitchellmanware/amadeusdb/actions/workflows/lint.yaml/badge.svg)](https://github.com/mitchellmanware/amadeusdb/actions/workflows/lint.yaml)

`amadeusdb` extends [amadeus](https://niehs.github.io/amadeus/) with a
`sedonadb` backend for processing environmental data, extracting covariates,
and joining results. Outputs are written to Parquet. The package is under
development, and its API may change.

Download raw data with `amadeus`. Before extraction, prepare `sf` locations with
the required coordinate reference system and buffers. Current functions include:

- `sedona_geos()`: extract GEOS-CF raster values at points or summarize them
  within polygons.
- `sedona_hms()`: calculate daily light, medium, and heavy HMS smoke indicators
  from intersections with smoke polygons.
- `join_variables()`: combine variable-specific Parquet tables from one source.
- `join_covariates()`: join hourly, daily, and static Parquet covariates onto
  an observation table using location and time keys.

The table below lists dataset families offered by `amadeus`. Support refers to
dataset-specific processing and extraction implemented in `amadeusdb`.

| Dataset | `amadeusdb` support |
| :--- | :--- |
| Climatology Lab gridMET | Not yet supported |
| Climatology Lab TerraClimate | Not yet supported |
| Drought indices and classifications (SPEI, EDDI, USDM) | Not yet supported |
| Emissions Database for Global Atmospheric Research (EDGAR) | Not yet supported |
| EPA Air Quality System (AQS) | Not yet supported |
| EPA Ecoregions | Not yet supported |
| EPA National Emissions Inventory (NEI) | Not yet supported |
| EPA Toxics Release Inventory (TRI) | Not yet supported |
| IMPROVE aerosol monitoring | Not yet supported |
| Köppen-Geiger climate classification | Not yet supported |
| NASA Black Marble (VIIRS nighttime lights) | Not yet supported |
| NASA GEOS-CF | `sedona_geos()` |
| NASA MERRA-2 | Not yet supported |
| NASA MODIS (including MCD14ML fire detections) | Not yet supported |
| NASA SEDAC Global Roads Open Access Data Set (gROADS) | Not yet supported |
| NASA SEDAC population density | Not yet supported |
| National Land Cover Database (NLCD) | Not yet supported |
| NOAA GOES Aerosol Detection Product (ADP) | Not yet supported |
| NOAA HMS smoke | `sedona_hms()` |
| NOAA North American Regional Reanalysis (NARR) | Not yet supported |
| PRISM climate data | Not yet supported |
| USDA CropScape Cropland Data Layer (CDL) | Not yet supported |
| USGS Global Multi-resolution Terrain Elevation Data (GMTED2010) | Not yet supported |
| USGS Hydrologic Unit Codes (HUC) | Not yet supported |

Prepared Parquet tables from other sources, including AQS observations, can be
used with the join functions when they satisfy the documented column and key
requirements. This does not imply support for processing those sources' raw
files.
