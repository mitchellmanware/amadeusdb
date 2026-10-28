# amadeusdb <a><img src="man/figures/amadeusdb.svg" align="right" width="250" alt="amadeus website" /></a>

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/mitchellmanware/amadeusdb/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/mitchellmanware/amadeusdb/actions/workflows/check-standard.yaml)
[![lint](https://github.com/mitchellmanware/amadeusdb/actions/workflows/lint.yaml/badge.svg)](https://github.com/mitchellmanware/amadeusdb/actions/workflows/lint.yaml)
[![cov](https://mitchellmanware.github.io/amadeusdb/badges/coverage.svg)](https://github.com/mitchellmanware/amadeusdb/actions)

`amadeusdb` extends [amadeus](https://niehs.github.io/amadeus/) with a
`sedonadb` backend for processing environmental data, extracting covariates,
and joining results. Outputs are written to Parquet. The package is under
development, and its API may change.

## Tier 2a: Process and Calculate Covariates

The `sedona_*` functions process local environmental data downloaded with
`amadeus` and calculate covariates for specified locations and dates using
`sedonadb`. Currently, `sedona_geos()` extracts NASA GEOS-CF raster values at
points or summarizes them within polygons, while `sedona_hms()` calculates
daily NOAA HMS smoke indicators. Locations are transformed and optionally
buffered, and results are written to Parquet files in batches.

### Tier 2a.1: Join Variables

`join_variables()` combines separate variable tables from one data source
into a wider Parquet dataset using shared location and time keys. Full outer
joins retain all keys present in any input table, leaving unmatched values
missing. Timestamp keys must match exactly.

## Tier 3a: Join Covariates

`join_covariates()` combines covariates from multiple data sources with an
observation table using left joins that preserve every observation row.
Hourly covariates match by location and calendar hour, daily covariates by
location and date, and static covariates by location alone. Temporal keys
must already represent UTC. The combined dataset is written to Parquet,
with unmatched covariates left missing.

## Example
The following example demonstrates how hourly (NASA GEOS CF) and daily (NOAA HMS Smoke) covariates 1) can be calculated with the `sedonadb`-backeded functions, 2) can be joined within a data source (if needed), and 3) joined between data sources to a common left table (target is U.S. EPA Air Quality System (AQS) sites). Function messages have been omitted for brevity.

```r
# ------------------------------------------------------------------------------
# Directories.
chr_tempdir <- tempdir()
chr_hmsdir <- file.path(chr_tempdir, "hms")
chr_geosdir <- file.path(chr_tempdir, "geos")

# ------------------------------------------------------------------------------
# Date of interest.
chr_dates <- "2018-01-01"

# ------------------------------------------------------------------------------
# Import fixture points.
polygons <- sf::st_read(file.path(
  "tests",
  "testdata",
  "locs",
  "polygons.shp"
))

# ------------------------------------------------------------------------------
# Define GEOS assets (directories stored) and variables.
df_geos_input <- data.frame(
  asset = c("aqc_tavg", "chm_tavg"),
  variable = c("O3", "CO")
)
# ------------------------------------------------------------------------------
# Split the data.frame into a list.
list_geos_input <- split(df_geos_input, seq_len(nrow(df_geos_input)))

# ------------------------------------------------------------------------------
# Covariates with `sedona_geos` (all variables in the list).
list_geos_sptmpl <- lapply(
  list_geos_input,
  function(x) {
    amadeusdb::sedona_geos(
      date = chr_date,
      input = file.path("tests", "testdata", "geos", x$asset),
      output = chr_geosdir,
      variable = x$variable,
      locs = polygons,
      locs_id = "NAME",
      radius = 0L
    )
  }
)

# ------------------------------------------------------------------------------
# Join GEOS variables into single .parquet database.
db_geos <- amadeusdb::join_variables(
  input = list_geos_sptmpl,
  output = chr_geosdir,
  by = c("NAME", "time")
)

# ------------------------------------------------------------------------------
# Covariates with `sedona_hms``.
db_hms <- amadeusdb::sedona_hms(
  date = chr_dates,
  input = file.path("tests", "testdata", "hms"),
  output = chr_hmsdir,
  locs = polygons,
  locs_id = "NAME",
  radius = 0
)

# ------------------------------------------------------------------------------
# Join GEOS and HMS data to database with independent variable (sim AQS).
db_join <- join_covariates(
  input = file.path("tests", "testdata", "locs", "db_hourly.parquet"),
  output = chr_tempdir,
  hourly = list(geos = db_geos),
  daily = list(hms = db_hms),
  static = NULL,
  locs_id = "NAME",
  prefix = FALSE
)

# ------------------------------------------------------------------------------
# Collect results in to R session.
sedonadb::sd_collect(sedonadb::sd_read_parquet(db_join))
```
```
      NAME                time       lon      lat   outcome           O3
1 Alamance 2018-01-01 00:30:00 -79.39793 36.03757 0.3436570 3.055902e-08
2 Alamance 2018-01-01 01:30:00 -79.39793 36.03757 0.8346653 2.895831e-08
3  Chatham 2018-01-01 00:30:00 -79.25672 35.69645 0.3777033 3.131572e-08
4  Chatham 2018-01-01 01:30:00 -79.25672 35.69645 0.9145516 2.991874e-08
5   Durham 2018-01-01 00:30:00 -78.87809 36.02862 0.1266433 2.828892e-08
6   Durham 2018-01-01 01:30:00 -78.87809 36.02862 0.1558236 2.759043e-08
7   Orange 2018-01-01 00:30:00 -79.12178 36.05355 0.8884222 2.828892e-08
8   Orange 2018-01-01 01:30:00 -79.12178 36.05355 0.1655643 2.759043e-08
            CO light medium heavy
1 1.642656e-07     0      0     0
2 1.717625e-07     0      0     0
3 1.602881e-07     0      0     0
4 1.668730e-07     0      0     0
5 1.760818e-07     0      0     0
6 1.799115e-07     0      0     0
7 1.689223e-07     0      0     0
8 1.749639e-07     0      0     0
```

The table below lists datas sources with `amadeus` download functions and their associated `sedona_*` processing and extraction implementation in `amadeusdb`.

| Dataset | `amadeusdb` |
| :--- | :--- |
| NASA GEOS-CF | `sedona_geos()` |
| NOAA HMS smoke | `sedona_hms()` |
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
| NASA MERRA-2 | Not yet supported |
| NASA MODIS (including MCD14ML fire detections) | Not yet supported |
| NASA SEDAC Global Roads Open Access Data Set (gROADS) | Not yet supported |
| NASA SEDAC population density | Not yet supported |
| National Land Cover Database (NLCD) | Not yet supported |
| NOAA GOES Aerosol Detection Product (ADP) | Not yet supported |
| NOAA North American Regional Reanalysis (NARR) | Not yet supported |
| PRISM climate data | Not yet supported |
| USDA CropScape Cropland Data Layer (CDL) | Not yet supported |
| USGS Global Multi-resolution Terrain Elevation Data (GMTED2010) | Not yet supported |
| USGS Hydrologic Unit Codes (HUC) | Not yet supported |
