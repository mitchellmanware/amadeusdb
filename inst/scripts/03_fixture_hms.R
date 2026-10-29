################################################################################
# Manual script for creating HMS fixture data for {amadeus} alignment tests.
# Mitchell Manware
# 29 October 2026

# ------------------------------------------------------------------------------
# Load libraries.
library(amadeus)

# ------------------------------------------------------------------------------
# Define common arguments.
chr_dates <- "2023-06-07"
chr_hmspath <- testthat::test_path("..", "testdata", "hms")
chr_locsid <- "NAME"

# ------------------------------------------------------------------------------
# {amadeus} calculation: polygons.
polygons <- sf::st_read(testthat::test_path(
  "..",
  "testdata",
  "locs",
  "polygons.shp"
))
int_radius <- 0
df_procalc_polygons <- suppressWarnings(amadeus::calculate_hms(
  from = amadeus::process_hms(
    date = chr_dates,
    path = chr_hmspath
  ),
  locs = polygons,
  locs_id = chr_locsid,
  radius = int_radius,
  geom = FALSE
))
saveRDS(
  df_procalc_polygons,
  testthat::test_path(
    "..",
    "testdata",
    "amadeus",
    "hms",
    "df_procalc_polygons.rds"
  )
)

# ------------------------------------------------------------------------------
# {amadeus} calculation: points.
points <- sf::st_read(testthat::test_path(
  "..",
  "testdata",
  "locs",
  "points.shp"
))
int_radius <- 0
df_procalc_points <- suppressWarnings(amadeus::calculate_hms(
  from = amadeus::process_hms(
    date = chr_dates,
    path = chr_hmspath
  ),
  locs = points,
  locs_id = chr_locsid,
  radius = int_radius,
  geom = FALSE
))
saveRDS(
  df_procalc_points,
  testthat::test_path(
    "..",
    "testdata",
    "amadeus",
    "hms",
    "df_procalc_points.rds"
  )
)

# ------------------------------------------------------------------------------
# {amadeus} calculation: points with 1e6 radius.
points <- sf::st_read(testthat::test_path(
  "..",
  "testdata",
  "locs",
  "points.shp"
))
int_radius <- 1e6
df_procalc_radius1e6 <- suppressWarnings(amadeus::calculate_hms(
  from = amadeus::process_hms(
    date = chr_dates,
    path = chr_hmspath
  ),
  locs = points,
  locs_id = chr_locsid,
  radius = int_radius,
  geom = FALSE
))
saveRDS(
  df_procalc_radius1e6,
  testthat::test_path(
    "..",
    "testdata",
    "amadeus",
    "hms",
    "df_procalc_radius1e6.rds"
  )
)
