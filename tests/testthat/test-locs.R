################################################################################
# Unit and integration tests for the join_* functions and auxiliaries.

################################################################################
library(testthat)

################################################################################
# Success cases.
################################################################################
testthat::test_that("prepare_sf; radius > 0", {
  # ----------------------------------------------------------------------------
  # Import fixture points.
  points <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "points.shp"
  ))
  # ----------------------------------------------------------------------------
  # Returns `sf` when supplied 0 row vector.
  testthat::expect_no_error(
    sf_locs <- amadeusdb::prepare_sf(
      points,
      crs = sf::st_crs("EPSG:4326"),
      radius = 100
    )
  )
  # ----------------------------------------------------------------------------
  # Expect `sf` object.
  testthat::expect_s3_class(sf_locs, "sf")
  # ----------------------------------------------------------------------------
  # Expect POINT have become POLYGON geometries.
  testthat::expect_true(all(sf::st_geometry_type(sf_locs) == "POLYGON"))
})

################################################################################
testthat::test_that("prepare_sf; nrow(locs) = 0", {
  # ----------------------------------------------------------------------------
  # Initiate data.frame with data to exclude.
  df_empty <- data.frame(site_id = "A001", lon = -79.33, lat = 36.07)
  # ----------------------------------------------------------------------------
  # Returns `sf` when supplied 0 row vector.
  testthat::expect_warning(
    testthat::expect_message(
      sf_locs <- amadeusdb::prepare_sf(
        terra::vect(df_empty)[0, ],
        crs = sf::st_crs("EPSG:4326"),
        radius = 0
      ),
      regexp = "no rows"
    )
  )
  # ----------------------------------------------------------------------------
  # Expect `sf` object.
  testthat::expect_s3_class(sf_locs, "sf")
})

################################################################################
# Failure cases.
################################################################################
testthat::test_that("prepare_sf; invalid locs", {
  # ----------------------------------------------------------------------------
  # Text locations.
  testthat::expect_error(
    amadeusdb::prepare_sf(
      locs = "text",
      crs = sf::st_crs("EPSG:4326"),
      radius = 0
    )
  )
  # ----------------------------------------------------------------------------
  # Numeric locations.
  testthat::expect_error(
    amadeusdb::prepare_sf(
      locs = 1e4,
      crs = sf::st_crs("EPSG:4326"),
      radius = 0
    )
  )
})
