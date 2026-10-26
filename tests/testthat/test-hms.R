################################################################################
# Unit and integration tests for the sedona_hms function and auxiliaries.

################################################################################
library(testthat)

################################################################################
# Success cases.
################################################################################
testthat::test_that("sedona_hms; locs = polygons", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Import fixture polygons.
  polygons <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "polygons.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime
  testthat::expect_no_error(
    db_hms <- amadeusdb::sedona_hms(
      date = c("2018-12-30", "2019-01-01"),
      input = testthat::test_path("..", "testdata", "hms"),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  chr_output <- list.files(chr_tempdir, full.names = TRUE)
  testthat::expect_true(all(grepl("\\.parquet", chr_output)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_hms1 <- sedonadb::sd_read_parquet(chr_output))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_hms1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(dt_hms1 <- sedonadb::sd_collect(db_hms1))
  # ----------------------------------------------------------------------------
  # Expect 12 rows (4 fixture sites x 3 days).
  testthat::expect_equal(nrow(dt_hms1), 12)
  # ----------------------------------------------------------------------------
  # Expect 5 columns (locs_id, time, light, mediuum, heavy)
  testthat::expect_equal(ncol(dt_hms1), 5)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(dt_hms1$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect Date "date" column.
  testthat::expect_s3_class(dt_hms1$date, "Date")
  # ----------------------------------------------------------------------------
  # Expect numeric (binary) for light, medium, and heavy columns.
  testthat::expect_equal(class(dt_hms1$light), "numeric")
  testthat::expect_equal(class(dt_hms1$medium), "numeric")
  testthat::expect_equal(class(dt_hms1$heavy), "numeric")
  # ----------------------------------------------------------------------------
  # Expect light, medium, and heavy columns contain only 0 or 1.
  testthat::expect_true(all(
    unique(c(dt_hms1$light, dt_hms1$medium, dt_hms1$heavy)) %in%
      c(0, 1)
  ))
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_hms; locs = points", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Import fixture points.
  points <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "points.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime
  testthat::expect_no_error(
    db_hms <- amadeusdb::sedona_hms(
      date = c("2018-12-30", "2019-01-01"),
      input = testthat::test_path("..", "testdata", "hms"),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  chr_output <- list.files(chr_tempdir, full.names = TRUE)
  testthat::expect_true(all(grepl("\\.parquet", chr_output)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_hms1 <- sedonadb::sd_read_parquet(chr_output))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_hms1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(dt_hms1 <- sedonadb::sd_collect(db_hms1))
  # ----------------------------------------------------------------------------
  # Expect 12 rows (4 fixture sites x 3 days).
  testthat::expect_equal(nrow(dt_hms1), 12)
  # ----------------------------------------------------------------------------
  # Expect 5 columns (locs_id, time, light, mediuum, heavy)
  testthat::expect_equal(ncol(dt_hms1), 5)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(dt_hms1$NAME, "character")
  # ----------------------------------------------------------------------------
  testthat::expect_s3_class(dt_hms1$date, "Date")
  # Expect Date "date" column.
  # ----------------------------------------------------------------------------
  # Expect numeric (binary) for light, medium, and heavy columns.
  testthat::expect_equal(class(dt_hms1$light), "numeric")
  testthat::expect_equal(class(dt_hms1$medium), "numeric")
  testthat::expect_equal(class(dt_hms1$heavy), "numeric")
  # ----------------------------------------------------------------------------
  # Expect light, medium, and heavy columns contain only 0 or 1.
  testthat::expect_true(all(
    unique(c(dt_hms1$light, dt_hms1$medium, dt_hms1$heavy)) %in%
      c(0, 1)
  ))
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_hms; empty .shp", {
  # ----------------------------------------------------------------------------
  # Some .shp files from NOAA HMS repository are empty, containing true no data.
  # sedona_hms should handle these without errors, as empty joins return 0
  # without complex preprocessing requried by amadeus::process_hms.
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Import fixture points.
  points <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "points.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime
  testthat::expect_no_error(
    db_hms <- amadeusdb::sedona_hms(
      date = c("2018-12-31", "2018-12-31"),
      input = testthat::test_path("..", "testdata", "hms"),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  chr_output <- list.files(chr_tempdir, full.names = TRUE)
  testthat::expect_true(all(grepl("\\.parquet", chr_output)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_hms1 <- sedonadb::sd_read_parquet(chr_output))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_hms1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(dt_hms1 <- sedonadb::sd_collect(db_hms1))
  # ----------------------------------------------------------------------------
  # Expect 4 rows (4 fixture sites x 1 days).
  testthat::expect_equal(nrow(dt_hms1), 4)
  # ----------------------------------------------------------------------------
  # Expect 5 columns (locs_id, time, light, mediuum, heavy)
  testthat::expect_equal(ncol(dt_hms1), 5)
  # ----------------------------------------------------------------------------
  # Expect all 0 values for missing data.
  testthat::expect_equal(
    unique(c(dt_hms1$light, dt_hms1$medium, dt_hms1$heavy)),
    0
  )
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_hms; skip existing .parquet", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Import fixture points.
  points <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "points.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime
  testthat::expect_no_error(
    db_hms <- amadeusdb::sedona_hms(
      date = c("2018-12-30", "2019-01-01"),
      input = testthat::test_path("..", "testdata", "hms"),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME"
    )
  )
  # ----------------------------------------------------------------------------
  # Expect skips when re-run with same date and file requests.
  testthat::expect_message(
    amadeusdb::sedona_hms(
      date = c("2018-12-30", "2019-01-01"),
      input = testthat::test_path("..", "testdata", "hms"),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME"
    ),
    regexp = "Skipping existing batch "
  )
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
# Failure cases.
################################################################################
testthat::test_that("sedona_hms; expected failures", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Improper batch arguments.
  testthat::expect_error(amadeusdb::sedona_hms(batch = NULL))
  testthat::expect_error(amadeusdb::sedona_hms(batch = 1.1))
  # ----------------------------------------------------------------------------
  # Improper input arguments.
  testthat::expect_error(amadeusdb::sedona_hms(input = NULL))
  testthat::expect_error(amadeusdb::sedona_hms(input = 10))
  testthat::expect_error(amadeusdb::sedona_hms(input = "path/to/empty"))
  # ----------------------------------------------------------------------------
  # Improper locs arguments.
  testthat::expect_error(
    amadeusdb::sedona_hms(input = chr_tempdir, locs = "locs")
  )
  testthat::expect_error(
    amadeusdb::sedona_hms(input = chr_tempdir, locs = data.frame(site_id = 1))
  )
  # ----------------------------------------------------------------------------
  # Improper radius arguments.
  testthat::expect_error(amadeusdb::sedona_hms(radius = -1))
  testthat::expect_error(amadeusdb::sedona_hms(radius = NULL))
  testthat::expect_error(amadeusdb::sedona_hms(radius = "text"))
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})
################################################################################
