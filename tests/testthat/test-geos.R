################################################################################
# Unit and integration tests for the sedona_geos function and auxiliaries.

################################################################################
library(testthat)

################################################################################
# Success cases.
################################################################################
testthat::test_that("sedona_geos; locs = sf::polygons; aqc", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_tempdir))
  # ----------------------------------------------------------------------------
  # Import fixture points.
  polygons <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "polygons.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime
  testthat::expect_no_error(
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "aqc"),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(db_geos1), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(db_geos1), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(db_geos1$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(db_geos1$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(db_geos1$O3), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_geos; locs = sf::points; aqc", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_tempdir))
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
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "aqc"),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME",
      radius = 0,
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(db_geos1), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(db_geos1), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(db_geos1$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(db_geos1$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(db_geos1$O3), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_geos; locs = terra::polygons; aqc", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_tempdir))
  # ----------------------------------------------------------------------------
  # Import fixture points.
  polygons <- terra::vect(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "polygons.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime
  testthat::expect_no_error(
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "aqc"),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(db_geos1), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(db_geos1), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(db_geos1$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(db_geos1$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(db_geos1$O3), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_geos; locs = terra::points; aqc", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_tempdir))
  # ----------------------------------------------------------------------------
  # Import fixture points.
  points <- terra::vect(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "points.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime
  testthat::expect_no_error(
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "aqc"),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME",
      radius = 0,
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(db_geos1), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(db_geos1), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(db_geos1$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(db_geos1$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(db_geos1$O3), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_geos; locs = data.frame; aqc", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_tempdir))
  # ----------------------------------------------------------------------------
  # Import data.frame points.
  df <- read.csv(testthat::test_path("..", "testdata", "locs", "df.csv"))
  # ----------------------------------------------------------------------------
  # No errors at runtime
  testthat::expect_no_error(
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "aqc"),
      output = chr_tempdir,
      locs = df,
      locs_id = "NAME",
      radius = 0,
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(db_geos1), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(db_geos1), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(db_geos1$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(db_geos1$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(db_geos1$O3), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_geos; locs = sf::polygons; chm", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_tempdir))
  # ----------------------------------------------------------------------------
  # Import fixture points.
  polygons <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "polygons.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime
  testthat::expect_no_error(
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "chm"),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(db_geos1), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(db_geos1), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(db_geos1$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(db_geos1$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(db_geos1$O3), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_geos; locs = sf::points; chm", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_tempdir))
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
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "chm"),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME",
      radius = 0,
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(db_geos1), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(db_geos1), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(db_geos1$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(db_geos1$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(db_geos1$O3), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_geos; locs = terra::polygons; chm", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_tempdir))
  # ----------------------------------------------------------------------------
  # Import fixture points.
  polygons <- terra::vect(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "polygons.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime
  testthat::expect_no_error(
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "chm"),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(db_geos1), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(db_geos1), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(db_geos1$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(db_geos1$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(db_geos1$O3), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_geos; locs = terra::points; chm", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_tempdir))
  # ----------------------------------------------------------------------------
  # Import fixture points.
  points <- terra::vect(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "points.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime
  testthat::expect_no_error(
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "chm"),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME",
      radius = 0,
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(db_geos1), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(db_geos1), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(db_geos1$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(db_geos1$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(db_geos1$O3), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_geos; locs = data.frame; chm", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_tempdir))
  # ----------------------------------------------------------------------------
  # Import data.frame points.
  df <- read.csv(testthat::test_path("..", "testdata", "locs", "df.csv"))
  # ----------------------------------------------------------------------------
  # No errors at runtime
  testthat::expect_no_error(
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "chm"),
      output = chr_tempdir,
      locs = df,
      locs_id = "NAME",
      radius = 0,
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output directory.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(db_geos1), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(db_geos1), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(db_geos1$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(db_geos1$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(db_geos1$O3), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_geos; skip existing .parquet", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_tempdir))
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
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "aqc"),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME",
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # Expect skips when re-run with same date and file requests.
  testthat::expect_message(
    amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "aqc"),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME",
      variable = "O3"
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
testthat::test_that("sedona_geos; expected failures", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  # ----------------------------------------------------------------------------
  # Import fixture points.
  points <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "points.shp"
  ))
  # ----------------------------------------------------------------------------
  # Improper locs arguments.
  testthat::expect_error(
    amadeusdb::sedona_geos(input = chr_tempdir, locs = "locs"),
    "locs"
  )
  testthat::expect_error(
    amadeusdb::sedona_geos(input = chr_tempdir, locs = data.frame(site_id = 1)),
    "locs"
  )
  # ----------------------------------------------------------------------------
  # Improper batch arguments.
  testthat::expect_error(
    amadeusdb::sedona_geos(locs = points, locs_id = "NAME", batch = NULL),
    regexp = "batch"
  )
  testthat::expect_error(
    amadeusdb::sedona_geos(locs = points, locs_id = "NAME", batch = 1.1),
    regexp = "batch"
  )
  # ----------------------------------------------------------------------------
  # Improper input arguments.
  testthat::expect_error(
    amadeusdb::sedona_geos(
      locs = points,
      locs_id = "NAME",
      batch = 1,
      input = NULL
    ),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::sedona_geos(
      locs = points,
      locs_id = "NAME",
      batch = 1,
      input = 10
    ),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::sedona_geos(
      locs = points,
      locs_id = "NAME",
      batch = 1,
      input = "path/to/empty/"
    ),
    regexp = "input"
  )
  # ----------------------------------------------------------------------------
  # Improper radius arguments.
  testthat::expect_error(
    amadeusdb::sedona_geos(
      locs = points,
      locs_id = "NAME",
      batch = 1,
      input = tempdir(),
      radius = -1
    ),
    regexp = "radius"
  )
  testthat::expect_error(
    amadeusdb::sedona_geos(
      locs = points,
      locs_id = "NAME",
      batch = 1,
      input = tempdir(),
      radius = NULL
    ),
    regexp = "radius"
  )
  testthat::expect_error(
    amadeusdb::sedona_geos(
      locs = points,
      locs_id = "NAME",
      batch = 1,
      input = tempdir(),
      radius = "text"
    ),
    regexp = "radius"
  )
  # ----------------------------------------------------------------------------
  # Improper variable arguments.
  testthat::expect_error(
    amadeusdb::sedona_geos(
      locs = points,
      locs_id = "NAME",
      batch = 1,
      input = tempdir(),
      radius = 0,
      variable = NULL
    ),
    regexp = "variable"
  )
  testthat::expect_error(
    amadeusdb::sedona_geos(
      locs = points,
      locs_id = "NAME",
      batch = 1,
      input = tempdir(),
      radius = 0,
      variable = 1L
    ),
    regexp = "variable"
  )
  # ----------------------------------------------------------------------------
  # Improper level arguements.
  testthat::expect_error(
    amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "aqc"),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3",
      level = 2 # single level for current data
    )
  )
  testthat::expect_error(
    amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "chm"),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3",
      level = 2 # single level for current data
    )
  )
  testthat::expect_error(
    amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "aqc"),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3",
      level = 2.1
    )
  )
  testthat::expect_error(
    amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "aqc"),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3",
      level = "level"
    )
  )
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})
################################################################################
