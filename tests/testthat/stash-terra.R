################################################################################
# Unit and integration tests for sedona_* tests with terra::vect extraction
# points. Skipped for now (10/30) while debugging a {terra} and {sedonadb}
# interaction error which causes C stack overflow, but stashed for future
# reintegration.

################################################################################
testthat::test_that("sedona_hms; locs = terra::polygons", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  file.remove(chr_tempdir)
  # ----------------------------------------------------------------------------
  # Import fixture polygons.
  polygons <- terra::vect(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "polygons.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime.
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
  # Only .parquet files in the output database.
  testthat::expect_true(all(grepl("\\.parquet", db_hms)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_hms1 <- sedonadb::sd_read_parquet(db_hms))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_hms1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_hms <- sedonadb::sd_collect(db_hms1))
  # ----------------------------------------------------------------------------
  # Expect 12 rows (4 fixture sites x 3 days).
  testthat::expect_equal(nrow(df_hms), 12)
  # ----------------------------------------------------------------------------
  # Expect 5 columns (locs_id, time, light, mediuum, heavy)
  testthat::expect_equal(ncol(df_hms), 5)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_hms$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect Date "date" column.
  testthat::expect_s3_class(df_hms$date, "Date")
  # ----------------------------------------------------------------------------
  # Expect numeric (binary) for light, medium, and heavy columns.
  testthat::expect_equal(class(df_hms$light_00000), "numeric")
  testthat::expect_equal(class(df_hms$medium_00000), "numeric")
  testthat::expect_equal(class(df_hms$heavy_00000), "numeric")
  # ----------------------------------------------------------------------------
  # Expect light, medium, and heavy columns contain only 0 or 1.
  testthat::expect_true(all(
    unique(c(df_hms$light_00000, df_hms$medium_00000, df_hms$heavy_00000)) %in%
      c(0, 1)
  ))
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_hms; locs = terra::points", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  file.remove(chr_tempdir)
  # ----------------------------------------------------------------------------
  # Import fixture points.
  points <- terra::vect(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "points.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime.
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
  # Only .parquet files in the output database.
  testthat::expect_true(all(grepl("\\.parquet", db_hms)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_hms1 <- sedonadb::sd_read_parquet(db_hms))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_hms1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_hms <- sedonadb::sd_collect(db_hms1))
  # ----------------------------------------------------------------------------
  # Expect 12 rows (4 fixture sites x 3 days).
  testthat::expect_equal(nrow(df_hms), 12)
  # ----------------------------------------------------------------------------
  # Expect 5 columns (locs_id, time, light, mediuum, heavy)
  testthat::expect_equal(ncol(df_hms), 5)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_hms$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect Date "date" column.
  testthat::expect_s3_class(df_hms$date, "Date")
  # ----------------------------------------------------------------------------
  # Expect numeric (binary) for light, medium, and heavy columns.
  testthat::expect_equal(class(df_hms$light_00000), "numeric")
  testthat::expect_equal(class(df_hms$medium_00000), "numeric")
  testthat::expect_equal(class(df_hms$heavy_00000), "numeric")
  # ----------------------------------------------------------------------------
  # Expect light, medium, and heavy columns contain only 0 or 1.
  testthat::expect_true(all(
    unique(c(df_hms$light_00000, df_hms$medium_00000, df_hms$heavy_00000)) %in%
      c(0, 1)
  ))
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
  # No errors at runtime.
  testthat::expect_no_error(
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "aqc_tavg"),
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
  # Only .parquet files in the output database.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_geos <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(df_geos), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(df_geos), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_geos$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(df_geos$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(df_geos$O3), "numeric")
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
  # No errors at runtime.
  testthat::expect_no_error(
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "aqc_tavg"),
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
  # Only .parquet files in the output database.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_geos <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(df_geos), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(df_geos), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_geos$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(df_geos$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(df_geos$O3), "numeric")
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
  # No errors at runtime.
  testthat::expect_no_error(
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "chm_tavg"),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "CO"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output database.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_geos <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(df_geos), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(df_geos), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_geos$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(df_geos$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(df_geos$CO), "numeric")
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
  # No errors at runtime.
  testthat::expect_no_error(
    db_geos <- amadeusdb::sedona_geos(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path("..", "testdata", "geos", "chm_tavg"),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME",
      radius = 0,
      variable = "CO"
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output database.
  testthat::expect_true(all(grepl("\\.parquet", db_geos)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_geos1 <- sedonadb::sd_read_parquet(db_geos))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_geos1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_geos <- sedonadb::sd_collect(db_geos1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(df_geos), 8)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(df_geos), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_geos$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_type(df_geos$time, "character")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(df_geos$CO), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})
################################################################################
