################################################################################
# Unit and integration tests for the sedona_hms function and auxiliaries.

################################################################################
library(testthat)

################################################################################
# Success cases.
################################################################################
testthat::test_that("sedona_hms; locs = sf::polygons", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  file.remove(chr_tempdir)
  # ----------------------------------------------------------------------------
  # Import fixture polygons.
  polygons <- sf::st_read(testthat::test_path(
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
testthat::test_that("sedona_hms; locs = sf::points", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  file.remove(chr_tempdir)
  # ----------------------------------------------------------------------------
  # Import fixture points.
  points <- sf::st_read(testthat::test_path(
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
testthat::test_that("sedona_hms; locs = data.frame", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  file.remove(chr_tempdir)
  # ----------------------------------------------------------------------------
  # Import data.frame points.
  df <- read.csv(testthat::test_path("..", "testdata", "locs", "df.csv"))
  # ----------------------------------------------------------------------------
  # No errors at runtime.
  testthat::expect_no_error(
    db_hms <- amadeusdb::sedona_hms(
      date = c("2018-12-30", "2019-01-01"),
      input = testthat::test_path("..", "testdata", "hms"),
      output = chr_tempdir,
      locs = df,
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
testthat::test_that("sedona_hms; empty .shp", {
  # ----------------------------------------------------------------------------
  # Some .shp files from NOAA HMS repository are empty, containing true no data.
  # sedona_hms should handle these without errors, as empty joins return 0
  # without complex preprocessing requried by amadeus::process_hms.
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  file.remove(chr_tempdir)
  # ----------------------------------------------------------------------------
  # Import fixture points.
  points <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "points.shp"
  ))
  # ----------------------------------------------------------------------------
  # No errors at runtime.
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
  # Expect 4 rows (4 fixture sites x 1 days).
  testthat::expect_equal(nrow(df_hms), 4)
  # ----------------------------------------------------------------------------
  # Expect 5 columns (locs_id, time, light, mediuum, heavy)
  testthat::expect_equal(ncol(df_hms), 5)
  # ----------------------------------------------------------------------------
  # Expect all 0 values for missing data.
  testthat::expect_equal(
    unique(c(df_hms$light_00000, df_hms$medium_00000, df_hms$heavy_00000)),
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
  # Remove temporary directory to force recreation.
  file.remove(chr_tempdir)
  # ----------------------------------------------------------------------------
  # Import fixture points.
  points <- sf::st_read(testthat::test_path(
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
    amadeusdb::sedona_hms(input = chr_tempdir, locs = "locs"),
    "locs"
  )
  testthat::expect_error(
    amadeusdb::sedona_hms(input = chr_tempdir, locs = data.frame(site_id = 1)),
    "locs"
  )
  # ----------------------------------------------------------------------------
  # Improper radius arguments.
  testthat::expect_error(
    amadeusdb::sedona_hms(
      locs = points,
      locs_id = "NAME",
      radius = -1
    ),
    regexp = "radius"
  )
  testthat::expect_error(
    amadeusdb::sedona_hms(
      locs = points,
      locs_id = "NAME",
      radius = NULL
    ),
    regexp = "radius"
  )
  testthat::expect_error(
    amadeusdb::sedona_hms(
      locs = points,
      locs_id = "NAME",
      radius = "text"
    ),
    regexp = "radius"
  )
  # ----------------------------------------------------------------------------
  # Improper date arguments.
  testthat::expect_error(
    amadeusdb::sedona_hms(locs = points, locs_id = "NAME", date = NULL),
    regexp = "date"
  )
  testthat::expect_error(
    amadeusdb::sedona_hms(locs = points, locs_id = "NAME", date = 1L),
    regexp = "date"
  )
  # ----------------------------------------------------------------------------
  # Improper input arguments.
  testthat::expect_error(
    amadeusdb::sedona_hms(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = NULL
    ),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::sedona_hms(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = 10
    ),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::sedona_hms(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = "path/to/empty/"
    ),
    regexp = "input"
  )
  # ----------------------------------------------------------------------------
  # Create fixture files in chr_tempdir.
  file.create(file.path(chr_tempdir, "/fixturepath.txt"))
  # ----------------------------------------------------------------------------
  # Improper output arguments.
  testthat::expect_error(
    amadeusdb::sedona_hms(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = chr_tempdir,
      output = NULL
    ),
    regexp = "output"
  )
  testthat::expect_error(
    amadeusdb::sedona_hms(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = chr_tempdir,
      output = 1L
    ),
    regexp = "output"
  )
  # ----------------------------------------------------------------------------
  # Improper batch arguments.
  testthat::expect_error(
    amadeusdb::sedona_hms(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = chr_tempdir,
      output = tempdir(),
      batch = NULL
    ),
    regexp = "batch"
  )
  testthat::expect_error(
    amadeusdb::sedona_hms(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = chr_tempdir,
      output = tempdir(),
      batch = 1.111111
    ),
    regexp = "batch"
  )
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})
################################################################################

################################################################################
# Alignment.
################################################################################
testthat::test_that("sedona_hms == amadeus::*_hms; polygons", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  file.remove(chr_tempdir)
  # ----------------------------------------------------------------------------
  # Define common arguments.
  chr_dates <- "2023-06-07"
  chr_hmspath <- testthat::test_path("..", "testdata", "hms")
  polygons <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "polygons.shp"
  ))
  chr_locsid <- "NAME"
  int_radius <- 0
  # ----------------------------------------------------------------------------
  # No errors at runtime.
  testthat::expect_no_error(
    db_hms <- amadeusdb::sedona_hms(
      date = chr_dates,
      input = chr_hmspath,
      output = chr_tempdir,
      locs = polygons,
      locs_id = chr_locsid,
      radius = int_radius
    )
  )
  testthat::expect_no_error(db_hms1 <- sedonadb::sd_read_parquet(db_hms))
  testthat::expect_no_error(
    df_sedona_hms <- sedonadb::sd_collect(db_hms1) |> dplyr::arrange(NAME, time)
  )
  # ----------------------------------------------------------------------------
  # Base {amadeus} implementation.
  df_procalc_hms <- suppressWarnings(amadeus::calculate_hms(
    from = amadeus::process_hms(
      date = chr_dates,
      path = chr_hmspath
    ),
    locs = polygons,
    locs_id = chr_locsid,
    radius = int_radius,
    geom = FALSE
  )) |>
    # Sort by characterized NAME, but does not change the class.
    dplyr::arrange(as.character(NAME), time)
  # ----------------------------------------------------------------------------
  # Expectations across {amadeus} and {amadeusdb} functions.
  # ----------------------------------------------------------------------------
  # Dimensions.
  testthat::expect_equal(dim(df_sedona_hms), dim(df_procalc_hms))
  # ----------------------------------------------------------------------------
  # Column mames
  testthat::expect_all_true(names(df_sedona_hms) == names(df_procalc_hms))
  # ----------------------------------------------------------------------------
  # Locations (10/29: fails as {amadeus::*_hms} returns factored names).
  testthat::expect_all_true(df_sedona_hms$NAME == df_procalc_hms$NAME)
  # ----------------------------------------------------------------------------
  # Dates (10/29: {amadeus::*_hms} returns POSIXct date).
  testthat::expect_all_true(df_sedona_hms$time == as.Date(df_procalc_hms$time))
  # ----------------------------------------------------------------------------
  # Values (3 = light; 4 = medium; 5 = heavy).
  testthat::expect_all_true(df_sedona_hms[, 3] == df_procalc_hms[, 3])
  testthat::expect_all_true(df_sedona_hms[, 4] == df_procalc_hms[, 4])
  testthat::expect_all_true(df_sedona_hms[, 5] == df_procalc_hms[, 5])
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_hms == amadeus::*_hms; points", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  file.remove(chr_tempdir)
  # ----------------------------------------------------------------------------
  # Define common arguments.
  chr_dates <- "2023-06-07"
  chr_hmspath <- testthat::test_path("..", "testdata", "hms")
  points <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "points.shp"
  ))
  chr_locsid <- "NAME"
  int_radius <- 0
  # ----------------------------------------------------------------------------
  # No errors at runtime.
  testthat::expect_no_error(
    db_hms <- amadeusdb::sedona_hms(
      date = chr_dates,
      input = chr_hmspath,
      output = chr_tempdir,
      locs = points,
      locs_id = chr_locsid,
      radius = int_radius
    )
  )
  testthat::expect_no_error(db_hms1 <- sedonadb::sd_read_parquet(db_hms))
  testthat::expect_no_error(
    df_sedona_hms <- sedonadb::sd_collect(db_hms1) |> dplyr::arrange(NAME, time)
  )
  # ----------------------------------------------------------------------------
  # Base {amadeus} implementation.
  df_procalc_hms <- suppressWarnings(amadeus::calculate_hms(
    from = amadeus::process_hms(
      date = chr_dates,
      path = chr_hmspath
    ),
    locs = points,
    locs_id = chr_locsid,
    radius = int_radius,
    geom = FALSE
  )) |>
    # Sort by characterized NAME, but does not change the class.
    dplyr::arrange(as.character(NAME), time)
  # ----------------------------------------------------------------------------
  # Expectations across {amadeus} and {amadeusdb} functions.
  # ----------------------------------------------------------------------------
  # Dimensions.
  testthat::expect_equal(dim(df_sedona_hms), dim(df_procalc_hms))
  # ----------------------------------------------------------------------------
  # Column mames.
  testthat::expect_all_true(names(df_sedona_hms) == names(df_procalc_hms))
  # ----------------------------------------------------------------------------
  # Locations (10/29: fails as {amadeus::*_hms} returns factored names).
  testthat::expect_all_true(df_sedona_hms$NAME == df_procalc_hms$NAME)
  # ----------------------------------------------------------------------------
  # Dates (10/29: {amadeus::*_hms} returns POSIXct date).
  testthat::expect_all_true(df_sedona_hms$time == as.Date(df_procalc_hms$time))
  # ----------------------------------------------------------------------------
  # Values (3 = light; 4 = medium; 5 = heavy).
  testthat::expect_all_true(df_sedona_hms[, 3] == df_procalc_hms[, 3])
  testthat::expect_all_true(df_sedona_hms[, 4] == df_procalc_hms[, 4])
  testthat::expect_all_true(df_sedona_hms[, 5] == df_procalc_hms[, 5])
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_hms == amadeus::*_hms; radius = 1e6", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "hms")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  file.remove(chr_tempdir)
  # ----------------------------------------------------------------------------
  # Define common arguments.
  chr_dates <- "2023-06-07"
  chr_hmspath <- testthat::test_path("..", "testdata", "hms")
  points <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "points.shp"
  ))
  chr_locsid <- "NAME"
  int_radius <- 1e6
  # ----------------------------------------------------------------------------
  # No errors at runtime.
  testthat::expect_no_error(
    db_hms <- amadeusdb::sedona_hms(
      date = chr_dates,
      input = chr_hmspath,
      output = chr_tempdir,
      locs = points,
      locs_id = chr_locsid,
      radius = int_radius
    )
  )
  testthat::expect_no_error(db_hms1 <- sedonadb::sd_read_parquet(db_hms))
  testthat::expect_no_error(
    df_sedona_hms <- sedonadb::sd_collect(db_hms1) |> dplyr::arrange(NAME, time)
  )
  # ----------------------------------------------------------------------------
  # Base {amadeus} implementation.
  df_procalc_hms <- suppressWarnings(amadeus::calculate_hms(
    from = amadeus::process_hms(
      date = chr_dates,
      path = chr_hmspath
    ),
    locs = points,
    locs_id = chr_locsid,
    radius = int_radius,
    geom = FALSE
  )) |>
    # Sort by characterized NAME, but does not change the class.
    dplyr::arrange(as.character(NAME), time)
  # ----------------------------------------------------------------------------
  # Expectations across {amadeus} and {amadeusdb} functions.
  # ----------------------------------------------------------------------------
  # Dimensions.
  testthat::expect_equal(dim(df_sedona_hms), dim(df_procalc_hms))
  # ----------------------------------------------------------------------------
  # Column mames.
  testthat::expect_all_true(names(df_sedona_hms) == names(df_procalc_hms))
  # ----------------------------------------------------------------------------
  # Locations (10/29: fails as {amadeus::*_hms} returns factored names).
  testthat::expect_all_true(df_sedona_hms$NAME == df_procalc_hms$NAME)
  # ----------------------------------------------------------------------------
  # Dates (10/29: {amadeus::*_hms} returns POSIXct date).
  testthat::expect_all_true(df_sedona_hms$time == as.Date(df_procalc_hms$time))
  # ----------------------------------------------------------------------------
  # Values (3 = light; 4 = medium; 5 = heavy).
  testthat::expect_all_true(df_sedona_hms[, 3] == df_procalc_hms[, 3])
  testthat::expect_all_true(df_sedona_hms[, 4] == df_procalc_hms[, 4])
  testthat::expect_all_true(df_sedona_hms[, 5] == df_procalc_hms[, 5])
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})
################################################################################
