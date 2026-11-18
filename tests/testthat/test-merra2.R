################################################################################
# Unit and integration tests for the sedona_merra2 function and auxiliaries.

################################################################################
library(testthat)

################################################################################
# Success cases.
################################################################################
testthat::test_that("sedona_merra2; locs = sf::polygons; tavg1", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "merra2")
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
  # No errors at runtime.
  testthat::expect_no_error(
    db_merra2 <- amadeusdb::sedona_merra2(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
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
  testthat::expect_true(all(grepl("\\.parquet", db_merra2)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_merra21 <- sedonadb::sd_read_parquet(db_merra2))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_merra21, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_merra2 <- sedonadb::sd_collect(db_merra21))
  # ----------------------------------------------------------------------------
  # Expect 96 rows (4 fixture site x 24 hours).
  testthat::expect_equal(nrow(df_merra2), 96)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(df_merra2), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_merra2$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect POSIXct"time" column.
  testthat::expect_s3_class(df_merra2$time, "POSIXct")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(df_merra2$o3_0), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_merra2; locs = sf::points; tavg1", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "merra2")
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
  # No errors at runtime.
  testthat::expect_no_error(
    db_merra2 <- amadeusdb::sedona_merra2(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
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
  testthat::expect_true(all(grepl("\\.parquet", db_merra2)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_merra21 <- sedonadb::sd_read_parquet(db_merra2))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_merra21, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_merra2 <- sedonadb::sd_collect(db_merra21))
  # ----------------------------------------------------------------------------
  # Expect 96 rows (4 fixture site x 24 hours).
  testthat::expect_equal(nrow(df_merra2), 96)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(df_merra2), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_merra2$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect POSIXct"time" column.
  testthat::expect_s3_class(df_merra2$time, "POSIXct")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(df_merra2$o3_0), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_merra2; locs = terra::polygons; tavg1", {
  # ----------------------------------------------------------------------------
  # {terra} functions omitted while debugging the C stack overflower error.
  # ----------------------------------------------------------------------------
})

################################################################################
testthat::test_that("sedona_merra2; locs = terra::points; tavg1", {
  # ----------------------------------------------------------------------------
  # {terra} functions omitted while debugging the C stack overflower error.
  # ----------------------------------------------------------------------------
})

################################################################################
testthat::test_that("sedona_merra2; locs = data.frame; tavg1", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "merra2")
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_tempdir))
  # ----------------------------------------------------------------------------
  # Import data.frame points.
  df <- read.csv(testthat::test_path("..", "testdata", "locs", "df.csv"))
  # ----------------------------------------------------------------------------
  # No errors at runtime.
  testthat::expect_no_error(
    db_merra2 <- amadeusdb::sedona_merra2(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
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
  # Only .parquet files in the output database.
  testthat::expect_true(all(grepl("\\.parquet", db_merra2)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_merra21 <- sedonadb::sd_read_parquet(db_merra2))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_merra21, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_merra2 <- sedonadb::sd_collect(db_merra21))
  # ----------------------------------------------------------------------------
  # Expect 96 rows (4 fixture site x 24 hours).
  testthat::expect_equal(nrow(df_merra2), 96)
  # ----------------------------------------------------------------------------
  # Expect 3 columns (locs_id, time, variable)
  testthat::expect_equal(ncol(df_merra2), 3)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_merra2$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect POSIXct"time" column.
  testthat::expect_s3_class(df_merra2$time, "POSIXct")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(df_merra2$o3_0), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})


################################################################################
testthat::test_that("sedona_merra2; skip existing .parquet", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "merra2")
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
  # No errors at runtime.
  testthat::expect_no_error(
    db_merra2 <- amadeusdb::sedona_merra2(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME",
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # Expect skips when re-run with same date and file requests.
  testthat::expect_message(
    amadeusdb::sedona_merra2(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
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
testthat::test_that("sedona_merra2; expected failures", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "merra2")
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
    amadeusdb::sedona_merra2(input = chr_tempdir, locs = "locs"),
    "locs"
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      input = chr_tempdir,
      locs = data.frame(site_id = 1)
    ),
    "locs"
  )
  # ----------------------------------------------------------------------------
  # Improper radius arguments.
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      radius = -1
    ),
    regexp = "radius"
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      radius = NULL
    ),
    regexp = "radius"
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      radius = "text"
    ),
    regexp = "radius"
  )
  # ----------------------------------------------------------------------------
  # Improper date arguments.
  testthat::expect_error(
    amadeusdb::sedona_merra2(locs = points, locs_id = "NAME", date = NULL),
    regexp = "date"
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(locs = points, locs_id = "NAME", date = 1L),
    regexp = "date"
  )
  # ----------------------------------------------------------------------------
  # Improper input arguments.
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = NULL
    ),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = 10
    ),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
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
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = chr_tempdir,
      output = NULL
    ),
    regexp = "output"
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
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
    amadeusdb::sedona_merra2(
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
    amadeusdb::sedona_merra2(
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
  # Improper fun arguments.
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = chr_tempdir,
      output = tempdir(),
      batch = 1L,
      fun = NULL
    )
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = chr_tempdir,
      output = tempdir(),
      batch = 1L,
      fun = "function"
    )
  )
  # ----------------------------------------------------------------------------
  # Improper variable arguments.
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = chr_tempdir,
      output = tempdir(),
      batch = 1L,
      variable = NULL
    ),
    regexp = "variable"
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = chr_tempdir,
      output = tempdir(),
      batch = 1L,
      variable = 1L
    ),
    regexp = "variable"
  )
  # ----------------------------------------------------------------------------
  # Improper level arguements.
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = tempdir(),
      batch = 1L,
      variable = "O3",
      band = 2 # single level for current data
    )
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = tempdir(),
      batch = 1L,
      variable = "O3",
      band = 2 # single level for current data
    )
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = tempdir(),
      batch = 1L,
      variable = "O3",
      band = 2.1
    )
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = tempdir(),
      batch = 1L,
      variable = "O3",
      band = "level"
    )
  )
  # ----------------------------------------------------------------------------
  # Improper touches arguements.
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = tempdir(),
      batch = 1L,
      variable = "O3",
      band = 1L,
      touches = 1
    ),
    regexp = "touches"
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = tempdir(),
      batch = 1L,
      variable = "O3",
      band = 1L,
      touches = "no"
    ),
    regexp = "touches"
  )
  # ----------------------------------------------------------------------------
  # Improper exclude_nodata arguements.
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = tempdir(),
      batch = 1L,
      variable = "O3",
      band = 1L,
      touches = TRUE,
      exclude_nodata = 1L
    ),
    regexp = "exclude_nodata"
  )
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = tempdir(),
      batch = 1L,
      variable = "O3",
      band = 1L,
      touches = TRUE,
      exclude_nodata = "no"
    ),
    regexp = "exclude_nodata"
  )
  # ----------------------------------------------------------------------------
  # More than one collection.
  testthat::expect_error(
    amadeusdb::sedona_merra2(
      locs = points,
      locs_id = "NAME",
      date = "2018-01-01",
      input = testthat::test_path("..", "testdata", "merra2", "all"),
      output = tempdir(),
      batch = 1L,
      variable = "O3",
      band = 1L,
      touches = TRUE,
      exclude_nodata = TRUE
    ),
    regexp = "single collection"
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
testthat::test_that("sedona_merra2 == amadeus::*_merra2; mean; polygons", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "merra2")
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
  # No errors at runtime.
  testthat::expect_no_error(
    db_merra2 <- amadeusdb::sedona_merra2(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3",
      fun = "mean"
    )
  )
  testthat::expect_no_error(db_merra21 <- sedonadb::sd_read_parquet(db_merra2))
  testthat::expect_no_error(
    df_sedona_merra2 <- sedonadb::sd_collect(db_merra21) |>
      dplyr::arrange(NAME, time)
  )
  # ----------------------------------------------------------------------------
  # Base {amadeus} implementation.
  # !!! Read in from .rds file as mixing {terra} and {sedonadb} operations !!!
  # !!! was causing C stack errors. See inst/scripts/03_fixture_merra2.R !!!
  df_procalc_merra2 <- readRDS(testthat::test_path(
    "..",
    "testdata",
    "amadeus",
    "merra2",
    "df_procalc_mean.rds"
  )) |>
    dplyr::arrange(as.character(NAME), time)
  # ----------------------------------------------------------------------------
  # Expectations across {amadeus} and {amadeusdb} functions.
  # ----------------------------------------------------------------------------
  # Dimensions.
  testthat::expect_equal(dim(df_sedona_merra2), dim(df_procalc_merra2))
  # ----------------------------------------------------------------------------
  # Column mames.
  testthat::expect_all_true(names(df_sedona_merra2) == names(df_procalc_merra2))
  # ----------------------------------------------------------------------------
  # Locations.
  testthat::expect_all_true(df_sedona_merra2$NAME == df_procalc_merra2$NAME)
  # ----------------------------------------------------------------------------
  # Times.
  testthat::expect_all_true(df_sedona_merra2$time == df_procalc_merra2$time)
  # ----------------------------------------------------------------------------
  # Values (O3). !!! false before deep dive on sedonadb geometry methods. !!!
  testthat::expect_all_false(df_sedona_merra2$o3_0 == df_procalc_merra2$o3_0)
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_merra2 == amadeus::*_merra2; max; polygons", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "merra2")
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
  # No errors at runtime.
  testthat::expect_no_error(
    db_merra2 <- amadeusdb::sedona_merra2(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3",
      fun = "max"
    )
  )
  testthat::expect_no_error(db_merra21 <- sedonadb::sd_read_parquet(db_merra2))
  testthat::expect_no_error(
    df_sedona_merra2 <- sedonadb::sd_collect(db_merra21) |>
      dplyr::arrange(NAME, time)
  )
  # ----------------------------------------------------------------------------
  # Base {amadeus} implementation.
  # !!! Read in from .rds file as mixing {terra} and {sedonadb} operations !!!
  # !!! was causing C stack errors. See inst/scripts/03_fixture_merra2.R !!!
  df_procalc_merra2 <- readRDS(testthat::test_path(
    "..",
    "testdata",
    "amadeus",
    "merra2",
    "df_procalc_max.rds"
  )) |>
    dplyr::arrange(as.character(NAME), time)
  # ----------------------------------------------------------------------------
  # Expect identical output across {amadeus} and {amadeusdb} functions.
  # ----------------------------------------------------------------------------
  testthat::expect_identical(df_sedona_merra2, df_procalc_merra2)
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_merra2 == amadeus::*_merra2; min; polygons", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "merra2")
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
  # No errors at runtime.
  testthat::expect_no_error(
    db_merra2 <- amadeusdb::sedona_merra2(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = chr_tempdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3",
      fun = "min"
    )
  )
  testthat::expect_no_error(db_merra21 <- sedonadb::sd_read_parquet(db_merra2))
  testthat::expect_no_error(
    df_sedona_merra2 <- sedonadb::sd_collect(db_merra21) |>
      dplyr::arrange(NAME, time)
  )
  # ----------------------------------------------------------------------------
  # Base {amadeus} implementation.
  # !!! Read in from .rds file as mixing {terra} and {sedonadb} operations !!!
  # !!! was causing C stack errors. See inst/scripts/03_fixture_merra2.R !!!
  df_procalc_merra2 <- readRDS(testthat::test_path(
    "..",
    "testdata",
    "amadeus",
    "merra2",
    "df_procalc_min.rds"
  )) |>
    dplyr::arrange(as.character(NAME), time)
  # ----------------------------------------------------------------------------
  # Expect identical output across {amadeus} and {amadeusdb} functions.
  # ----------------------------------------------------------------------------
  testthat::expect_identical(df_sedona_merra2, df_procalc_merra2)
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
testthat::test_that("sedona_merra2 == amadeus::*_merra2; points", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "merra2")
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
  # No errors at runtime.
  testthat::expect_no_error(
    db_merra2 <- amadeusdb::sedona_merra2(
      date = c("2018-01-01", "2018-01-01"),
      input = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "tavg1_2d_slv_Nx"
      ),
      output = chr_tempdir,
      locs = points,
      locs_id = "NAME",
      radius = 0,
      variable = "O3",
      fun = "mean"
    )
  )
  testthat::expect_no_error(db_merra21 <- sedonadb::sd_read_parquet(db_merra2))
  testthat::expect_no_error(
    df_sedona_merra2 <- sedonadb::sd_collect(db_merra21) |>
      dplyr::arrange(NAME, time)
  )
  # ----------------------------------------------------------------------------
  # Base {amadeus} implementation.
  # !!! Read in from .rds file as mixing {terra} and {sedonadb} operations !!!
  # !!! was causing C stack errors. See inst/scripts/03_fixture_merra2.R !!!
  df_procalc_merra2 <- readRDS(testthat::test_path(
    "..",
    "testdata",
    "amadeus",
    "merra2",
    "df_procalc_points.rds"
  )) |>
    dplyr::arrange(as.character(NAME), time)
  # ----------------------------------------------------------------------------
  # Expect identical output across {amadeus} and {amadeusdb} functions.
  # ----------------------------------------------------------------------------
  testthat::expect_identical(df_sedona_merra2, df_procalc_merra2)
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})
