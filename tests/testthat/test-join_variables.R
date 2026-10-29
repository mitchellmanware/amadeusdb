################################################################################
# Unit and integration tests for the join_variables function.

################################################################################
library(testthat)

################################################################################
# Success cases.
################################################################################
testthat::test_that("join_variables; GEOS (CO & O3); points", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  chr_output <- file.path(tempdir(), "output")
  # ----------------------------------------------------------------------------
  # Import fixture points.
  points <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "points.shp"
  ))
  # ----------------------------------------------------------------------------
  # Define assets (directories stored) and variables.
  df_geos_input <- data.frame(
    asset = c("aqc_tavg", "chm_tavg"),
    variable = c("O3", "CO")
  )
  # ----------------------------------------------------------------------------
  # Split the data.frame into a list.
  list_geos_input <- split(df_geos_input, seq_len(nrow(df_geos_input)))
  # ----------------------------------------------------------------------------
  # Apply sedona_geos to all variables in the list.
  # No errors at runtime.
  testthat::expect_no_error(
    list_geos_sptmpl <- lapply(
      list_geos_input,
      function(x) {
        amadeusdb::sedona_geos(
          date = "2018-01-01",
          input = testthat::test_path("..", "testdata", "geos", x$asset),
          output = chr_tempdir,
          variable = x$variable,
          locs = points,
          locs_id = "NAME",
          radius = 0L
        )
      }
    )
  )
  names(list_geos_sptmpl) <- NULL
  # ----------------------------------------------------------------------------
  # No errors at runtime.
  testthat::expect_no_error(
    db_geos <- amadeusdb::join_variables(
      input = list_geos_sptmpl,
      output = chr_output,
      by = c("NAME", "time", "level")
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
  # Expect 4 columns (locs_id, time, O3, CO)
  testthat::expect_equal(ncol(df_geos), 5) # !!! drop level dev!!!
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_geos$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_s3_class(df_geos$time, "POSIXct")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(df_geos$o3_0), "numeric")
  testthat::expect_equal(class(df_geos$co_0), "numeric")
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
})

################################################################################
# Failure cases.
################################################################################
testthat::test_that("join_variables; expected failures", code = {
  # ----------------------------------------------------------------------------
  # Initiate true GEOS variable data for mock failures with real input files.
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  chr_output <- file.path(tempdir(), "output")
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
  # Improper input arguments.
  testthat::expect_error(
    amadeusdb::join_variables(input = NULL),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::join_variables(input = 1L),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::join_variables(input = "path"),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::join_variables(input = list(var1 = c(1L))),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::join_variables(input = list(var1 = c("file_does_not_exist"))),
    regexp = "File does not exist"
  )
  # ----------------------------------------------------------------------------
  # Improper output arguments.
  testthat::expect_error(
    amadeusdb::join_variables(
      input = list(var1 = db_geos),
      output = NULL
    ),
    regexp = "output"
  )
  testthat::expect_error(
    amadeusdb::join_variables(
      input = list(var1 = db_geos),
      output = 1L
    ),
    regexp = "output"
  )
  # ----------------------------------------------------------------------------
  # Improper by arguments.
  testthat::expect_error(
    amadeusdb::join_variables(
      input = list(var1 = db_geos),
      output = chr_output,
      by = NULL
    ),
    regexp = "by"
  )
  testthat::expect_error(
    amadeusdb::join_variables(
      input = list(var1 = db_geos),
      output = chr_output,
      by = 1L
    ),
    regexp = "by"
  )
  testthat::expect_error(
    amadeusdb::join_variables(
      input = list(var1 = db_geos),
      output = chr_output,
      by = "invalid"
    ),
    regexp = "join columns"
  )
  # ----------------------------------------------------------------------------
  # No data columns in input data.
  testthat::expect_error(
    amadeusdb::join_variables(
      input = list(
        var1 = db_geos,
        var2 = testthat::test_path("..", "testdata", "join", "nodata.parquet")
      ),
      output = chr_output,
      by = c("NAME", "time")
    ),
    regexp = "No data columns"
  )
  # ----------------------------------------------------------------------------
  # Conflicting data columns (O3 data tried to merge twice).
  testthat::expect_error(
    amadeusdb::join_variables(
      input = list(var1 = db_geos, var2 = db_geos),
      output = chr_output,
      by = c("NAME", "time")
    ),
    regexp = "Data columns"
  )
  # ----------------------------------------------------------------------------
  # Missing join key (include "NAME" but not "time")
  testthat::expect_error(
    amadeusdb::join_variables(
      input = list(var1 = db_geos),
      output = chr_output,
      by = c("NAME")
    ),
    regexp = "Duplicate or missing join keys"
  )
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
  file.remove(list.files(chr_output, full.names = TRUE, recursive = TRUE))
  unlink(chr_output)
})
