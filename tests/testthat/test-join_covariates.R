################################################################################
# Unit and integration tests for the join_covariates function.

################################################################################
library(testthat)

################################################################################
# Success cases.
################################################################################
testthat::test_that("join_covariates; GEOS (O3) & HMS", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_geosdir <- file.path(tempdir(), "geos")
  chr_hmsdir <- file.path(tempdir(), "hms")
  chr_joindir <- file.path(tempdir(), "join")
  # ----------------------------------------------------------------------------
  # Initiate common date.
  chr_date <- "2018-01-01"
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_geosdir))
  suppressWarnings(file.remove(chr_hmsdir))
  suppressWarnings(file.remove(chr_joindir))
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
    db_geos <- amadeusdb::sedona_geos(
      date = chr_date,
      input = testthat::test_path("..", "testdata", "geos", "aqc_tavg"),
      output = chr_geosdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # No errors at runtime.
  testthat::expect_no_error(
    db_hms <- amadeusdb::sedona_hms(
      date = chr_date,
      input = testthat::test_path("..", "testdata", "hms"),
      output = chr_hmsdir,
      locs = polygons,
      locs_id = "NAME"
    )
  )
  # ----------------------------------------------------------------------------
  # No errors at runtime.
  testthat::expect_no_error(
    db_join <- join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_joindir,
      hourly = list(geos = db_geos),
      daily = list(hms = db_hms),
      static = NULL,
      locs_id = "NAME",
      prefix = FALSE
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output database.
  testthat::expect_true(all(grepl("\\.parquet", db_join)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_join1 <- sedonadb::sd_read_parquet(db_join))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_join1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_join <- sedonadb::sd_collect(db_join1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(df_join), 8)
  # ----------------------------------------------------------------------------
  # Expect 10 columns (NAME, time, lon/lat, outcome, 1 GEOS, 3 HMS).
  testthat::expect_equal(ncol(df_join), 10) # !!! drop level dev!!!
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_join$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_s3_class(df_join$time, "POSIXct")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(df_join$o3_0), "numeric")
  testthat::expect_equal(class(df_join$light_00000), "numeric")
  testthat::expect_equal(class(df_join$medium_00000), "numeric")
  testthat::expect_equal(class(df_join$heavy_00000), "numeric")
  # ----------------------------------------------------------------------------
  # Expect light, medium, and heavy columns contain only 0 or 1.
  testthat::expect_true(all(
    unique(c(
      df_join$light_00000,
      df_join$medium_00000,
      df_join$heavy_00000
    )) %in%
      c(0, 1)
  ))
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  for (c in c(chr_geosdir, chr_hmsdir, chr_joindir)) {
    file.remove(list.files(c, full.names = TRUE, recursive = TRUE))
    unlink(c)
  }
})

################################################################################
testthat::test_that("join_covariates; GEOS (O3 & CO) & HMS", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_geosdir <- file.path(tempdir(), "geos")
  chr_hmsdir <- file.path(tempdir(), "hms")
  chr_joindir <- file.path(tempdir(), "join")
  # ----------------------------------------------------------------------------
  # Initiate common date.
  chr_date <- "2018-01-01"
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_geosdir))
  suppressWarnings(file.remove(chr_hmsdir))
  suppressWarnings(file.remove(chr_joindir))
  # ----------------------------------------------------------------------------
  # Import fixture points.
  polygons <- sf::st_read(testthat::test_path(
    "..",
    "testdata",
    "locs",
    "polygons.shp"
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
          date = chr_date,
          input = testthat::test_path("..", "testdata", "geos", x$asset),
          output = chr_geosdir,
          variable = x$variable,
          locs = polygons,
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
      output = chr_geosdir,
      by = c("NAME", "time", "level")
    )
  )
  # ----------------------------------------------------------------------------
  # No errors at runtime.
  testthat::expect_no_error(
    db_hms <- amadeusdb::sedona_hms(
      date = chr_date,
      input = testthat::test_path("..", "testdata", "hms"),
      output = chr_hmsdir,
      locs = polygons,
      locs_id = "NAME"
    )
  )
  # ----------------------------------------------------------------------------
  # No errors at runtime.
  testthat::expect_no_error(
    db_join <- join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_joindir,
      hourly = list(geos = db_geos),
      daily = list(hms = db_hms),
      static = NULL,
      locs_id = "NAME",
      prefix = FALSE
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output database.
  testthat::expect_true(all(grepl("\\.parquet", db_join)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_join1 <- sedonadb::sd_read_parquet(db_join))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_join1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_join <- sedonadb::sd_collect(db_join1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(df_join), 8)
  # ----------------------------------------------------------------------------
  # Expect 10 columns (NAME, time, lon/lat, outcome, 2 GEOS, 3 HMS).
  testthat::expect_equal(ncol(df_join), 11) # !!! drop level dev!!!
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_join$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_s3_class(df_join$time, "POSIXct")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(df_join$o3_0), "numeric")
  testthat::expect_equal(class(df_join$co_0), "numeric")
  testthat::expect_equal(class(df_join$light_00000), "numeric")
  testthat::expect_equal(class(df_join$medium_00000), "numeric")
  testthat::expect_equal(class(df_join$heavy_00000), "numeric")
  # ----------------------------------------------------------------------------
  # Expect light, medium, and heavy columns contain only 0 or 1.
  testthat::expect_true(all(
    unique(c(
      df_join$light_00000,
      df_join$medium_00000,
      df_join$heavy_00000
    )) %in%
      c(0, 1)
  ))
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  for (c in c(chr_geosdir, chr_hmsdir, chr_joindir)) {
    file.remove(list.files(c, full.names = TRUE, recursive = TRUE))
    unlink(c)
  }
})

################################################################################
testthat::test_that("join_covariates; GEOS (empty) & HMS", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_hmsdir <- file.path(tempdir(), "hms")
  chr_joindir <- file.path(tempdir(), "join")
  # ----------------------------------------------------------------------------
  # Initiate common date.
  chr_date <- "2018-01-01"
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_hmsdir))
  suppressWarnings(file.remove(chr_joindir))
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
    db_hms <- amadeusdb::sedona_hms(
      date = chr_date,
      input = testthat::test_path("..", "testdata", "hms"),
      output = chr_hmsdir,
      locs = polygons,
      locs_id = "NAME"
    )
  )
  # ----------------------------------------------------------------------------
  # No errors at runtime.
  testthat::expect_no_error(
    db_join <- join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_joindir,
      hourly = list(),
      daily = list(hms = db_hms),
      static = NULL,
      locs_id = "NAME",
      prefix = FALSE
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Only .parquet files in the output database.
  testthat::expect_true(all(grepl("\\.parquet", db_join)))
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_join1 <- sedonadb::sd_read_parquet(db_join))
  # ----------------------------------------------------------------------------
  # Expect SedonaDB S3 class.
  testthat::expect_s3_class(db_join1, "sedonadb_dataframe")
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_join <- sedonadb::sd_collect(db_join1))
  # ----------------------------------------------------------------------------
  # Expect 8 rows (4 fixture site x 2 hours).
  testthat::expect_equal(nrow(df_join), 8)
  # ----------------------------------------------------------------------------
  # Expect 10 columns (NAME, time, lon/lat, outcome, 0 GEOS, 3 HMS).
  testthat::expect_equal(ncol(df_join), 8)
  # ----------------------------------------------------------------------------
  # Expect character location identifer column.
  testthat::expect_type(df_join$NAME, "character")
  # ----------------------------------------------------------------------------
  # Expect character "time" column.
  testthat::expect_s3_class(df_join$time, "POSIXct")
  # ----------------------------------------------------------------------------
  # Expect numeric for variable columns.
  testthat::expect_equal(class(df_join$light_00000), "numeric")
  testthat::expect_equal(class(df_join$medium_00000), "numeric")
  testthat::expect_equal(class(df_join$heavy_00000), "numeric")
  # ----------------------------------------------------------------------------
  # Expect light, medium, and heavy columns contain only 0 or 1.
  testthat::expect_true(all(
    unique(c(
      df_join$light_00000,
      df_join$medium_00000,
      df_join$heavy_00000
    )) %in%
      c(0, 1)
  ))
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  for (c in c(chr_hmsdir, chr_joindir)) {
    file.remove(list.files(c, full.names = TRUE, recursive = TRUE))
    unlink(c)
  }
})

################################################################################
testthat::test_that("join_covariates; database directory", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_geosdir <- file.path(tempdir(), "geos")
  chr_hmsdir <- file.path(tempdir(), "hms")
  chr_joindir <- file.path(tempdir(), "join")
  # ----------------------------------------------------------------------------
  # Initiate common date.
  chr_date <- "2018-01-01"
  # ----------------------------------------------------------------------------
  # Remove temporary directory to force recreation.
  suppressWarnings(file.remove(chr_geosdir))
  suppressWarnings(file.remove(chr_hmsdir))
  suppressWarnings(file.remove(chr_joindir))
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
    db_geos <- amadeusdb::sedona_geos(
      date = chr_date,
      input = testthat::test_path("..", "testdata", "geos", "aqc_tavg"),
      output = chr_geosdir,
      locs = polygons,
      locs_id = "NAME",
      radius = 0,
      variable = "O3"
    )
  )
  # ----------------------------------------------------------------------------
  # No errors at runtime.
  testthat::expect_no_error(
    db_hms <- amadeusdb::sedona_hms(
      date = chr_date,
      input = testthat::test_path("..", "testdata", "hms"),
      output = chr_hmsdir,
      locs = polygons,
      locs_id = "NAME"
    )
  )
  # ----------------------------------------------------------------------------
  # No errors at runtime (full .parquet).
  testthat::expect_no_error(
    db_pqt <- join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_joindir,
      hourly = list(geos = db_geos),
      daily = list(hms = db_hms),
      static = NULL,
      locs_id = "NAME",
      prefix = FALSE
    )
  )
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_pqt1 <- sedonadb::sd_read_parquet(db_pqt))
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_pqt <- sedonadb::sd_collect(db_pqt1))
  # ----------------------------------------------------------------------------
  # No errors at runtime (directory only).
  testthat::expect_no_error(
    db_dir <- join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_joindir,
      hourly = list(geos = chr_geosdir),
      daily = list(hms = chr_hmsdir),
      static = NULL,
      locs_id = "NAME",
      prefix = FALSE
    )
  )
  # ----------------------------------------------------------------------------
  # No errors on read as .parquet.
  testthat::expect_no_error(db_dir1 <- sedonadb::sd_read_parquet(db_dir))
  # ----------------------------------------------------------------------------
  # No errors on collection to data frame.
  testthat::expect_no_error(df_dir <- sedonadb::sd_collect(db_dir1))
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Expect identical outcomes from both methods.
  testthat::expect_identical(df_pqt, df_dir)
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  for (c in c(chr_geosdir, chr_hmsdir, chr_joindir)) {
    file.remove(list.files(c, full.names = TRUE, recursive = TRUE))
    unlink(c)
  }
})

################################################################################
# Failure cases.
################################################################################
testthat::test_that("join_covariates; expected failures", code = {
  # ----------------------------------------------------------------------------
  # Initiate true GEOS variable data for mock failures with real input files.
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "geos")
  chr_output <- file.path(tempdir(), "output")
  # ----------------------------------------------------------------------------
  # Improper input arguments.
  testthat::expect_error(
    amadeusdb::join_covariates(input = NULL),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::join_covariates(input = 1L),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::join_covariates(input = list()),
    regexp = "input"
  )
  testthat::expect_error(
    amadeusdb::join_covariates(input = "file_does_not_exist"),
    regexp = "input"
  )
  # ----------------------------------------------------------------------------
  # Improper output arguments.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = NULL
    ),
    regexp = "output"
  )
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = 1L
    ),
    regexp = "output"
  )
  # ----------------------------------------------------------------------------
  # Improper `locs_id` argument.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = NULL
    ),
    regexp = "locs_id"
  )
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = 1L
    ),
    regexp = "locs_id"
  )
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = c()
    ),
    regexp = "locs_id"
  )
  # ----------------------------------------------------------------------------
  # Improper `time_id` argument.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = "NAME",
      time_id = NULL
    ),
    regexp = "time_id"
  )
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = "NAME",
      time_id = 1L
    ),
    regexp = "time_id"
  )
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = "NAME",
      time_id = c()
    ),
    regexp = "time_id"
  )
  # ----------------------------------------------------------------------------
  # Improper `date_id` argument.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = "NAME",
      time_id = "time",
      date_id = NULL
    ),
    regexp = "date_id"
  )
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = "NAME",
      time_id = "time",
      date_id = 1L
    ),
    regexp = "date_id"
  )
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = "NAME",
      time_id = "time",
      date_id = c()
    ),
    regexp = "date_id"
  )
  # ----------------------------------------------------------------------------
  # Improper `prefix` argument.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = "NAME",
      time_id = "time",
      date_id = "date",
      prefix = NULL
    ),
    regexp = "prefix"
  )
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = "NAME",
      time_id = "time",
      date_id = "date",
      prefix = "TRUE"
    ),
    regexp = "prefix"
  )
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = "NAME",
      time_id = "time",
      date_id = "date",
      prefix = 1L
    ),
    regexp = "prefix"
  )
  # ----------------------------------------------------------------------------
  # Path with no parquet files.
  chr_empty <- file.path(chr_tempdir, "empty")
  suppressWarnings(dir.create(chr_empty))
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = "NAME",
      time_id = "time",
      date_id = "date",
      prefix = FALSE,
      hourly = list(geos = chr_empty)
    ),
    regexp = "Parquet"
  )
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
  # Conflicting data columns (O3 data tried to merge twice).
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = testthat::test_path(
        "..",
        "testdata",
        "locs",
        "db_hourly.parquet"
      ),
      output = chr_tempdir,
      locs_id = "NAME",
      time_id = "time",
      date_id = "date",
      prefix = FALSE,
      hourly = list(var1 = db_geos, var2 = db_geos)
    ),
    regexp = "Conflicting data columns"
  )
  # ----------------------------------------------------------------------------
  # Clean up temporary directory.
  file.remove(list.files(chr_tempdir, full.names = TRUE, recursive = TRUE))
  unlink(chr_tempdir)
  file.remove(list.files(chr_output, full.names = TRUE, recursive = TRUE))
  unlink(chr_output)
})
