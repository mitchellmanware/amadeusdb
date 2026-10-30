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
testthat::test_that("join_covariates; prefix = TRUE", {
  # ----------------------------------------------------------------------------
  # Initiate an isolated temporary directory and clean up even on failure.
  chr_tempdir <- file.path(tempdir(), "join")
  dir.create(chr_tempdir)
  on.exit(unlink(chr_tempdir, recursive = TRUE), add = TRUE)
  # ----------------------------------------------------------------------------
  # Import fixture data.
  list_input <- list(
    aqs = testthat::test_path("..", "testdata", "join", "aqs.parquet"),
    geos = testthat::test_path("..", "testdata", "join", "geos.parquet"),
    hms = testthat::test_path("..", "testdata", "join", "hms.parquet"),
    gmted = testthat::test_path("..", "testdata", "join", "gmted.parquet")
  )
  # ----------------------------------------------------------------------------
  # Confirm the shared name conflicts before prefixing is enabled.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = list_input[["aqs"]],
      output = file.path(chr_tempdir, "unprefixed"),
      hourly = list(geos = list_input[["geos"]]),
      prefix = FALSE
    ),
    regexp = "Conflicting data columns in hourly/geos: value",
    fixed = TRUE
  )
  # ----------------------------------------------------------------------------
  # Include two hourly datasets and a label containing a slash.
  testthat::expect_no_error(
    db_join <- amadeusdb::join_covariates(
      input = list_input[["aqs"]],
      output = file.path(chr_tempdir, "prefixed"),
      hourly = list(
        geos = list_input[["geos"]],
        "geos/alt" = list_input[["geos"]]
      ),
      daily = list(hms = list_input[["hms"]]),
      static = list(gmted = list_input[["gmted"]]),
      prefix = TRUE
    )
  )
  # ----------------------------------------------------------------------------
  # !!! OUTPUT CHECKS !!!
  # ----------------------------------------------------------------------------
  # Require newly written Parquet files and read the joined values back.
  testthat::expect_gt(length(db_join), 0L)
  testthat::expect_true(all(file.exists(db_join)))
  testthat::expect_true(all(grepl("\\.parquet$", db_join)))
  testthat::expect_no_error(
    df_join <- sedonadb::sd_collect(sedonadb::sd_read_parquet(db_join))
  )
  df_join <- df_join[order(df_join$value), , drop = FALSE]
  # ----------------------------------------------------------------------------
  # Keep observation names and prefix only non-key covariate columns.
  testthat::expect_identical(
    names(df_join),
    c(
      "site_id",
      "time",
      "value",
      "hourly_geos_value",
      "hourly_geos_alt_value",
      "daily_hms_value",
      "static_gmted_value"
    )
  )
  # ----------------------------------------------------------------------------
  # Import un-joined AQS data.
  df_aqs <- sedonadb::sd_collect(sedonadb::sd_read_parquet(list_input[["aqs"]]))
  # ----------------------------------------------------------------------------
  # Retain repeated observations, leading zeroes, and original timestamps.
  testthat::expect_equal(nrow(df_join), 3L)
  testthat::expect_identical(df_join$site_id, df_aqs$site_id)
  testthat::expect_s3_class(df_join$time, "POSIXct")
  testthat::expect_equal(as.numeric(df_join$time), as.numeric(df_aqs$time))
  testthat::expect_equal(df_join$value, df_aqs$value)
  # ----------------------------------------------------------------------------
  # Match within the hour and repeat daily/static values over observations.
  testthat::expect_equal(df_join$hourly_geos_value, c(20, 20, 21))
  testthat::expect_equal(df_join$hourly_geos_alt_value, c(20, 20, 21))
  testthat::expect_equal(df_join$daily_hms_value, rep(30, 3))
  testthat::expect_equal(df_join$static_gmted_value, rep(40, 3))
  # ----------------------------------------------------------------------------
  # Character file vectors form one table named for each resolution.
  testthat::expect_no_error(
    db_vector <- amadeusdb::join_covariates(
      input = list_input[["aqs"]],
      output = file.path(chr_tempdir, "file_vectors"),
      hourly = list_input[["geos"]],
      daily = list_input[["hms"]],
      static = list_input[["gmted"]],
      prefix = TRUE
    )
  )
  testthat::expect_gt(length(db_vector), 0L)
  testthat::expect_no_error(
    df_vector <- sedonadb::sd_collect(sedonadb::sd_read_parquet(db_vector))
  )
  df_vector <- df_vector[order(df_vector$value), , drop = FALSE]
  # ----------------------------------------------------------------------------
  # Default labels must appear in prefixes without changing the joined values.
  testthat::expect_identical(
    names(df_vector),
    c(
      "site_id",
      "time",
      "value",
      "hourly_hourly_value",
      "daily_daily_value",
      "static_static_value"
    )
  )
  testthat::expect_equal(df_vector$value, df_aqs$value)
  testthat::expect_equal(df_vector$hourly_hourly_value, c(20, 20, 21))
  testthat::expect_equal(df_vector$daily_daily_value, rep(30, 3))
  testthat::expect_equal(df_vector$static_static_value, rep(40, 3))
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

################################################################################
testthat::test_that("join_covariates; invalid resolution", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- file.path(tempdir(), "join")
  dir.create(chr_tempdir)
  on.exit(unlink(chr_tempdir, recursive = TRUE), add = TRUE)
  # ----------------------------------------------------------------------------
  # Import fixture data.
  chr_input <- testthat::test_path(
    "..",
    "testdata",
    "locs",
    "db_hourly.parquet"
  )
  # ----------------------------------------------------------------------------
  # Invalid optional resolution inputs.
  # ----------------------------------------------------------------------------
  # Invalid `hourly`.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = chr_input,
      output = chr_tempdir,
      locs_id = "NAME",
      hourly = 1L
    ),
    regexp = paste0("Assertion on 'hourly' failed:.*list")
  )
  # ----------------------------------------------------------------------------
  # Invalid inputs must not write a joined dataset.
  testthat::expect_length(
    list.files(chr_tempdir, pattern = "\\.parquet$", recursive = TRUE),
    0L
  )
  # ----------------------------------------------------------------------------
  # Invalid optional resolution inputs.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = chr_input,
      output = chr_tempdir,
      locs_id = "NAME",
      hourly = TRUE
    ),
    regexp = paste0("Assertion on 'hourly' failed:.*list")
  )
  # ----------------------------------------------------------------------------
  # Invalid inputs must not write a joined dataset.
  testthat::expect_length(
    list.files(chr_tempdir, pattern = "\\.parquet$", recursive = TRUE),
    0L
  )
  # ----------------------------------------------------------------------------
  # Invalid `daily`.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = chr_input,
      output = chr_tempdir,
      locs_id = "NAME",
      daily = 1L
    ),
    regexp = paste0("Assertion on 'daily' failed:.*list")
  )
  # ----------------------------------------------------------------------------
  # Invalid inputs must not write a joined dataset.
  testthat::expect_length(
    list.files(chr_tempdir, pattern = "\\.parquet$", recursive = TRUE),
    0L
  )
  # ----------------------------------------------------------------------------
  # Invalid optional resolution inputs.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = chr_input,
      output = chr_tempdir,
      locs_id = "NAME",
      daily = TRUE
    ),
    regexp = paste0("Assertion on 'daily' failed:.*list")
  )
  # ----------------------------------------------------------------------------
  # Invalid inputs must not write a joined dataset.
  testthat::expect_length(
    list.files(chr_tempdir, pattern = "\\.parquet$", recursive = TRUE),
    0L
  )
  # ----------------------------------------------------------------------------
  # Invalid `static`.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = chr_input,
      output = chr_tempdir,
      locs_id = "NAME",
      static = 1L
    ),
    regexp = paste0("Assertion on 'static' failed:.*list")
  )
  # ----------------------------------------------------------------------------
  # Invalid inputs must not write a joined dataset.
  testthat::expect_length(
    list.files(chr_tempdir, pattern = "\\.parquet$", recursive = TRUE),
    0L
  )
  # ----------------------------------------------------------------------------
  # Invalid optional resolution inputs.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = chr_input,
      output = chr_tempdir,
      locs_id = "NAME",
      static = TRUE
    ),
    regexp = paste0("Assertion on 'static' failed:.*list")
  )
  # ----------------------------------------------------------------------------
  # Invalid inputs must not write a joined dataset.
  testthat::expect_length(
    list.files(chr_tempdir, pattern = "\\.parquet$", recursive = TRUE),
    0L
  )
})

################################################################################
testthat::test_that("join_covariates; duplicate normalized join keys", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- tempfile("join_covariates_duplicate_")
  dir.create(chr_tempdir)
  on.exit(unlink(chr_tempdir, recursive = TRUE), add = TRUE)
  # ----------------------------------------------------------------------------
  # Import fixtures created by inst/scripts/04_fixture_join.R.
  list_input <- list(
    aqs = testthat::test_path("..", "testdata", "join", "aqs.parquet"),
    geos = testthat::test_path("..", "testdata", "join", "geos.parquet"),
    geos_duplicate = testthat::test_path(
      "..",
      "testdata",
      "join",
      "geos_duplicate.parquet"
    ),
    hms_duplicate = testthat::test_path(
      "..",
      "testdata",
      "join",
      "hms_duplicate.parquet"
    ),
    gmted_duplicate = testthat::test_path(
      "..",
      "testdata",
      "join",
      "gmted_duplicate.parquet"
    )
  )
  # ----------------------------------------------------------------------------
  # GEOS times at 00:30 and 00:55 in separate files share a normalized hour.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = list_input[["aqs"]],
      output = file.path(chr_tempdir, "hourly"),
      hourly = list(
        geos = c(
          list_input[["geos"]],
          list_input[["geos_duplicate"]]
        )
      ),
      prefix = TRUE
    ),
    regexp = "Duplicate or missing normalized join keys in source: hourly/geos",
    fixed = TRUE
  )
  # ----------------------------------------------------------------------------
  # Duplicate hourly keys must be rejected before writing a joined dataset.
  testthat::expect_length(
    list.files(
      file.path(chr_tempdir, "hourly"),
      pattern = "\\.parquet$",
      recursive = TRUE
    ),
    0L
  )
  # ----------------------------------------------------------------------------
  # HMS timestamps at 00:05 and 23:55 share a normalized date.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = list_input[["aqs"]],
      output = file.path(chr_tempdir, "daily"),
      daily = list(hms = list_input[["hms_duplicate"]]),
      prefix = TRUE
    ),
    regexp = "Duplicate or missing normalized join keys in source: daily/hms",
    fixed = TRUE
  )
  # ----------------------------------------------------------------------------
  # Duplicate daily keys must be rejected before writing a joined dataset.
  testthat::expect_length(
    list.files(
      file.path(chr_tempdir, "daily"),
      pattern = "\\.parquet$",
      recursive = TRUE
    ),
    0L
  )
  # ----------------------------------------------------------------------------
  # GMTED contains two rows for the same static location key.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = list_input[["aqs"]],
      output = file.path(chr_tempdir, "static"),
      static = list(gmted = list_input[["gmted_duplicate"]]),
      prefix = TRUE
    ),
    regexp = paste0(
      "Duplicate or missing normalized join keys in source: ",
      "static/gmted"
    ),
    fixed = TRUE
  )
  # ----------------------------------------------------------------------------
  # Duplicate static keys must be rejected before writing a joined dataset.
  testthat::expect_length(
    list.files(
      file.path(chr_tempdir, "static"),
      pattern = "\\.parquet$",
      recursive = TRUE
    ),
    0L
  )
})

################################################################################
testthat::test_that("join_covariates; missing normalized join keys", {
  # ----------------------------------------------------------------------------
  # Initiate temporary directory.
  chr_tempdir <- tempfile("join_covariates_missing_")
  dir.create(chr_tempdir)
  on.exit(unlink(chr_tempdir, recursive = TRUE), add = TRUE)
  # ----------------------------------------------------------------------------
  # Import fixtures with one missing key component per source.
  list_input <- list(
    aqs = testthat::test_path("..", "testdata", "join", "aqs.parquet"),
    geos_missing_site = testthat::test_path(
      "..",
      "testdata",
      "join",
      "geos_missing_site.parquet"
    ),
    geos_missing_time = testthat::test_path(
      "..",
      "testdata",
      "join",
      "geos_missing_time.parquet"
    ),
    hms_missing_site = testthat::test_path(
      "..",
      "testdata",
      "join",
      "hms_missing_site.parquet"
    ),
    hms_missing_date = testthat::test_path(
      "..",
      "testdata",
      "join",
      "hms_missing_date.parquet"
    ),
    gmted_missing_site = testthat::test_path(
      "..",
      "testdata",
      "join",
      "gmted_missing_site.parquet"
    )
  )
  # ----------------------------------------------------------------------------
  # Missing GEOS location key.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = list_input[["aqs"]],
      output = file.path(chr_tempdir, "hourly_site"),
      hourly = list(geos = list_input[["geos_missing_site"]]),
      prefix = TRUE
    ),
    regexp = "Duplicate or missing normalized join keys in source: hourly/geos",
    fixed = TRUE
  )
  # ----------------------------------------------------------------------------
  # Missing hourly location keys must not produce a joined dataset.
  testthat::expect_length(
    list.files(
      file.path(chr_tempdir, "hourly_site"),
      pattern = "\\.parquet$",
      recursive = TRUE
    ),
    0L
  )
  # ----------------------------------------------------------------------------
  # Missing GEOS timestamp key.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = list_input[["aqs"]],
      output = file.path(chr_tempdir, "hourly_time"),
      hourly = list(geos = list_input[["geos_missing_time"]]),
      prefix = TRUE
    ),
    regexp = "Duplicate or missing normalized join keys in source: hourly/geos",
    fixed = TRUE
  )
  # ----------------------------------------------------------------------------
  # Missing hourly timestamp keys must not produce a joined dataset.
  testthat::expect_length(
    list.files(
      file.path(chr_tempdir, "hourly_time"),
      pattern = "\\.parquet$",
      recursive = TRUE
    ),
    0L
  )
  # ----------------------------------------------------------------------------
  # Missing HMS location key.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = list_input[["aqs"]],
      output = file.path(chr_tempdir, "daily_site"),
      daily = list(hms = list_input[["hms_missing_site"]]),
      prefix = TRUE
    ),
    regexp = "Duplicate or missing normalized join keys in source: daily/hms",
    fixed = TRUE
  )
  # ----------------------------------------------------------------------------
  # Missing daily location keys must not produce a joined dataset.
  testthat::expect_length(
    list.files(
      file.path(chr_tempdir, "daily_site"),
      pattern = "\\.parquet$",
      recursive = TRUE
    ),
    0L
  )
  # ----------------------------------------------------------------------------
  # Missing HMS date key.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = list_input[["aqs"]],
      output = file.path(chr_tempdir, "daily_date"),
      daily = list(hms = list_input[["hms_missing_date"]]),
      prefix = TRUE
    ),
    regexp = "Duplicate or missing normalized join keys in source: daily/hms",
    fixed = TRUE
  )
  # ----------------------------------------------------------------------------
  # Missing daily date keys must not produce a joined dataset.
  testthat::expect_length(
    list.files(
      file.path(chr_tempdir, "daily_date"),
      pattern = "\\.parquet$",
      recursive = TRUE
    ),
    0L
  )
  # ----------------------------------------------------------------------------
  # Missing GMTED location key.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = list_input[["aqs"]],
      output = file.path(chr_tempdir, "static_site"),
      static = list(gmted = list_input[["gmted_missing_site"]]),
      prefix = TRUE
    ),
    regexp = paste0(
      "Duplicate or missing normalized join keys in source: ",
      "static/gmted"
    ),
    fixed = TRUE
  )
  # ----------------------------------------------------------------------------
  # Missing static location keys must not produce a joined dataset.
  testthat::expect_length(
    list.files(
      file.path(chr_tempdir, "static_site"),
      pattern = "\\.parquet$",
      recursive = TRUE
    ),
    0L
  )
})
