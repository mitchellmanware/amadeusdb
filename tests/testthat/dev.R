################################################################################
# Failure cases.
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
      "..", "testdata", "join", "geos_duplicate.parquet"
    ),
    hms_duplicate = testthat::test_path(
      "..", "testdata", "join", "hms_duplicate.parquet"
    ),
    gmted_duplicate = testthat::test_path(
      "..", "testdata", "join", "gmted_duplicate.parquet"
    )
  )
  # ----------------------------------------------------------------------------
  # GEOS times at 00:30 and 00:55 in separate files share a normalized hour.
  testthat::expect_error(
    amadeusdb::join_covariates(
      input = list_input[["aqs"]],
      output = file.path(chr_tempdir, "hourly"),
      hourly = list(geos = c(
        list_input[["geos"]], list_input[["geos_duplicate"]]
      )),
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
      "..", "testdata", "join", "geos_missing_site.parquet"
    ),
    geos_missing_time = testthat::test_path(
      "..", "testdata", "join", "geos_missing_time.parquet"
    ),
    hms_missing_site = testthat::test_path(
      "..", "testdata", "join", "hms_missing_site.parquet"
    ),
    hms_missing_date = testthat::test_path(
      "..", "testdata", "join", "hms_missing_date.parquet"
    ),
    gmted_missing_site = testthat::test_path(
      "..", "testdata", "join", "gmted_missing_site.parquet"
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
