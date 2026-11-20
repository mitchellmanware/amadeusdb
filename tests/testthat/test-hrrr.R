################################################################################
##### unit and integration tests for NOAA HRRR functions

################################################################################
testthat::test_that("download_hrrr (single date)", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/hrrr/")

  # no errors, no warnings
  testthat::expect_no_error(
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = 0L,
      forecast_hour = 1L,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    )
  )

  # Check that directory was created
  testthat::expect_true(dir.exists(directory_to_save))
  files <- list.files(directory_to_save, recursive = TRUE, full.names = TRUE)
  testthat::expect_gt(length(files), 0)
  testthat::expect_gt(sum(file.info(files)$size > 0), 0)

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_hrrr (date range)", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/hrrr/")

  # Expect deprecation warning
  testthat::expect_no_error(
    download_hrrr(
      date = c("2021-05-04", "2021-05-05"),
      product = "2d surface",
      sector = "conus",
      cycle_runtime = 0L,
      forecast_hour = 1L,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    )
  )

  # Check that directory was created
  testthat::expect_true(dir.exists(directory_to_save))
  files <- list.files(directory_to_save, recursive = TRUE, full.names = TRUE)
  testthat::expect_gt(length(files), 0)
  testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_hrrr (expected errors)", {
  # input year instead of date
  testthat::expect_error(
    download_hrrr(
      date = "2021",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = 0:4L,
      forecast_hour = 1:5L,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "chr_dates.*exactly 10 characters"
  )
  # invalid cycle runtime (out of bounds)
  testthat::expect_error(
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = 25L,
      forecast_hour = 1:5L,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "cycle_runtime.*<= 23"
  )
  # invalid cycle runtime (character)
  testthat::expect_error(
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = "four",
      forecast_hour = 1:5L,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "cycle_runtime.*type 'integerish'"
  )

  # invalid forecast hour (out of bounds)
  testthat::expect_error(
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = 4L,
      forecast_hour = 500L,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "forecast_hour.*<= 48"
  )

  # invalid forecast hour (character)
  testthat::expect_error(
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = 4L,
      forecast_hour = "five",
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "forecast_hour.*type 'integerish'"
  )

  # unknown product
  testthat::expect_error(
    download_hrrr(
      date = "2021-05-04",
      product = "4d spacetime",
      sector = "conus",
      cycle_runtime = 4L,
      forecast_hour = 5L,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "chr_product.*element of set"
  )

  # unknown sector
  testthat::expect_error(
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "europe",
      cycle_runtime = 4L,
      forecast_hour = 5L,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "chr_sector.*element of set"
  )
})

testthat::test_that("download_hrrr mock download with hash", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_hrrr(
          date = "2021-05-04",
          product = "2d surface",
          sector = "conus",
          cycle_runtime = 0L,
          forecast_hour = 1L,
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

################################################################################
# Live network tests for download_hrrr(). Mocked tests: test-hrrr.R.
################################################################################
testthat::test_that(
  paste0(
    "download_hrrr(product = 'surface', date = '2021-04-05')): ",
    "downloads non-empty file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = 0L,
      forecast_hour = 1L,
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_hrrr(product = '3d pressure', date = '2021-04-05')): ",
    "downloads monolevel snow water equivalent file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = 0L,
      forecast_hour = 1L,
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_hrrr(product = 'native', date = '2021-04-05')): ",
    "downloads pressure-level files"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    download_hrrr(
      date = "2021-05-04",
      product = "native",
      sector = "conus",
      cycle_runtime = 0L,
      forecast_hour = 1L,
      date = "2021-04-05",
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
