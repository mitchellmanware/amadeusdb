################################################################################
# Small NetCDF fixtures keep timestamp tests independent of downloaded rasters.
merra2_time_fixture <- function(units, offsets = c(0, 60), calendar = NA) {
  path <- tempfile(fileext = ".nc")
  time <- ncdf4::ncdim_def("time", units, offsets, calendar = calendar)
  value <- ncdf4::ncvar_def("value", "1", list(time), missval = -9999)
  nc <- ncdf4::nc_create(path, value)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  path
}

testthat::test_that("merra2_times preserves all 24 half-hour timestamps", {
  path <- merra2_time_fixture(
    "minutes since 2018-01-01 00:30:00",
    offsets = seq(0, 1380, by = 60)
  )
  expected <- as.POSIXct(
    sprintf("2018-01-01 %02d:30:00", 0:23),
    tz = "UTC"
  )
  testthat::expect_identical(merra2_times(path), expected)
})

testthat::test_that("merra2_times uses each file's origin and actual offsets", {
  first <- merra2_time_fixture("minutes since 2018-01-01 00:30:00")
  second <- merra2_time_fixture(
    "minutes since 2018-01-02 23:30:00",
    offsets = c(0, 120),
    calendar = "standard"
  )
  testthat::expect_identical(
    merra2_times(first),
    as.POSIXct(c("2018-01-01 00:30:00", "2018-01-01 01:30:00"), tz = "UTC")
  )
  testthat::expect_identical(
    merra2_times(second),
    as.POSIXct(c("2018-01-02 23:30:00", "2018-01-03 01:30:00"), tz = "UTC")
  )
})

testthat::test_that("merra2_times rejects metadata it cannot decode", {
  units <- merra2_time_fixture("hours since 2018-01-01 00:30:00")
  origin <- merra2_time_fixture("minutes since 2018-02-30 00:30:00")
  calendar <- merra2_time_fixture(
    "minutes since 2018-01-01 00:30:00",
    calendar = "360_day"
  )
  testthat::expect_error(merra2_times(units), "MERRA-2 time units")
  testthat::expect_error(merra2_times(origin), "Invalid MERRA-2 reference timestamp")
  testthat::expect_error(merra2_times(calendar), "Unsupported MERRA-2 time calendar")
})
