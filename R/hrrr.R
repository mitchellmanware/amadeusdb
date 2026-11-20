#' Download NOAA High-Resolution Rapid Refresh (HRRR) data
#'
#' @description
#' Downloads NOAA High-Resolution Rapid Refresh (HRRR) model data from the
#' NOAA HRRR public Amazon S3 archive.
#'
#' The function supports the HRRR three-dimensional pressure-level,
#' native-level, two-dimensional surface, and sub-hourly products. Multiple
#' dates, model cycle run times, and forecast hours can be requested in a
#' single call.
#'
#' HRRR data are downloaded as GRIB2 files and organized under a sector-specific
#' directory within `directory_to_save`.
#'
#' @param product character(1). HRRR product to download. Accepted values are
#'   `"3d pressure"`, `"native"`, `"2d surface"`, `"sub-hourly"`, or
#'   `"subhourly"`. Matching is case-insensitive.
#' @param sector character(1). HRRR geographic sector to download. Currently
#'   only `"conus"` is supported. Matching is case-insensitive.
#' @param date character(1 or 2). Dates in `"YYYY-MM-DD"` format defining
#'   the download period. A single date downloads data for that date.
#'   When two dates are supplied, all dates from the earlier date through the
#'   later date are included. All dates must be between `"2014-07-30"` and
#'   today, inclusive. Dates outside this range raise an error before directory
#'   creation or network requests.
#' @param cycle_runtime numeric. Model initialization hour(s), specified as
#'   integers from `0` through `23`. Values are formatted as two-digit UTC
#'   cycle times when constructing the HRRR file names. At least one hour is
#'   required; repeated hours are requested once.
#' @param forecast_hour numeric. Forecast lead hour(s), specified as integers
#'   from `1` through `48`. Values are formatted as two-digit forecast hours
#'   when constructing the HRRR file names. At least one hour is required;
#'   repeated hours are requested once.
#' @param directory_to_save character(1). Directory in which downloaded HRRR
#'   files are stored. A sector-specific subdirectory is created automatically.
#' @param acknowledgement logical(1). Must be `TRUE` to acknowledge the
#'   download before data are retrieved.
#' @inheritParams amadeus::download_run show_progress
#' @param hash logical(1). Whether to return a hash of the downloaded data
#'   directory instead of the download results.
#' @inheritParams amadeus::download_run max_tries rate_limit
#'
#' @return
#' If `hash = FALSE`, an invisible summary from [amadeus::download_run()]
#' containing `success`, `failed`, `skipped`, `failed_urls`, and `failed_files`.
#' The two failure vectors are `character(0)` when no downloads failed,
#' including when every file was skipped. If `hash = TRUE`, a visible character
#' scalar containing the combined MD5 hash of all files recursively under
#' `directory_to_save`, including existing files.
#'
#' @examples
#' \dontrun{
#' download_hrrr(
#'   product = "2d surface",
#'   sector = "conus",
#'   date = "2020-01-01",
#'   cycle_runtime = 12,
#'   forecast_hour = 1:18,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#'
#' download_hrrr(
#'   product = "sub-hourly",
#'   date = c("2020-01-01", "2020-01-03"),
#'   cycle_runtime = 0:23,
#'   forecast_hour = 1:6,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#'
#' @inheritSection amadeus::download_run Concurrent downloads
#' @export
download_hrrr <- function(
  product = NULL,
  sector = "conus",
  date = c("2018-01-01", "2018-01-01"),
  cycle_runtime = 0:23,
  forecast_hour = 1:18,
  directory_to_save = NULL,
  acknowledgement = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "hrrr",
    acknowledgement = acknowledgement,
    download = TRUE,
    remove_command = FALSE,
    unzip = FALSE,
    remove_zip = FALSE,
    directory_to_save = directory_to_save,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit,
    hash = hash
  )
  # ----------------------------------------------------------------------------
  # Check source-specific arguments.
  checkmate::assert_string(product, min.chars = 1L)
  chr_product <- tolower(product)
  checkmate::assert_choice(
    chr_product,
    c("3d pressure", "native", "2d surface", "sub-hourly", "subhourly")
  )
  checkmate::assert_string(sector, min.chars = 1L)
  chr_sector <- tolower(sector)
  checkmate::assert_choice(chr_sector, "conus")
  checkmate::assert_integerish(
    cycle_runtime,
    lower = 0L,
    upper = 23L,
    min.len = 1L,
    any.missing = FALSE
  )
  checkmate::assert_integerish(
    forecast_hour,
    lower = 1L,
    upper = 48L,
    min.len = 1L,
    any.missing = FALSE
  )
  # ----------------------------------------------------------------------------
  # Check `date` and generate sequence.
  chr_dates <- amadeus::sequence_dates(date, min = "2014-07-30", hyphen = FALSE)
  # ----------------------------------------------------------------------------
  # Generate saving and sector-specific directories.
  chr_dir_to_save <- amadeus::download_setup_dir(directory_to_save)
  chr_dir_sector <- amadeus::download_setup_dir(
    file.path(chr_dir_to_save, chr_sector)
  )
  # ----------------------------------------------------------------------------
  # Switch product name to product code, including the sub-hourly alias.
  chr_product_code <- switch(
    gsub("-", "", chr_product, fixed = TRUE),
    "3d pressure" = "prs",
    "native" = "nat",
    "2d surface" = "sfc",
    "subhourly" = "subh"
  )
  # ----------------------------------------------------------------------------
  # Format distinct cycle run times and forecast hours.
  chr_cc <- sprintf("%02d", unique(as.integer(cycle_runtime)))
  chr_fh <- sprintf("%02d", unique(as.integer(forecast_hour)))
  # ----------------------------------------------------------------------------
  # Generate download URLs and destination files.
  df_download <- expand.grid(
    base = "https://noaa-hrrr-bdp-pds.s3.amazonaws.com/hrrr.",
    date = chr_dates,
    sector = chr_sector,
    cc = chr_cc,
    product_code = chr_product_code,
    fh = chr_fh,
    dir = chr_dir_sector,
    stringsAsFactors = FALSE
  )
  df_download$file <- with(
    df_download,
    paste0("hrrr.t", cc, "z.wrf", product_code, "f", fh, ".grib2")
  )
  df_download$url <- with(
    df_download,
    paste0(base, date, "/", sector, "/", file)
  )
  df_download$destfile <- with(
    df_download,
    file.path(
      dir,
      paste0(
        "hrrr.",
        date,
        ".t",
        cc,
        "z.wrf",
        product_code,
        "f",
        fh,
        ".grib2"
      )
    )
  )
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus::download_run(
    urls = df_download$url,
    destfiles = df_download$destfile,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  amadeus:::download_return(download_result, hash, chr_dir_to_save)
}
