#' Extract 2D MERRA-2 covariates with SedonaDB
#'
#' @description
#' Read local daily MERRA-2 NetCDF files, extract one variable at prepared
#' locations for each available time slice, and write Parquet files in
#' batches. Point extraction uses raster cell values; polygon extraction
#' uses zonal statistics.
#'
#' @inheritParams sedona_geos
#'
#' @param input character(1). Non-empty path to an existing directory containing
#'   daily files from one MERRA-2 collection. Filenames must retain the
#'   `MERRA2_400.<collection>.<YYYYMMDD>.nc4` form. All listed immediate
#'   entries must be files; subdirectories are not searched.
#' @param variable character(1). Non-empty, case-sensitive NetCDF variable
#'   name, such as `"T2M"`. Supply a variable with time, latitude, and longitude
#'   dimensions whose raster bands correspond to successive time slices.
#'   Variables with a vertical dimension are unsupported; this restriction
#'   is not currently enforced by an explicit dimension check.
#' @param band integer(1). Non-missing positive integer retained for shared
#'   argument validation. Leave at the default `1L`. This argument currently
#'   does not select extraction bands or vertical levels; the function
#'   processes every time slice using internally generated band indices.
#' @param batch integer(1). Maximum number of daily source files per output
#'   Parquet file, at least `1L`. Defaults to `50L`. Batching occurs before
#'   expansion into time slices, so 50 complete files with 24 slices each
#'   produce 1,200 extraction views in one batch.
#'
#' @details
#' Locations are transformed and optionally buffered by [prepare_locs()],
#' currently using the package's GEOS CRS lookup entry.
#'
#' The time schedule is read once with [merra2_times()] from the first file
#' in the first batch requiring extraction. Timestamps are converted to
#' offsets from UTC midnight on that file's date. These offsets are then
#' added to the filename-derived date of each selected file.
#'
#' Supply complete, unmodified daily files from one collection with identical
#' time-of-day schedules and band ordering. These properties are assumed
#' across files rather than checked separately for every file.
#'
#' Time coordinates retain their native spacing and offsets. For example,
#' `tavg1_2d_slv_Nx` uses 00:30 through 23:30 UTC. The function does not
#' require exactly 24 slices, round timestamps, or interpolate missing times.
#'
#' Each batch is written as
#' `merra2_<variable>_<first_date>_<last_date>.parquet`, with dates formatted
#' as `YYYYMMDD`. Existing batch outputs are skipped and included in the
#' returned paths. Their contents are not checked against the supplied
#' locations or extraction settings; use a separate output directory when
#' those settings change.
#'
#' No time metadata are read when every batch output already exists.
#'
#' @return
#' Invisibly, a character vector of absolute Parquet file paths, including
#' existing batches that were skipped. Each file contains the location
#' identifier column, a SQL `time` timestamp representing UTC, and a value
#' column named with `sprintf("%s_%d", tolower(variable), radius)`, such as
#' `t2m_0`. No vertical `level` column is produced.
#'
#' @note
#' Multi-file filename parsing remains a limitation of the current calls to
#' [amadeus::process_collection()]. Until this is corrected, select one daily
#' file per call.
#'
#' Uses the default SedonaDB context and view names `db_locs` and
#' `db_time_<index>`. Existing views with those names may be replaced.
#'
#' @importFrom amadeus sequence_dates process_collection
#' @importFrom sf st_geometry_type
#'
#' @seealso [amadeus::download_merra2()], [merra2_times()], [sedona_geos()],
#'   [prepare_locs()], [join_variables()], [join_covariates()]
#' @examples
#' \dontrun{
#' merra2_files <- sedona_merra2(
#'   date = "2018-01-01",
#'   input = "data/merra2/tavg1_2d_slv_Nx",
#'   output = tempfile("merra2_covariates_"),
#'   variable = "T2M",
#'   locs = data.frame(site_id = "001", lon = -78.90, lat = 35.97),
#'   locs_id = "site_id",
#'   radius = 0,
#'   batch = 1L
#' )
#' }
#' @export
sedona_merra2 <- function(
  date,
  input,
  output,
  variable,
  band = 1L,
  locs,
  locs_id,
  radius = 0,
  fun = "mean",
  touches = TRUE,
  exclude_nodata = TRUE,
  batch = 50L
) {
  # ----------------------------------------------------------------------------
  # Prepare extraction locations (internal checks for arguments).
  sf_locs <- amadeusdb::prepare_locs(
    locs = locs,
    locs_id = locs_id,
    crs = .amadeusdb$list_crs$geos$crs, # !!! replace with MERRA2 !!!
    radius = radius
  )
  # ----------------------------------------------------------------------------
  # Extract `sf` geometry type.
  chr_locsgeom <- as.character(unique(sf::st_geometry_type(sf_locs)))
  # ----------------------------------------------------------------------------
  # Coerce `date` to vector of 2 if not already.
  date <- amadeusdb::fl_dates(amadeus::sequence_dates(date, hyphen = TRUE))
  # ----------------------------------------------------------------------------
  # Check zonal statistic function against SedonaDB supported (as of 10/21/26)
  fun <- match.arg(
    tolower(fun),
    c(
      "mean",
      "sum",
      "min",
      "max",
      "median",
      "count",
      "stddev",
      "variance",
      "mode"
    )
  )
  # ----------------------------------------------------------------------------
  # Check arguments.
  amadeusdb::check_arguments(
    locs = sf_locs,
    date = date,
    input = input,
    output = output,
    batch = batch,
    raster = TRUE,
    fun = fun,
    variable = variable,
    band = band,
    touches = touches,
    exclude_nodata = exclude_nodata
  )
  # ----------------------------------------------------------------------------
  # Identify time-relevant MERRA2 file paths in `input`.
  chr_datapaths <- amadeusdb::discover(
    date = date,
    input = input,
    pattern = "MERRA2_400.*.nc4",
    hyphen = FALSE
  )
  # ----------------------------------------------------------------------------
  # Store file path, NetCDF variable path, date, and generated db names.
  df_merra2paths <- data.frame(
    path = chr_datapaths,
    tchar = vapply(
      chr_datapaths,
      amadeus::process_collection,
      FUN.VALUE = character(1),
      source = "merra2",
      date = TRUE,
      USE.NAMES = FALSE
    )
  )
  df_merra2paths$time <- as.character(as.POSIXct(
    as.character(df_merra2paths$tchar),
    format = "%Y%m%d",
    tz = "UTC"
  ))
  df_merra2paths$var <- sprintf(
    'NETCDF:"%s":%s',
    normalizePath(df_merra2paths$path, winslash = "/", mustWork = TRUE),
    variable
  )
  df_merra2paths$view <- sprintf("db_merra2_%08s", df_merra2paths$tchar)
  # ----------------------------------------------------------------------------
  # Require one collection before reusing its time schedule across files.
  chr_merra2_collection <- unique(vapply(
    df_merra2paths$path,
    amadeus::process_collection,
    FUN.VALUE = character(1),
    source = "merra2",
    collection = TRUE,
    USE.NAMES = FALSE
  ))
  if (length(chr_merra2_collection) != 1L) {
    stop(
      "Selected MERRA-2 files must belong to a single collection.\n",
      "Found: ",
      paste(chr_merra2_collection, collapse = ", "),
      ".\n",
      "Provide files from only one collection per call.",
      call. = FALSE
    )
  }
  # ----------------------------------------------------------------------------
  # Check dates, paths, and database view names.
  amadeusdb::check_views(df_merra2paths)
  # ----------------------------------------------------------------------------
  # Split all source rows before checking outputs to preserve batch membership.
  list_merra2_batch <- amadeusdb::batch_split(df_merra2paths, batch)
  # ----------------------------------------------------------------------------
  # Generate .parquet paths for every batch, including existing outputs.
  chr_merra2_parquet <- amadeusdb::batch_prep(
    df = df_merra2paths,
    batch = list_merra2_batch,
    output = output,
    source = "merra2",
    variable = variable,
    time_id = "tchar"
  )
  # ----------------------------------------------------------------------------
  # Check outputs before reading NetCDF contents or registering database views.
  lgl_merra2_skip <- vapply(
    chr_merra2_parquet,
    amadeusdb::batch_skip,
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
  if (all(lgl_merra2_skip)) {
    return(invisible(chr_merra2_parquet))
  }
  # ----------------------------------------------------------------------------
  # Read the shared coordinate once, using the first file that needs extraction.
  int_merra2_pending <- which(!lgl_merra2_skip)
  int_merra2_first <- list_merra2_batch[[int_merra2_pending[1L]]][1L]
  # ----------------------------------------------------------------------------
  # Read the time schedule from the first file requiring extraction.
  time_merra2_reference <- merra2_times(
    df_merra2paths$path[int_merra2_first]
  )
  # ----------------------------------------------------------------------------
  # Identify midnight on that file's date.
  time_merra2_midnight <- as.POSIXct(
    df_merra2paths$tchar[int_merra2_first],
    format = "%Y%m%d",
    tz = "UTC"
  )
  # ----------------------------------------------------------------------------
  # Cache the time offsets in seconds from midnight.
  num_merra2_offsets <- as.numeric(difftime(
    time_merra2_reference,
    time_merra2_midnight,
    units = "secs"
  ))
  # ----------------------------------------------------------------------------
  # Label every extraction with the coordinate of the requested raster band.
  # checkmate::assert_integer(band, upper = length(num_merra2_levels))
  # chr_merra2_level <- as.character(num_merra2_levels[band])
  # ----------------------------------------------------------------------------
  # Define location identifiers in SQL-friendly format.
  locs_id_sql <- amadeusdb::quote_sql(locs_id)
  # ----------------------------------------------------------------------------
  # Define variable identifiers in SQL-friendly format.
  variable_sql <- amadeusdb::quote_sql(
    sprintf("%s_%d", toupper(variable), radius)
  )
  # ----------------------------------------------------------------------------
  # Define SQL code for processing MERRA2 data for points or polygons.
  sql_extraction <- if (chr_locsgeom == "POINT") {
    "RS_Value(r.raster, db_locs.geometry, $3)"
  } else {
    "RS_ZonalStats(r.raster, db_locs.geometry, $3, $4, ($5 = 1), ($6 = 1))"
  }
  # ----------------------------------------------------------------------------
  # Define full SQL code including point/polygon distinction.
  sql_processmerra2 <- amadeusdb::glue_sql(
    file = "sedona_merra2.sql",
    args = list(
      locs_id_sql = locs_id_sql,
      variable_sql = variable_sql,
      sql_extraction = sql_extraction
    )
  )
  # ----------------------------------------------------------------------------
  # Initiate database cleanup upon exit.
  chr_merra2_views <- character(0)
  on.exit(amadeusdb::cleanup_views(chr_merra2_views), add = TRUE)
  # ----------------------------------------------------------------------------
  # Register the extraction point data object in the SedonaDB session ! once !.
  amadeusdb::register_locs(sf_locs)
  chr_merra2_views <- c(chr_merra2_views, "db_locs")
  # ----------------------------------------------------------------------------
  # Perform spatial extraction within missing batches using original indices.
  for (l in int_merra2_pending) {
    # --------------------------------------------------------------------------
    # Index data files, times, and names to active batch.
    df_merra2_batch <- df_merra2paths[list_merra2_batch[[l]], , drop = FALSE]
    # --------------------------------------------------------------------------
    # Expand each daily file into its individual time slices.
    df_merra2_hourly <- do.call(
      rbind,
      lapply(
        seq_len(nrow(df_merra2_batch)),
        function(i) {
          # --------------------------------------------------------------------
          # Reuse the collection's time schedule for this file's date.
          time_merra2_midnight <- as.POSIXct(
            df_merra2_batch$tchar[i],
            format = "%Y%m%d",
            tz = "UTC"
          )
          time_merra2 <- time_merra2_midnight + num_merra2_offsets
          # --------------------------------------------------------------------
          # Pair each timestamp with its raster band.
          data.frame(
            path = df_merra2_batch$path[i],
            var = df_merra2_batch$var[i],
            time = format(
              time_merra2,
              format = "%Y-%m-%d %H:%M:%S",
              tz = "UTC"
            ),
            raster_band = seq_along(time_merra2)
          )
        }
      )
    )
    # --------------------------------------------------------------------------
    # Generate one extraction view name per time slice in the batch.
    chr_merra2_hourly <- sprintf(
      "db_time_%03d",
      seq_len(nrow(df_merra2_hourly))
    )
    # --------------------------------------------------------------------------
    message(variable, ": batch ", l, "/", length(list_merra2_batch))
    for (i in seq_len(nrow(df_merra2_hourly))) {
      # ------------------------------------------------------------------------
      # Supply the source variable, timestamp, and band for this time slice.
      list_params <- list(
        # $1: NetCDF source for the requested variable
        as.character(df_merra2_hourly$var[i]),
        # $2: timestamp corresponding to this raster band
        df_merra2_hourly$time[i],
        # $3: raster band corresponding to this time slice
        as.integer(df_merra2_hourly$raster_band[i])
      )
      # ------------------------------------------------------------------------
      # Include additional arguments for polygon zonal statistics.
      if (!chr_locsgeom == "POINT") {
        list_params <- c(
          list_params,
          list(
            # $4: summary statistic
            fun,
            # $5: summarize based on all touched cells (TRUE/FALSE)
            as.integer(touches),
            # $6: exclude no data (TRUE/FALSE)
            as.integer(exclude_nodata)
          )
        )
      }
      # ------------------------------------------------------------------------
      # Compute the spatial join.
      chr_merra2_views <- amadeusdb::compute_view(
        sql = sql_processmerra2,
        params = list_params,
        view = chr_merra2_hourly[i],
        all = chr_merra2_views
      )
    }
    # --------------------------------------------------------------------------
    # Write batch .parquet.
    amadeusdb::batch_write(
      locs_id = locs_id_sql,
      time_id = "time",
      columns = variable_sql,
      views = chr_merra2_hourly,
      parquet = chr_merra2_parquet[l],
      single = TRUE
    )
    # --------------------------------------------------------------------------
    # Remove hourly views now represented by the registered batch.
    chr_merra2_views <- amadeusdb::batch_drop(
      chr_merra2_views,
      chr_merra2_hourly
    )
  }
  # ----------------------------------------------------------------------------
  # Return parquet file out paths.
  invisible(chr_merra2_parquet)
}

################################################################################
#' Read MERRA-2 timestamps from a NetCDF file
#'
#' @description
#' Decode the file's numeric `time` coordinate using its units and reference
#' timestamp, preserving the time slices in their stored order.
#'
#' @param path character(1). Path to a MERRA-2 NetCDF file containing a `time`
#'   coordinate with units `"minutes since YYYY-MM-DD HH:MM:SS"`.
#'
#' @details
#' The reference timestamp is interpreted as UTC. Missing calendar metadata
#' and the calendars `"standard"`, `"gregorian"`, and `"proleptic_gregorian"`
#' are supported. Other calendars, unsupported units, invalid reference
#' timestamps, and missing or non-finite time offsets raise an error.
#'
#' The file is closed on exit, including when decoding fails. Only time
#' metadata are read; raster values are not loaded. The helper does not
#' require 24 slices or hourly spacing and does not round timestamps.
#'
#' @return A `POSIXct` vector in UTC, with one timestamp per time coordinate.
#' @importFrom checkmate assert_numeric
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @seealso [sedona_merra2()], [ncdf4::ncvar_get()]
#' @keywords internal
#' @export
merra2_times <- function(path) {
  # ----------------------------------------------------------------------------
  # Close the file even if reading or decoding its metadata fails.
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  # ----------------------------------------------------------------------------
  # Read the time offsets and their reference timestamp.
  num_time <- as.numeric(ncdf4::ncvar_get(nc, "time"))
  list_units <- ncdf4::ncatt_get(nc, "time", "units")
  if (
    !isTRUE(list_units$hasatt) ||
      !grepl("^minutes since ", list_units$value)
  ) {
    stop("MERRA-2 time units must be 'minutes since YYYY-MM-DD HH:MM:SS'.")
  }
  # ----------------------------------------------------------------------------
  # POSIXct cannot represent dates in non-Gregorian model calendars.
  list_calendar <- ncdf4::ncatt_get(nc, "time", "calendar")
  if (
    isTRUE(list_calendar$hasatt) &&
      !list_calendar$value %in%
        c("standard", "gregorian", "proleptic_gregorian")
  ) {
    stop("Unsupported MERRA-2 time calendar: ", list_calendar$value)
  }
  # ----------------------------------------------------------------------------
  # Require an unambiguous UTC reference timestamp in the supported format.
  chr_origin <- sub("^minutes since ", "", list_units$value)
  time_origin <- as.POSIXct(
    chr_origin,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
  if (
    is.na(time_origin) ||
      format(time_origin, "%Y-%m-%d %H:%M:%S", tz = "UTC") != chr_origin
  ) {
    stop("Invalid MERRA-2 reference timestamp: ", chr_origin)
  }
  # ----------------------------------------------------------------------------
  # Convert minute offsets to seconds without changing their order or spacing.
  checkmate::assert_numeric(
    num_time,
    min.len = 1L,
    any.missing = FALSE,
    finite = TRUE
  )
  time_origin + num_time * 60
}

################################################################################
