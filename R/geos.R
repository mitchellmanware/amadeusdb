################################################################################
#' Extract GEOS-CF covariates with SedonaDB
#'
#' @description
#' Read local NASA GEOS-CF NetCDF files, extract one variable at prepared
#' locations, and write the results to Parquet files in batches. Locations are
#' transformed and optionally buffered with [prepare_locs()]. Point extraction
#' uses raster cell values; polygon extraction uses zonal statistics.
#'
#' @param date character(1) or character(2). Inclusive start and end dates in
#'   chronological order, formatted as `"YYYY-MM-DD"`. A single date is
#'   repeated to process one day.
#' @param input character(1). Non-empty path to an existing directory containing
#'   GEOS-CF files with their original `GEOS-CF.v01.rpl` filenames. File
#'   discovery is recursive, but the current input check requires all listed
#'   immediate entries to be files. Supply files from one collection with the
#'   same vertical coordinate and no repeated filename timestamps.
#' @param output character(1). Non-empty directory path for the output Parquet
#'   files. Created recursively if it does not exist.
#' @param variable character(1). Non-empty, case-sensitive NetCDF variable name.
#'   The same variable is read from every selected file. The output value
#'   column is named with `sprintf("%s_%d", tolower(variable), radius)`, for
#'   example `co_0` for `variable = "CO"` and `radius = 0`.
#' @param band integer(1). Non-missing, one-based raster band index for both
#'   point and polygon extraction, at least `1L`. Defaults to `1L`. The index
#'   must not exceed the number of NetCDF `lev` coordinates when processing
#'   missing batches. The output `level` is the coordinate at this index;
#'   for surface collections, band 1 corresponds to model layer 72.
#' @param locs sf, SpatVector, or data.frame object. Extraction locations with
#'   an identifier column named by `locs_id`. Spatial objects must have a source
#'   CRS; data frames must contain numeric `lon` and `lat` columns in longitude
#'   and latitude degrees. [prepare_locs()] transforms and optionally buffers
#'   locations using the package's GEOS CRS. The prepared object must have a
#'   column named `geometry` and one geometry type throughout: `POINT`,
#'   `POLYGON`, or `MULTIPOLYGON`.
#' @param locs_id character(1). Non-empty name of an existing location
#'   identifier column in `locs`. Supply unique, non-missing identifiers to
#'   obtain one row per location and timestamp; identifier values are not
#'   validated.
#' @param radius numeric(1). Non-missing, non-negative buffer distance passed to
#'   [prepare_locs()]. Distance units follow [sf::st_buffer()] for the target
#'   CRS. Defaults to `0`, which skips buffering.
#' @param fun character(1). Polygon summary statistic: `"mean"`, `"sum"`,
#'   `"min"`, `"max"`, `"median"`, `"count"`, `"stddev"`, `"variance"`, or
#'   `"mode"`. Matching ignores case and accepts unambiguous abbreviations.
#'   Defaults to `"mean"`. Validated but unused for point extraction.
#' @param touches logical(1). Non-missing flag indicating whether polygon
#'   extraction includes all raster cells touched by the geometry. Passed to
#'   `RS_ZonalStats()`; validated but unused for point extraction. Defaults to
#'   `TRUE`.
#' @param exclude_nodata logical(1). Non-missing flag indicating whether to
#'   exclude raster NoData cells from polygon statistics. Passed to
#'   `RS_ZonalStats()`; validated but unused for point extraction. Defaults to
#'   `TRUE`.
#' @param batch integer(1). Number of source files to process per output file,
#'   at least `1L`. Supply an integer such as `50L`, not a double such as `50`.
#'   Defaults to `50L`.
#'
#' @details
#' Files are selected using the requested dates, and timestamps are derived
#' from their filenames and parsed as UTC. Both point extraction and polygon
#' statistics use `band` of the requested NetCDF variable. Selected filenames
#' must identify one collection. Its vertical coordinate is assumed to be
#' identical across files and is read once with [geoslev()] from the first
#' file in the first missing batch. The coordinate at `band` supplies the
#' output `level` for every processed file. No coordinate is read when all
#' batch outputs already exist.
#'
#' Each batch is written as
#' `geos_<variable>_<first_timestamp>_<last_timestamp>.parquet`. A batch whose
#' output file already exists is skipped, and its path is included in the
#' return value. Existing files are not checked against the supplied locations
#' or extraction settings; use a separate output directory when these change.
#'
#' @return
#' Invisibly, a character vector of absolute Parquet file paths, including
#' existing batches that were skipped. Each file contains the location
#' identifier column, a `time` column stored as a SQL timestamp and derived
#' from the source filenames, a character `level` column, and a value column
#' named with the lowercase `variable` and `radius`, such as `co_0`.
#'
#' @note
#' Uses the default SedonaDB context and view names `db_locs` and
#' `db_time_<index>`. Existing views with those names may be replaced.
#'
#' @importFrom amadeus generate_date_sequence process_collection
#' @importFrom glue glue
#' @importFrom sedonadb as_sedonadb_dataframe sd_compute sd_drop_view sd_sql
#'   sd_to_view sd_write_parquet
#' @importFrom sf st_geometry_type
#'
#' @seealso [amadeus::download_geos()], [prepare_locs()],
#'   \code{\link{check_arguments}()}, [join_variables()], [join_covariates()]
#' @examples
#' \dontrun{
#' locs <- sf::st_as_sf(
#'   data.frame(site_id = "001", lon = -78.90, lat = 35.97),
#'   coords = c("lon", "lat"),
#'   crs = 4326
#' )
#' geos_files <- sedona_geos(
#'   date = c("2024-01-01", "2024-01-02"),
#'   input = "data/geos",
#'   output = tempdir(),
#'   variable = "CO",
#'   band = 1L,
#'   locs = locs,
#'   locs_id = "site_id",
#'   radius = 0,
#'   batch = 24L
#' )
#' }
#' @export
sedona_geos <- function(
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
    crs = .amadeusdb$list_crs$geos$crs,
    radius = radius
  )
  # ----------------------------------------------------------------------------
  # Extract `sf` geometry type.
  chr_locsgeom <- as.character(unique(sf::st_geometry_type(sf_locs)))
  # ----------------------------------------------------------------------------
  # Coerce `date` to vector of 2 if not already.
  date <- if (length(date) == 2) date else rep(date, 2)
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
  # Identify time-relevant GEOS file paths in `input`.
  chr_datapaths <- amadeusdb::discover(
    date = date,
    input = input,
    pattern = "GEOS-CF.v01.rpl",
    sub_hyphen = TRUE
  )
  # ----------------------------------------------------------------------------
  # Store file path, NetCDF variable path, date, and generated db names.
  df_geospaths <- data.frame(
    path = chr_datapaths,
    tchar = amadeus::process_collection(
      chr_datapaths,
      source = "geos",
      datetime = TRUE
    )
  )
  df_geospaths$time <- as.character(as.POSIXct(
    as.character(df_geospaths$tchar),
    format = "%Y%m%d%H%M",
    tz = "UTC"
  ))
  df_geospaths$var <- sprintf(
    'NETCDF:"%s":%s',
    normalizePath(df_geospaths$path, winslash = "/", mustWork = TRUE),
    variable
  )
  df_geospaths$view <- sprintf("db_geos_%012s", df_geospaths$tchar)
  # ----------------------------------------------------------------------------
  # Require one collection before reusing its vertical coordinate across files.
  chr_geos_collection <- unique(amadeus::process_collection(
    df_geospaths$path,
    source = "geos",
    collection = TRUE
  ))
  if (length(chr_geos_collection) != 1L) {
    stop("Selected GEOS-CF files must belong to a single collection.")
  }
  # ----------------------------------------------------------------------------
  # Check dates, paths, and database view names.
  amadeusdb::check_views(df_geospaths)
  # ----------------------------------------------------------------------------
  # Split all source rows before checking outputs to preserve batch membership.
  list_geos_batch <- amadeusdb::batch_split(df_geospaths, batch)
  # ----------------------------------------------------------------------------
  # Generate .parquet paths for every batch, including existing outputs.
  chr_geos_parquet <- amadeusdb::batch_prep(
    df = df_geospaths,
    batch = list_geos_batch,
    output = output,
    source = "geos",
    variable = variable,
    time_id = "tchar"
  )
  # ----------------------------------------------------------------------------
  # Check outputs before reading NetCDF contents or registering database views.
  lgl_geos_skip <- vapply(
    chr_geos_parquet,
    amadeusdb::batch_skip,
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
  if (all(lgl_geos_skip)) {
    return(invisible(chr_geos_parquet))
  }
  # ----------------------------------------------------------------------------
  # Read the shared coordinate once, using the first file that needs extraction.
  int_geos_pending <- which(!lgl_geos_skip)
  int_geos_first <- list_geos_batch[[int_geos_pending[1L]]][1L]
  num_geos_levels <- amadeusdb::geoslev(
    df_geospaths$path[int_geos_first],
    "lev"
  )
  # ----------------------------------------------------------------------------
  # Label every extraction with the coordinate of the requested raster band.
  checkmate::assert_integer(band, upper = length(num_geos_levels))
  chr_geos_level <- as.character(num_geos_levels[band])
  # ----------------------------------------------------------------------------
  # Define location identifiers in SQL-friendly format.
  locs_id_sql <- amadeusdb::quote_sql(locs_id)
  # ----------------------------------------------------------------------------
  # Define variable identifiers in SQL-friendly format.
  variable_sql <- amadeusdb::quote_sql(
    sprintf("%s_%d", tolower(variable), radius)
  )
  # ----------------------------------------------------------------------------
  # Define SQL code for processing GEOS data for points or polygons.
  sql_extraction <- if (chr_locsgeom == "POINT") {
    "RS_Value(r.raster, db_locs.geometry, $4)"
  } else {
    "RS_ZonalStats(r.raster, db_locs.geometry, $4, $5, ($6 = 1), ($7 = 1))"
  }
  # ----------------------------------------------------------------------------
  # Define full SQL code including point/polygon distinction.
  sql_processgeos <- amadeusdb::glue_sql(
    file = "sedona_geos.sql",
    args = list(
      locs_id_sql = locs_id_sql,
      variable_sql = variable_sql,
      sql_extraction = sql_extraction
    )
  )
  # ----------------------------------------------------------------------------
  # Initiate database cleanup upon exit.
  chr_geos_views <- character(0)
  on.exit(amadeusdb::cleanup_views(chr_geos_views), add = TRUE)
  # ----------------------------------------------------------------------------
  # Register the extraction point data object in the SedonaDB session ! once !.
  amadeusdb::register_locs(sf_locs)
  chr_geos_views <- c(chr_geos_views, "db_locs")
  # ----------------------------------------------------------------------------
  # Perform spatial extraction within missing batches using original indices.
  for (l in int_geos_pending) {
    # --------------------------------------------------------------------------
    # Index data files, times, and names to active batch.
    df_geos_batch <- df_geospaths[list_geos_batch[[l]], , drop = FALSE]
    # --------------------------------------------------------------------------
    # Generate daily view names for active batch.
    chr_geos_hourly <- sprintf("db_time_%03d", seq_len(nrow(df_geos_batch)))
    # --------------------------------------------------------------------------
    message(variable, ": batch ", l, "/", length(list_geos_batch))
    for (i in seq_len(nrow(df_geos_batch))) {
      # ------------------------------------------------------------------------
      # Time extraction directly from NetCDF not supported (as of 10/21/26)
      list_params <- list(
        # $1: raster source
        as.character(df_geos_batch$var[i]),
        # $2: timestamp
        df_geos_batch$time[i],
        # $3: vertical depth/pressure level (for `lev` column output)
        chr_geos_level,
        # $4: vertical band to extract from (default = 1L)
        as.integer(band)
      )
      # ------------------------------------------------------------------------
      # Include additional arguments for polygon zonal statistics.
      if (!chr_locsgeom == "POINT") {
        list_params <- c(
          list_params,
          list(
            # $5: summary statistic
            fun, # $5: statistic
            # $6: summarize based on all touched cells (TRUE/FALSE)
            as.integer(touches),
            # $7: exclude no data (TRUE/FALSE)
            as.integer(exclude_nodata)
          )
        )
      }
      # ------------------------------------------------------------------------
      # Compute the spatial join.
      chr_geos_views <- amadeusdb::compute_view(
        sql = sql_processgeos,
        params = list_params,
        view = chr_geos_hourly[i],
        all = chr_geos_views
      )
    }
    # --------------------------------------------------------------------------
    # Write batch .parquet.
    amadeusdb::batch_write(
      locs_id = locs_id_sql,
      time_id = "time, level",
      columns = variable_sql,
      views = chr_geos_hourly,
      parquet = chr_geos_parquet[l],
      single = TRUE
    )
    # --------------------------------------------------------------------------
    # Remove hourly views now represented by the registered batch.
    chr_geos_views <- amadeusdb::batch_drop(chr_geos_views, chr_geos_hourly)
  }
  # ----------------------------------------------------------------------------
  # Return parquet file out paths.
  invisible(chr_geos_parquet)
}

################################################################################
#' Read GEOS Vertical Coordinate Values
#'
#' Read a coordinate variable from a GEOS NetCDF file.
#'
#' @param path character(1). Path to the NetCDF file.
#' @param coordinate character(1). Name of the coordinate variable, such as
#'   `"lev"` for vertical levels.
#'
#' @return The coordinate values returned by [ncdf4::ncvar_get()], typically
#'   a numeric vector of vertical levels.
#'
#' @details
#' The NetCDF file is closed when the function exits, including when reading
#' the coordinate fails.
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @keywords internal
#' @export
geoslev <- function(path, coordinate) {
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  ncdf4::ncvar_get(nc, coordinate)
}
