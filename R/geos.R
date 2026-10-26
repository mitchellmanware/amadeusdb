################################################################################
#' Extract GEOS-CF covariates with SedonaDB
#'
#' @description
#' Read local NASA GEOS-CF NetCDF files, extract one variable at prepared
#' locations, and write the results to Parquet files in batches. Point locations
#' use raster cell values; polygon locations use zonal statistics.
#'
#' @param date character(2) or Date(2). Inclusive start and end dates in
#'   chronological order. Character dates must use `"YYYY-MM-DD"`. Repeat the
#'   date to process a single day.
#' @param input character(1). Directory containing GEOS-CF files with their
#'   original `GEOS-CF.v01.rpl` filenames. Subdirectories are searched
#'   recursively. Supply files from a compatible collection with no repeated
#'   filename timestamps.
#' @param output character(1). Directory for the output Parquet files. Created
#'   recursively if it does not exist.
#' @param variable character(1). Case-sensitive NetCDF variable name. The same
#'   variable is read from every selected file and used as the output value
#'   column name.
#' @param locs sf object. Prepared extraction locations with a geometry column
#'   named `geometry` and an identifier column named by `locs_id`. Use one
#'   geometry type throughout: `POINT`, `POLYGON`, or `MULTIPOLYGON`. Locations
#'   must already have a CRS compatible with the raster's `OGC:CRS84` CRS, such
#'   as `EPSG:4326`; this function does not transform or buffer locations.
#' @param locs_id character(1). Name of the location identifier column in
#'   `locs`. Supply unique, non-missing identifiers to obtain one row per
#'   location and timestamp.
#' @param radius numeric(1). Reserved for location preparation. Currently
#'   unused; apply any required buffer to `locs` before calling this function.
#' @param fun character(1). Polygon summary statistic: `"mean"`, `"sum"`,
#'   `"min"`, `"max"`, `"median"`, `"count"`, `"stddev"`, `"variance"`, or
#'   `"mode"`. Defaults to `"mean"`. Ignored for point extraction.
#' @param touches logical(1). Whether polygon extraction includes all raster
#'   cells touched by the geometry. Passed to `RS_ZonalStats()`; ignored for
#'   point extraction. Defaults to `TRUE`.
#' @param exclude_nodata logical(1). Whether to exclude raster NoData cells from
#'   polygon statistics. Passed to `RS_ZonalStats()`; ignored for point
#'   extraction. Defaults to `TRUE`.
#' @param batch numeric(1). Positive whole number of source files to process per
#'   output file. Defaults to `50L`.
#'
#' @details
#' Files are selected using the requested dates, and timestamps are derived
#' from their filenames and parsed as UTC. Extraction uses band 1 of the
#' requested NetCDF variable. Additional bands or vertical levels are not
#' extracted.
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
#' identifier column, a character `time` column derived from the source
#' filenames, and a value column named by `variable`.
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
#' @import checkmate
#'
#' @seealso [amadeus::download_geos()], [join_variables()],
#'   [join_covariates()]
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
#'   output = tempfile("geos_CO_"),
#'   variable = "CO",
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
  # Validate batch size.
  # NEW CHECKMATE PROPOSAL: replace this stopifnot() with the two assertions.
  # These preserve exact whole numbers without imposing an integer-size limit.
  checkmate::assert_number(batch, lower = 1, finite = TRUE)
  checkmate::assert_true(batch == floor(batch))
  # ----------------------------------------------------------------------------
  # Check zonal statistic function against SedonaDB supported (as of 10/21/26)
  # NEW CHECKMATE NOTE: retain match.arg() and tolower(); assert_choice() alone
  # would change the current handling of abbreviations and uppercase values.
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
  # Derive extraction location geometries.
  # NEW CHECKMATE PROPOSAL: optional additional checks before using arguments.
  checkmate::assert_string(input, min.chars = 1L)
  checkmate::assert_directory_exists(input)
  checkmate::assert_string(variable, min.chars = 1L)
  checkmate::assert_string(locs_id, min.chars = 1L)
  checkmate::assert_class(sf_locs, "sf")
  checkmate::assert_subset(c(locs_id, "geometry"), names(sf_locs))
  checkmate::assert_flag(touches)
  checkmate::assert_flag(exclude_nodata)
  # Date parsing/order, CRS compatibility, and buffer preparation need separate
  # domain checks. radius and geom remain reserved for location preparation.
  chr_locsgeom <- as.character(unique(sf::st_geometry_type(sf_locs)))
  # NEW CHECKMATE PROPOSAL: reject mixed or unsupported geometry types early.
  checkmate::assert_choice(chr_locsgeom, c("POINT", "POLYGON", "MULTIPOLYGON"))
  # ----------------------------------------------------------------------------
  # Identify GEOS file paths from all files in the path.
  chr_paths <- list.files(
    input,
    pattern = "GEOS-CF.v01.rpl",
    full.names = TRUE,
    recursive = TRUE
  )
  # ----------------------------------------------------------------------------
  # Generate sequence from user supplied dates.
  chr_datesofinterest <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  # ----------------------------------------------------------------------------
  # Select only those file paths which align with user supplied date sequence.
  chr_datapaths <- unique(
    grep(
      paste(
        chr_datesofinterest,
        collapse = "|"
      ),
      chr_paths,
      value = TRUE
    )
  )
  # ----------------------------------------------------------------------------
  # Store file path, NetCDF variable path, date, and generated db names.
  df_geospaths <- data.frame(
    path = chr_datapaths,
    time = amadeus::process_collection(
      chr_datapaths,
      source = "geos",
      datetime = TRUE
    )
  )
  df_geospaths$date <- as.character(as.POSIXct(
    as.character(df_geospaths$time),
    format = "%Y%m%d%H%M",
    tz = "UTC"
  ))
  df_geospaths$var <- sprintf(
    'NETCDF:"%s":%s',
    normalizePath(df_geospaths$path, winslash = "/", mustWork = TRUE),
    variable
  )
  df_geospaths$view <- sprintf("db_geos_%012s", df_geospaths$time)
  # ----------------------------------------------------------------------------
  # Check dates, paths, and db names are set, and all exist with no NAs/NULLs.
  # NEW CHECKMATE PROPOSAL: express the metadata requirements separately.
  checkmate::assert_data_frame(df_geospaths, min.rows = 1L)
  checkmate::assert_names(
    names(df_geospaths),
    must.include = c("time", "path", "view")
  )
  checkmate::assert_vector(
    df_geospaths$time,
    any.missing = FALSE,
    unique = TRUE
  )
  checkmate::assert_file_exists(df_geospaths$path)
  checkmate::assert_character(
    df_geospaths$view,
    any.missing = FALSE,
    unique = TRUE
  )
  # ----------------------------------------------------------------------------
  # Define location identifiers in SQL-friendly format.
  locs_id_sql <- paste0(
    '"',
    gsub('"', '""', locs_id, fixed = TRUE),
    '"'
  )
  # ----------------------------------------------------------------------------
  # Define variable identifiers in SQL-friendly format.
  variable_sql <- paste0(
    '"',
    gsub('"', '""', variable, fixed = TRUE),
    '"'
  )
  # ----------------------------------------------------------------------------
  # Define SQL code for processing GEOS data for points or polygons.
  sql_extraction <- if (chr_locsgeom == "POINT") {
    "RS_Value(r.raster, db_locs.geometry, 1)"
  } else {
    "
      RS_ZonalStats(
        r.raster,
        db_locs.geometry,
        1,
        $3,
        ($4 = 1),
        ($5 = 1)
      )
    "
  }
  # ----------------------------------------------------------------------------
  # Define full SQL code including point/polygon distinction.
  sql_processgeos <- glue::glue(
    "
      WITH r AS (
        SELECT
          RS_SetCRS(
            RS_FromPath($1),
            'OGC:CRS84'
          ) AS raster
      )
      SELECT
        db_locs.{locs_id_sql} AS {locs_id_sql},
        CAST($2 AS TEXT) AS time,
        {sql_extraction} AS {variable_sql}
      FROM db_locs
      CROSS JOIN r
    "
  )
  # ----------------------------------------------------------------------------
  # Initiate database cleanup upon exit.
  chr_geos_views <- character(0)
  on.exit(amadeusdb::cleanup_views(chr_geos_views), add = TRUE)
  # ----------------------------------------------------------------------------
  # Convert extraction points to SedonaDB data frame.
  db_locs <- sedonadb::as_sedonadb_dataframe(sf_locs)
  # ----------------------------------------------------------------------------
  # Register the extraction point data object in the SedonaDB session ! once !.
  sedonadb::sd_to_view(
    db_locs,
    "db_locs",
    overwrite = TRUE
  )
  chr_geos_views <- c(chr_geos_views, "db_locs")
  # ----------------------------------------------------------------------------
  # Split hourly file row indices into `batch` sized batches.
  list_geos_batch <- split(
    seq_len(nrow(df_geospaths)),
    ceiling(seq_len(nrow(df_geospaths)) / batch)
  )
  # ----------------------------------------------------------------------------
  # Name output parquet paths based on variable and time inclusions.
  chr_geos_batch <- vapply(
    list_geos_batch,
    function(i) {
      chr_range <- range(as.character(df_geospaths$time[i]))
      sprintf(
        "%s_%s_%s_%s",
        "geos",
        variable,
        chr_range[1],
        chr_range[2]
      )
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  # ----------------------------------------------------------------------------
  # Prepare one Parquet output path per batch.
  # NEW CHECKMATE PROPOSAL: replace the scalar path check below.
  checkmate::assert_string(output, min.chars = 1L)
  # ----------------------------------------------------------------------------
  # Create output path if does not exist.
  if (!dir.exists(output)) {
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
  }
  # ----------------------------------------------------------------------------
  # Create output path if does not exist.
  if (!dir.exists(output)) {
    stop("Could not create output directory: ", output)
  }
  # ----------------------------------------------------------------------------
  # Generate .parquet paths for each batch view.
  chr_output <- normalizePath(
    output,
    winslash = "/",
    mustWork = TRUE
  )
  chr_geos_parquet <- file.path(
    chr_output,
    paste0(chr_geos_batch, ".parquet")
  )
  # ----------------------------------------------------------------------------
  # Perform spatial extraction within each batch.
  for (l in seq_along(list_geos_batch)) {
    # --------------------------------------------------------------------------
    # Skip if batch exists.
    if (file.exists(chr_geos_parquet[l])) {
      message(
        "Skipping existing batch ",
        l,
        ": ",
        chr_geos_parquet[l]
      )
      next
    }
    # --------------------------------------------------------------------------
    # Index data files, times, and names to active batch.
    df_geos_batch <- df_geospaths[
      list_geos_batch[[l]],
      ,
      drop = FALSE
    ]
    # --------------------------------------------------------------------------
    # Generate daily view names for active batch.
    chr_geos_hourly <- sprintf(
      "db_time_%03d",
      seq_len(nrow(df_geos_batch))
    )
    message(variable, ": batch ", l, "/", length(list_geos_batch))
    for (i in seq_len(nrow(df_geos_batch))) {
      # ------------------------------------------------------------------------
      # Time extraction directly from NetCDF not supported (as of 10/21/26)
      list_params <- list(
        as.character(df_geos_batch$var[i]), # $1: raster source
        df_geos_batch$date[i] # $2: timestamp
      )
      # ------------------------------------------------------------------------
      # Include additional arguments for zonal statistics.
      if (!chr_locsgeom == "POINT") {
        list_params <- c(
          list_params,
          list(
            fun, # $3: statistic
            as.integer(touches), # $4: 0/1
            as.integer(exclude_nodata) # $5: 0/1
          )
        )
      }
      # ------------------------------------------------------------------------
      # Compute and register the hourly extraction.
      db_time <- sedonadb::sd_sql(
        sql_processgeos,
        params = list_params
      ) |>
        sedonadb::sd_compute()
      # ------------------------------------------------------------------------
      sedonadb::sd_to_view(
        db_time,
        chr_geos_hourly[i],
        overwrite = TRUE
      )
    }
    # --------------------------------------------------------------------------
    # Write batch directly to Parquet without materializing another copy.
    db_batch <- sedonadb::sd_sql(
      amadeusdb::geosunion(locs_id_sql, variable_sql, chr_geos_hourly)
    )
    sedonadb::sd_write_parquet(
      db_batch,
      chr_geos_parquet[l],
      single_file_output = TRUE
    )
    # --------------------------------------------------------------------------
    # Remove hourly views after the batch has been written successfully.
    for (h in chr_geos_hourly) {
      sedonadb::sd_drop_view(h)
      chr_geos_views <- chr_geos_views[chr_geos_views != h]
    }
    rm(db_time, db_batch)
  }
  # ----------------------------------------------------------------------------
  # Return parquet file out paths.
  invisible(chr_geos_parquet)
}

################################################################################
#' Build a GEOS extraction union query
#'
#' @description
#' Construct SQL that stacks GEOS extraction views using `UNION ALL`, retaining
#' the location identifier, time, and selected variable columns.
#'
#' @param l character(1). SQL identifier for the location column, already quoted
#'   if necessary.
#' @param var character(1). SQL identifier for the variable column, already
#'   quoted if necessary.
#' @param views character. Non-empty vector of SQL view names to stack, in query
#'   order. Names are inserted verbatim and must already be valid SQL
#'   identifiers.
#'
#' @details
#' Every view must contain the columns identified by `l` and `var`, and a column
#' named `time`. The query retains duplicate rows. This helper builds SQL text
#' only; it does not execute the query or validate the views.
#'
#' @return A character(1) SQL query.
#' @seealso [sedona_geos()], [quote_sql()]
#' @keywords internal
#' @examples
#' amadeusdb::geosunion(
#'   l = '"site_id"',
#'   var = '"CO"',
#'   views = c("db_time_001", "db_time_002")
#' )
#' @export
geosunion <- function(l, var, views) {
  paste(
    sprintf(
      "SELECT %s, time, %s FROM %s",
      l,
      var,
      views
    ),
    collapse = "\nUNION ALL\n"
  )
}
