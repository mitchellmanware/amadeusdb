################################################################################
#' Calculate daily HMS smoke indicators with SedonaDB
#'
#' @description
#' Intersect prepared locations with local NOAA Hazard Mapping System (HMS)
#' smoke polygons and write daily light, medium, and heavy smoke indicators to
#' Parquet files in batches.
#'
#' @param date character(2) or Date(2). Inclusive start and end dates in
#'   chronological order. Character dates must use `"YYYY-MM-DD"`. Repeat the
#'   date to process a single day.
#' @param input character(1). Directory containing HMS smoke shapefiles named
#'   with the `hms_smoke` prefix and a final `YYYYMMDD.shp` date suffix. The
#'   accompanying shapefile components must be available. Subdirectories are
#'   not searched.
#' @param output character(1). Directory for the output Parquet files. Created
#'   recursively if it does not exist.
#' @param locs sf object. Prepared point or polygon locations with a geometry
#'   column named `geometry` and an identifier column named by `locs_id`.
#'   Locations must already use a CRS compatible with the HMS shapefiles;
#'   this function does not transform or buffer locations.
#' @param locs_id character(1). Name of the location identifier column in
#'   `locs`. Rows sharing an identifier are grouped when computing the
#'   indicators. Supply unique, non-missing identifiers for separate location
#'   results.
#' @param radius numeric(1). Reserved for location preparation. Currently
#'   unused; apply any required buffer to `locs` before calling this function.
#' @param batch numeric(1). Positive whole number of source files to process per
#'   output file. Defaults to `50L`.
#'
#' @details
#' Dates are derived from filenames, with at most one selected shapefile per
#' date. Only dates with selected input files are processed; missing dates are
#' not filled with additional rows.
#'
#' A spatial left join uses `ST_Intersects()` to retain locations without
#' matching smoke polygons. For each processed date and location identifier,
#' `light`, `medium`, and `heavy` are 1 when any intersecting polygon has that
#' density and 0 otherwise. Density labels are trimmed and compared without
#' regard to case. The three indicators are independent, so more than one can
#' equal 1. Polygon overlap fractions and within-day exposure durations are
#' not calculated.
#'
#' Each batch is written as `hms_<first_date>_<last_date>.parquet`, with dates
#' formatted as `YYYYMMDD`. A batch whose output file already exists is skipped,
#' and its path is included in the return value. Existing files are not checked
#' against the supplied locations; use a separate output directory when the
#' locations change.
#'
#' @return
#' Invisibly, a character vector of absolute Parquet file paths, including
#' existing batches that were skipped. Each file contains the location
#' identifier column, a `date` column stored as a SQL date, and integer
#' `light`, `medium`, and `heavy` indicator columns.
#'
#' @note
#' Uses the default SedonaDB context and view names `db_locs`, `db_hms`, and
#' `db_day_<index>`. Existing views with those names may be replaced.
#'
#' @importFrom amadeus generate_date_sequence
#' @importFrom glue glue
#' @importFrom sedonadb as_sedonadb_dataframe sd_compute sd_drop_view sd_read_sf
#'   sd_sql sd_to_view sd_write_parquet
#' @import checkmate
#'
#' @seealso [amadeus::download_hms()], [join_covariates()], [hmsdates()]
#' @examples
#' \dontrun{
#' locs <- sf::st_as_sf(
#'   data.frame(site_id = "001", lon = -78.90, lat = 35.97),
#'   coords = c("lon", "lat"),
#'   crs = 4326
#' )
#' hms_files <- sedona_hms(
#'   date = c("2024-01-01", "2024-01-31"),
#'   input = "data/hms",
#'   output = tempfile("hms_covariates_"),
#'   locs = locs,
#'   locs_id = "site_id",
#'   radius = 0,
#'   batch = 31L
#' )
#' }
#' @export
sedona_hms <- function(
  date,
  input,
  output,
  locs,
  locs_id,
  radius = 0,
  batch = 50L
) {
  # ----------------------------------------------------------------------------
  # Prepare extraction locations (internal checks for arguments).
  sf_locs <- amadeusdb::prepare_locs(
    locs = locs,
    locs_id = locs_id,
    crs = .amadeusdb$list_crs$hms$crs,
    radius = radius
  )
  # ----------------------------------------------------------------------------
  # Validate batch size.
  checkmate::assert_number(batch, lower = 1, finite = TRUE)
  checkmate::assert_true(batch == floor(batch))
  # ----------------------------------------------------------------------------
  # Validate input path.
  checkmate::assert_string(input, min.chars = 1L)
  checkmate::assert_directory_exists(input)
  # ----------------------------------------------------------------------------
  # Validate output path.
  checkmate::assert_string(output, min.chars = 1L)
  # ----------------------------------------------------------------------------
  # Validate locations and `locs_id` argument..
  checkmate::assert_string(locs_id, min.chars = 1L)
  checkmate::assert_class(sf_locs, "sf")
  checkmate::assert_subset(c(locs_id, "geometry"), names(sf_locs))
  # ----------------------------------------------------------------------------
  # Identify HMS shapefile paths from all files in the path.
  chr_paths <- list.files(
    input,
    pattern = "hms_smoke*.*.shp$",
    full.names = TRUE
  )
  # ----------------------------------------------------------------------------
  # Generate sequence from user supplied dates.
  chr_datesofinterest <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  # ----------------------------------------------------------------------------
  # Select paths which align with the requested date sequence.
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
  # Store file path, date from file, and generated database view names.
  df_hmspaths <- data.frame(
    path = chr_datapaths,
    date = amadeusdb::hmsdates(chr_datapaths)
  )
  df_hmspaths$view <- sprintf(
    "db_hms_%08s",
    gsub("-", "", df_hmspaths$date)
  )
  # ----------------------------------------------------------------------------
  # Validate dates, paths, and database view names.
  checkmate::assert_names(
    names(df_hmspaths),
    must.include = c("date", "path", "view")
  )
  checkmate::assert_character(
    df_hmspaths$date,
    any.missing = FALSE,
    unique = TRUE
  )
  checkmate::assert_file_exists(df_hmspaths$path)
  checkmate::assert_character(
    df_hmspaths$view,
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
  # Define SQL code for processing HMS data.
  sql_processhms <- glue::glue(
    "
      SELECT
        db_locs.{locs_id_sql} AS {locs_id_sql},
        CAST($1 AS DATE) AS date,

        MAX(CASE
          WHEN LOWER(TRIM(db_hms.\"Density\")) = 'light' THEN 1
          ELSE 0
        END) AS light,

        MAX(CASE
          WHEN LOWER(TRIM(db_hms.\"Density\")) = 'medium' THEN 1
          ELSE 0
        END) AS medium,

        MAX(CASE
          WHEN LOWER(TRIM(db_hms.\"Density\")) = 'heavy' THEN 1
          ELSE 0
        END) AS heavy

      FROM db_locs
      LEFT JOIN db_hms
        ON ST_Intersects(db_locs.geometry, db_hms.wkb_geometry)
      GROUP BY db_locs.{locs_id_sql}
    "
  )
  # ----------------------------------------------------------------------------
  # Initiate database cleanup upon exit.
  chr_hms_views <- character(0)
  on.exit(amadeusdb:::cleanup_views(chr_hms_views), add = TRUE)
  # ----------------------------------------------------------------------------
  # Convert extraction locations to a SedonaDB data frame.
  db_locs <- sedonadb::as_sedonadb_dataframe(sf_locs)
  # ----------------------------------------------------------------------------
  # Register the extraction point data object in the SedonaDB session ! once !.
  sedonadb::sd_to_view(
    db_locs,
    "db_locs",
    overwrite = TRUE
  )
  # ----------------------------------------------------------------------------
  # Split date row indices into batches.
  list_hms_batch <- split(
    seq_len(nrow(df_hmspaths)),
    ceiling(seq_len(nrow(df_hmspaths)) / batch)
  )
  # ----------------------------------------------------------------------------
  # Generate a database view name for each batch.
  chr_hms_batch <- vapply(
    list_hms_batch,
    function(i) {
      chr_range <- range(as.character(df_hmspaths$date[i]))
      sprintf(
        "%s_%s_%s",
        "hms",
        gsub("-", "", chr_range[1]),
        gsub("-", "", chr_range[2])
      )
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  # ----------------------------------------------------------------------------
  # Create output path if does not exist.
  if (!dir.exists(output)) {
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
  }
  stopifnot(dir.exists(output))
  # ----------------------------------------------------------------------------
  # Generate .parquet paths for each batch view.
  chr_output <- normalizePath(
    output,
    winslash = "/",
    mustWork = TRUE
  )
  chr_hms_parquet <- file.path(
    chr_output,
    paste0(chr_hms_batch, ".parquet")
  )
  # ----------------------------------------------------------------------------
  # Perform daily spatial joins within each batch.
  for (l in seq_along(list_hms_batch)) {
    # --------------------------------------------------------------------------
    # Skip if batch exists.
    if (file.exists(chr_hms_parquet[l])) {
      message(
        "Skipping existing batch ",
        l,
        ": ",
        chr_hms_parquet[l]
      )
      next
    }
    # --------------------------------------------------------------------------
    # Index data files, dates, and names to active batch.
    df_hms_batch <- df_hmspaths[
      list_hms_batch[[l]],
      ,
      drop = FALSE
    ]
    # --------------------------------------------------------------------------
    # Generate daily view names for active batch.
    chr_hms_daily <- sprintf("db_day_%03d", seq_len(nrow(df_hms_batch)))
    # --------------------------------------------------------------------------
    message("Processing batch ", l, "/", length(list_hms_batch))
    for (i in seq_len(nrow(df_hms_batch))) {
      # ------------------------------------------------------------------------
      # Read HMS data shapefile into the database.
      db_hms <- sedonadb::sd_read_sf(
        df_hms_batch$path[i],
        lazy = FALSE
      )
      # ------------------------------------------------------------------------
      # Register polygons in the database.
      sedonadb::sd_to_view(
        db_hms,
        "db_hms",
        overwrite = TRUE
      )
      # ------------------------------------------------------------------------
      # Compute the spatial join.
      db_date <- sedonadb::sd_sql(
        sql_processhms,
        params = list(as.character(df_hms_batch$date[i]))
      ) |>
        sedonadb::sd_compute()
      # ------------------------------------------------------------------------
      # Register post-spatial join data in the database.
      sedonadb::sd_to_view(
        db_date,
        chr_hms_daily[i],
        overwrite = TRUE
      )
    }
    # --------------------------------------------------------------------------
    # Materialize and union the batch before removing its daily views.
    db_batch <- sedonadb::sd_sql(
      amadeusdb::hmsunion(locs_id_sql, chr_hms_daily)
    )
    sedonadb::sd_write_parquet(
      db_batch,
      chr_hms_parquet[l],
      single_file_output = TRUE
    )
    # --------------------------------------------------------------------------
    # Remove daily views now represented by the registered batch.
    for (d in chr_hms_daily) {
      sedonadb::sd_drop_view(d)
      chr_hms_views <- chr_hms_views[chr_hms_views != d]
    }
    rm(db_date, db_batch)
  }
  # ----------------------------------------------------------------------------
  # Return parquet file out paths.
  invisible(chr_hms_parquet)
}

################################################################################
#' Build an HMS indicator union query
#'
#' @description
#' Construct SQL that stacks daily HMS indicator views using `UNION ALL`.
#'
#' @param l character(1). SQL identifier for the location column, already quoted
#'   if necessary.
#' @param v character. Non-empty vector of SQL view names to stack, in query
#'   order. Names are inserted verbatim and must already be valid SQL
#'   identifiers.
#'
#' @details
#' Every view must contain the location column identified by `l`, together with
#' `date`, `light`, `medium`, and `heavy`. The query retains duplicate rows.
#' This helper builds SQL text only; it does not execute the query or validate
#' the views.
#'
#' @return A character(1) SQL query.
#' @seealso [sedona_hms()], [quote_sql()]
#' @keywords internal
#' @examples
#' amadeusdb::hmsunion(
#'   l = '"site_id"',
#'   v = c("db_day_001", "db_day_002")
#' )
#' @export
hmsunion <- function(l, v) {
  paste(
    sprintf(
      "SELECT %s, date, light, medium, heavy FROM %s",
      l,
      v
    ),
    collapse = "\nUNION ALL\n"
  )
}

################################################################################
#' Extract dates from HMS shapefile paths
#'
#' @description
#' Format the final eight characters before the `.shp` suffix of an HMS
#' shapefile path as a date string.
#'
#' @param x character. Non-empty vector of HMS shapefile paths ending in
#'   `YYYYMMDD.shp`.
#'
#' @details
#' This helper extracts text only. It does not check that files exist or that
#' the extracted values are valid calendar dates.
#'
#' @return A character vector of date strings in `"YYYY-MM-DD"` format, in
#'   input order. The result is not a `Date` vector.
#' @seealso [sedona_hms()]
#' @keywords internal
#' @examples
#' amadeusdb::hmsdates(c(
#'   "data/hms_smoke20240101.shp",
#'   "data/hms_smoke20240102.shp"
#' ))
#' @export
hmsdates <- function(x) {
  chr_hmssplit <- gsub("\\.shp", "", x)
  chr_hmssub <- substr(
    chr_hmssplit,
    nchar(chr_hmssplit) - 7,
    nchar(chr_hmssplit)
  )
  paste0(
    substr(chr_hmssub, 1, 4),
    "-",
    substr(chr_hmssub, 5, 6),
    "-",
    substr(chr_hmssub, 7, 8)
  )
}
