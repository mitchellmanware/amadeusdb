################################################################################
#' Calculate daily HMS smoke indicators with SedonaDB
#'
#' @description
#' Prepare locations with [prepare_locs()], intersect them with local NOAA
#' Hazard Mapping System (HMS) smoke polygons, and write daily light, medium,
#' and heavy smoke indicators to Parquet files in batches.
#'
#' @param date character(1) or character(2). Inclusive start and end dates in
#'   chronological order, formatted as `"YYYY-MM-DD"`. A single date is
#'   repeated to process one day.
#' @param input character(1). Non-empty path to an existing directory containing
#'   HMS smoke shapefiles named with the `hms_smoke` prefix and a final
#'   `YYYYMMDD.shp` date suffix. The accompanying shapefile components must be
#'   available. All listed immediate entries must be files; subdirectories are
#'   not searched.
#' @param output character(1). Non-empty directory path for the output Parquet
#'   files. Created recursively if it does not exist.
#' @param locs sf, SpatVector, or data.frame object. Extraction locations with
#'   an identifier column named by `locs_id`. Spatial objects must have a source
#'   CRS; data frames must contain numeric `lon` and `lat` columns in longitude
#'   and latitude degrees. [prepare_locs()] transforms and optionally buffers
#'   locations. The prepared object must have a column named `geometry` and
#'   one geometry type throughout: `POINT`, `POLYGON`, or `MULTIPOLYGON`.
#' @param locs_id character(1). Non-empty name of an existing location
#'   identifier column in `locs`. Rows sharing an identifier are grouped when
#'   computing the indicators. Supply unique, non-missing identifiers for
#'   separate location results; identifier values are not validated.
#' @param radius numeric(1). Non-missing, non-negative buffer distance passed to
#'   [prepare_locs()]. Distance units follow [sf::st_buffer()] for the target
#'   CRS. Defaults to `0`, which skips buffering.
#' @param batch integer(1). Number of source files to process per output file,
#'   at least `1L`. Supply an integer such as `50L`, not a double such as `50`.
#'   Defaults to `50L`.
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
#'
#' @seealso [amadeus::download_hms()], [prepare_locs()],
#'   \code{\link{check_arguments}()}, [join_covariates()], [hmsdates()]
#' @examples
#' \dontrun{
#' locs <- sf::st_as_sf(
#'   data.frame(site_id = "001", lon = -78.90, lat = 35.97),
#'   coords = c("lon", "lat"),
#'   crs = 4326
#' )
#' hms_files <- sedona_hms(
#'   date = as.integer(as.Date(c("2024-01-01", "2024-01-31"))),
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
    crs = .amadeusdb$list_crs$geos$crs,
    radius = radius
  )
  # ----------------------------------------------------------------------------
  # Coerce `date` to vector of 2 if not already.
  date <- if (length(date) == 2) date else rep(date, 2)
  # ----------------------------------------------------------------------------
  # Check arguments.
  amadeusdb::check_arguments(
    locs = sf_locs,
    date = date,
    input = input,
    output = output,
    batch = batch,
    raster = FALSE
  )
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
    time = amadeusdb::hmsdates(chr_datapaths)
  )
  df_hmspaths$view <- sprintf(
    "db_hms_%08s",
    gsub("-", "", df_hmspaths$time)
  )
  # ----------------------------------------------------------------------------
  # Check dates, paths, and database view names.
  amadeusdb::check_views(df_hmspaths)
  # ----------------------------------------------------------------------------
  # Define location identifiers in SQL-friendly format.
  locs_id_sql <- paste0(
    '"',
    gsub('"', '""', locs_id, fixed = TRUE),
    '"'
  )
  # ----------------------------------------------------------------------------
  # Define column names based on radius buffer.
  light_sql <- sprintf("\"light_%05d\"", radius)
  medium_sql <- sprintf("\"medium_%05d\"", radius)
  heavy_sql <- sprintf("\"heavy_%05d\"", radius)
  # ----------------------------------------------------------------------------
  # Define SQL code for processing HMS data.
  sql_processhms <- glue::glue_data(
    paste(
      readLines(
        system.file(
          "sql",
          "sedona_hms.sql",
          package = "amadeusdb",
          mustWork = TRUE
        ),
        warn = FALSE
      ),
      collapse = "\n"
    ),
    .x = list(
      locs_id_sql = locs_id_sql,
      light_sql = light_sql,
      medium_sql = medium_sql,
      heavy_sql = heavy_sql
    )
  )
  # ----------------------------------------------------------------------------
  # Initiate database cleanup upon exit.
  chr_hms_views <- character(0)
  on.exit(amadeusdb::cleanup_views(chr_hms_views), add = TRUE)
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
  chr_hms_views <- unique(c(chr_hms_views, "db_locs"))
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
      chr_range <- range(as.character(df_hmspaths$time[i]))
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
      chr_hms_views <- unique(c(chr_hms_views, "db_hms"))
      # ------------------------------------------------------------------------
      # Compute the spatial join.
      db_date <- sedonadb::sd_sql(
        sql_processhms,
        params = list(as.character(df_hms_batch$time[i]))
      ) |>
        sedonadb::sd_compute()

      # ------------------------------------------------------------------------
      # Register post-spatial join data in the database.
      sedonadb::sd_to_view(
        db_date,
        chr_hms_daily[i],
        overwrite = TRUE
      )
      chr_hms_views <- unique(c(chr_hms_views, chr_hms_daily[i]))
    }
    # --------------------------------------------------------------------------
    # Materialize and union the batch before removing its daily views.
    db_batch <- sedonadb::sd_sql(
      amadeusdb::hmsunion(
        locs_id_sql,
        paste0(
          "date, ",
          paste0(
            sprintf("%s_%05d", c("light", "medium", "heavy"), radius),
            collapse = ", "
          )
        ),
        chr_hms_daily
      )
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
hmsunion <- function(l, c = c("time, light, medium, heavy"), v) {
  paste(
    sprintf(
      "SELECT %s, %s FROM %s",
      l,
      c,
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
