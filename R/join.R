#' Join variable-specific Parquet tables from one data source
#'
#' @description
#' Combine separate covariate tables from a single data source into one wider
#' Parquet dataset. Batch files within each input group form one logical table;
#' the tables are full-joined on the supplied keys using SedonaDB.
#'
#' @param input list. Non-empty list of non-empty character vectors of existing
#'   Parquet file paths. Each element contains all batch files for one variable
#'   or table. Directory paths and repeated paths within a group are not
#'   accepted. Optional list names identify groups in messages; they do not
#'   rename data columns.
#' @param output character(1). Directory for the joined Parquet dataset.
#'   Supply a new directory; existing output files are not removed.
#' @param by character. Non-empty vector of distinct join column names present
#'   in every input group. Defaults to `c("site_id", "time")`. Key types must
#'   be compatible across groups.
#'
#' @details
#' Each group must contain at least one non-key data column. Join keys must
#' be non-missing and unique across all files within that group. Non-key column
#' names must be distinct across groups; rename conflicting columns before
#' calling this function.
#'
#' Full outer joins retain keys present in any group and one shared copy of
#' each key column. Missing covariate values remain SQL `NULL`. Timestamp keys
#' are matched exactly without conversion, rounding, shifting, or aggregation.
#' A single input group is validated and written without joining another table.
#'
#' Source rows remain in SedonaDB during validation and joining. Only column
#' names and an at-most-one-row key diagnostic are collected into R. Temporary
#' views use names specific to the call and are scheduled for removal on exit.
#'
#' @return
#' A character vector of newly created `.parquet` file paths under `output`.
#' Paths already present before writing are excluded. The joined data are
#' written to disk rather than returned as an R data frame.
#'
#' @importFrom sedonadb sd_collect sd_drop_view sd_read_parquet sd_sql
#'   sd_to_view sd_write_parquet
#' @import checkmate
#'
#' @seealso [sedona_geos()], [join_covariates()],
#'   [sedonadb::sd_read_parquet()]
#' @examples
#' \dontrun{
#' co_files <- list.files(
#'   "data/geos_CO", pattern = "\\.parquet$", full.names = TRUE
#' )
#' no2_files <- list.files(
#'   "data/geos_NO2", pattern = "\\.parquet$", full.names = TRUE
#' )
#' geos_files <- join_variables(
#'   input = list(CO = co_files, NO2 = no2_files),
#'   output = file.path("outdir"),
#'   by = c("site_id", "time")
#' )
#' }
#' @export
join_variables <- function(
  input,
  output,
  by = c("site_id", "time")
) {
  # ----------------------------------------------------------------------------
  # Check `input` and contents.
  # ----------------------------------------------------------------------------
  # Ensure input is a list.
  checkmate::assert_list(input, min.len = 1L)
  # ----------------------------------------------------------------------------
  # Extract variables from list names.
  chr_variables <- names(input)
  if (is.null(chr_variables)) {
    chr_variables <- sprintf("input_%03d", seq_along(input))
  }
  # ----------------------------------------------------------------------------
  # Ensure paths within `input` are characters and exist.
  for (i in seq_along(input)) {
    # --------------------------------------------------------------------------
    # Extract paths.
    chr_paths <- input[[i]]
    # --------------------------------------------------------------------------
    # Ensure paths are characters.
    checkmate::assert_character(
      chr_paths,
      min.len = 1L,
      any.missing = FALSE,
      unique = TRUE,
      .var.name = sprintf("input[[%d]]", i)
    )
    # --------------------------------------------------------------------------
    # Ensure paths exist.
    checkmate::assert_file_exists(
      chr_paths,
      .var.name = sprintf("input[[%d]]", i)
    )
  }
  # ----------------------------------------------------------------------------
  # Check `output`.
  # ----------------------------------------------------------------------------
  # Ensure `output` is a character.
  checkmate::assert_string(output, min.chars = 1L)
  # ----------------------------------------------------------------------------
  # Create output path if does not exist.
  if (!dir.exists(output)) {
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
  }
  checkmate::assert_directory_exists(output)
  # ----------------------------------------------------------------------------
  # Ensure `by` is a character.
  checkmate::assert_character(
    by,
    min.len = 1L,
    min.chars = 1L,
    any.missing = FALSE,
    unique = TRUE
  )
  # ----------------------------------------------------------------------------
  # Quote identifiers, including names containing capitals or punctuation.
  chr_by_sql <- paste0('"', gsub('"', '""', by, fixed = TRUE), '"')
  sql_by <- paste(chr_by_sql, collapse = ", ")
  # ----------------------------------------------------------------------------
  # Generate names specific to this call in the working global context.
  chr_view_prefix <- gsub(
    "[^A-Za-z0-9_]",
    "_",
    basename(tempfile("db_merge_variables_"))
  )
  chr_variable_views <- sprintf(
    "%s_%03d",
    chr_view_prefix,
    seq_along(input)
  )
  # ----------------------------------------------------------------------------
  # Initiate views and data columns vectors.
  chr_merge_views <- character(0)
  chr_data_columns <- character(0)
  # ----------------------------------------------------------------------------
  # Drop only views created by this call, including when an R error occurs.
  on.exit(amadeusdb::cleanup_views(chr_merge_views), add = TRUE)
  # ----------------------------------------------------------------------------
  # Read each group as one logical table and check its schema and join keys.
  for (i in seq_along(input)) {
    # --------------------------------------------------------------------------
    # Import one variable (input) data from .parquet files in the database.
    message("Registering variable group ", i, "/", length(input), "...")
    db_variable <- sedonadb::sd_read_parquet(input[[i]])
    sedonadb::sd_to_view(db_variable, chr_variable_views[i])
    # --------------------------------------------------------------------------
    # Append to views to be cleaned up.
    chr_merge_views <- c(chr_merge_views, chr_variable_views[i])
    # --------------------------------------------------------------------------
    # LIMIT 0 collects column names and types without collecting data rows.
    chr_columns <- names(
      sedonadb::sd_sql(
        paste0("SELECT * FROM ", chr_variable_views[i], " LIMIT 0")
      ) |>
        sedonadb::sd_collect()
    )
    # --------------------------------------------------------------------------
    # Ensure merge keys (`by`) are in data.
    checkmate::assert_subset(
      by,
      chr_columns,
      .var.name = paste0("join columns in ", chr_variables[i])
    )
    # --------------------------------------------------------------------------
    # Identify data columns.
    chr_variable_columns <- setdiff(chr_columns, by)
    if (!length(chr_variable_columns)) {
      stop("No data columns in input group: ", chr_variables[i])
    }
    # --------------------------------------------------------------------------
    # Check for data column conflicts.
    chr_conflicts <- intersect(chr_variable_columns, chr_data_columns)
    if (length(chr_conflicts)) {
      stop(
        "Data columns occur in more than one group: ",
        paste(chr_conflicts, collapse = ", "),
        ". Rename these columns before merging."
      )
    }
    chr_data_columns <- c(chr_data_columns, chr_variable_columns)
    # --------------------------------------------------------------------------
    # This scan prevents duplicate keys from multiplying output rows.
    # It also rejects missing keys, which would not match using SQL equality.
    # Set FALSE only when the input key constraints are already established.
    message("Checking keys for ", chr_variables[i], "...")
    sql_missing <- paste(paste(chr_by_sql, "IS NULL"), collapse = " OR ")
    sql_check <- paste0(
      "SELECT ",
      sql_by,
      " FROM ",
      chr_variable_views[i],
      " GROUP BY ",
      sql_by,
      " HAVING COUNT(*) > 1 OR (",
      sql_missing,
      ") LIMIT 1"
    )
    df_key_check <- sedonadb::sd_sql(sql_check) |>
      sedonadb::sd_collect()
    if (nrow(df_key_check)) {
      stop(
        "Duplicate or missing join keys in input group: ",
        chr_variables[i]
      )
    }
  }
  # ----------------------------------------------------------------------------
  # Build a single query to merge multiple variable tables.
  # FULL OUTER JOIN preserves keys present in any variable group.
  # USING produces one shared copy of each key in SELECT *.
  # Missing variable values remain NULL; all data columns are retained.
  # Retain keys from either side, including rows absent from earlier variables.
  sql_join_keys <- paste(
    sprintf(
      "COALESCE(l.%s, r.%s) AS %s",
      chr_by_sql,
      chr_by_sql,
      chr_by_sql
    ),
    collapse = ",\n"
  )
  # ----------------------------------------------------------------------------
  # Fully qualify both sides of each join condition.
  sql_join_condition <- paste(
    sprintf("l.%s = r.%s", chr_by_sql, chr_by_sql),
    collapse = " AND "
  )
  # ----------------------------------------------------------------------------
  # Build nested joins with one shared set of keys at every step.
  # This constructs one SQL query; it does not execute joins inside the loop.
  sql_merge <- paste0(
    "SELECT * FROM ",
    chr_variable_views[1L]
  )
  # ----------------------------------------------------------------------------
  # Build query for merging acros all active views.
  for (i in seq_along(chr_variable_views)[-1L]) {
    sql_merge <- paste0(
      "SELECT\n",
      sql_join_keys,
      ",\n",
      "l.* EXCLUDE (",
      sql_by,
      "),\n",
      "r.* EXCLUDE (",
      sql_by,
      ")\n",
      "FROM (\n",
      sql_merge,
      "\n) AS l\n",
      "FULL OUTER JOIN ",
      chr_variable_views[i],
      " AS r\n",
      "ON ",
      sql_join_condition
    )
  }
  # ----------------------------------------------------------------------------
  # Extract file names in output before parquet file writing.
  chr_output1 <- list.files(
    output,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.parquet$"
  )
  # ----------------------------------------------------------------------------
  # Peform SQL-defined join.
  message("Performing join...")
  db_extraction <- sedonadb::sd_sql(sql_merge)
  # ----------------------------------------------------------------------------
  # Write joined data to a .parquet file/database (.parquet files).
  message("Saving to .parquet file(s)...")
  sedonadb::sd_write_parquet(
    db_extraction,
    output,
    single_file_output = FALSE
  )
  # ----------------------------------------------------------------------------
  # Extract file names in output after parquet file writing, and limit to only
  # the new .parquet files.
  chr_output2 <- list.files(
    output,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.parquet$"
  )
  # ----------------------------------------------------------------------------
  # Return only the newly written .parquet file path(s).
  setdiff(chr_output2, chr_output1)
}

#' Join hourly, daily, and static covariates to observations
#'
#' @description
#' Left-join Parquet covariate tables onto an observation table, such as AQS,
#' using location identifiers and resolution-specific temporal keys. Preserve
#' observation rows and write the combined dataset to Parquet using SedonaDB.
#'
#' @param input character(1). Path to one existing observation Parquet file
#'   containing the columns named by `locs_id` and `time_id`. A directory or
#'   vector of observation files is not accepted.
#' @param output character(1). Directory for the joined Parquet dataset.
#'   Supply a new directory; existing output files are not removed.
#' @param hourly NULL, character, or named list. Hourly covariate inputs
#'   containing the columns named by `locs_id` and `time_id`. A character
#'   vector of files or directories represents one logical table. A named list
#'   represents separate tables, for example `list(geos = geos_files)`.
#'   Directory contents are searched recursively for `.parquet` files.
#'   Defaults to `NULL`.
#' @param daily NULL, character, or named list. Daily covariate inputs
#'   containing the columns named by `locs_id` and `date_id`. Accepts the same
#'   file, directory, and named-list forms as `hourly`. Defaults to `NULL`.
#' @param static NULL, character, or named list. Covariate inputs matched only
#'   by `locs_id`. Accepts the same file, directory, and named-list forms as
#'   `hourly`. Defaults to `NULL`.
#' @param locs_id character(1). Location identifier column shared by the
#'   observation table and every covariate table. Defaults to `"site_id"`.
#'   Identifier types must be compatible across tables.
#' @param time_id character(1). Timestamp column in the observation table and
#'   hourly tables. Defaults to `"time"`. Values must be castable to a SQL
#'   timestamp and already represent UTC.
#' @param date_id character(1). Date column in daily covariate tables. Defaults
#'   to `"date"`. Values must be castable to a SQL date. A separate date column
#'   is not required in the observation table.
#' @param prefix logical(1). Whether to prefix right-hand data column names
#'   with their resolution and dataset label, for example `hourly_geos_CO` for
#'   column `CO` in `hourly = list(geos = geos_files)`. Defaults to `FALSE`.
#'
#' @details
#' The names supplied to `locs_id`, `time_id`, and `date_id` must be distinct
#' and non-empty. Named lists must have distinct, non-empty labels within each
#' resolution. Repeated file paths within a source are removed after path
#' normalization. Combine variable-specific tables with [join_variables()]
#' before supplying their batch files as one source.
#'
#' Temporal matching depends on the covariate resolution:
#' \itemize{
#'   \item Hourly tables match on location and the start of the calendar hour
#'     on both sides. The observation timestamp is cast to a SQL timestamp but
#'     retained without hour truncation in the output.
#'   \item Daily tables match on location and the date derived from the
#'     observation timestamp.
#'   \item Static tables match only on location and repeat their values across
#'     matching observation rows.
#' }
#'
#' All temporal keys must already represent UTC; local clock times are not
#' converted to UTC. For GEOS hourly averages labeled at `00:30`, the matching
#' hour starts at `00:00`. This matching rule assumes both sources describe the
#' same clock-hour interval, rather than instantaneous measurements or other
#' averaging windows.
#'
#' Right-hand keys must be non-missing and unique after temporal normalization.
#' Multiple records for the same location and normalized hour or date cause an
#' error; they are not aggregated. Repeated observation keys are retained.
#' No additional observation hours are generated, and unmatched covariates
#' remain SQL `NULL`.
#'
#' All observation columns and non-key covariate columns are retained. Column
#' conflicts after optional prefixing cause an error. Every output column name
#' has periods replaced with underscores; names must remain distinct after
#' this replacement. Preserve identifiers with leading zeroes as character
#' columns when preparing inputs.
#'
#' Temporary SedonaDB views use names specific to the call and are scheduled
#' for removal on exit. Input and output files are not deleted.
#'
#' @return
#' A character vector of newly created `.parquet` file paths under `output`.
#' Paths already present before writing are excluded. The combined data are
#' written to disk rather than returned as an R data frame.
#'
#' @importFrom sedonadb sd_collect sd_drop_view sd_read_parquet sd_select sd_sql
#'   sd_to_view sd_write_parquet
#' @importFrom stats setNames
#' @importFrom tidyselect all_of
#'
#' @seealso [join_variables()], [sedona_geos()], [sedona_hms()],
#'   [sedonadb::sd_read_parquet()]
#' @examples
#' \dontrun{
#' covariate_files <- join_covariates(
#'   input = "data/aqs.parquet",
#'   output = file.path("outdir"),
#'   hourly = list(geos = "data/geos_variables"),
#'   daily = list(hms = "data/hms_covariates"),
#'   static = list(elevation = "data/elevation.parquet"),
#'   locs_id = "site_id",
#'   time_id = "time",
#'   date_id = "date",
#'   prefix = TRUE
#' )
#' }
#' @export
join_covariates <- function(
  input,
  output,
  hourly = NULL,
  daily = NULL,
  static = NULL,
  locs_id = "site_id",
  time_id = "time",
  date_id = "date",
  prefix = FALSE
) {
  # ----------------------------------------------------------------------------
  # Check `input` and contents.
  # ----------------------------------------------------------------------------
  # Ensure input is a character (path to .parquet files)
  # !!! Assumed to be AQS data for devel. !!!
  checkmate::assert_string(input, min.chars = 1L)
  checkmate::assert_file_exists(input)
  # ----------------------------------------------------------------------------
  # Check `output`.
  # ----------------------------------------------------------------------------
  # Ensure `output` is a character.
  checkmate::assert_string(output, min.chars = 1L)
  # ----------------------------------------------------------------------------
  # Create output path if does not exist.
  if (!dir.exists(output)) {
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
  }
  checkmate::assert_directory_exists(output)
  # ----------------------------------------------------------------------------
  # Ensure `locs_id`, `time_id`, and `date_id` are characters.
  checkmate::assert_string(locs_id, min.chars = 1L)
  checkmate::assert_string(time_id, min.chars = 1L)
  checkmate::assert_string(date_id, min.chars = 1L)
  # ----------------------------------------------------------------------------
  # Ensure `locs_id`, `time_id`, and `date_id` are unique.
  checkmate::assert_character(
    c(locs_id, time_id, date_id),
    any.missing = FALSE,
    unique = TRUE
  )
  # ----------------------------------------------------------------------------
  # Ensure `prefix` is TRUE/FALSE.
  checkmate::assert_flag(prefix)
  # ----------------------------------------------------------------------------
  # Quote SQL identifiers, preserving capitals and punctuation in data columns.
  locs_id_sql <- amadeusdb::quote_sql(locs_id)
  time_id_sql <- amadeusdb::quote_sql(time_id)
  date_id_sql <- amadeusdb::quote_sql(date_id)
  # ----------------------------------------------------------------------------
  # Expand each optional resolution into its separate source tables.
  # A character vector is ONE table's batch files; a named list is MANY tables.
  list_inputs <- list(aqs = input)
  chr_source_types <- c(aqs = "aqs")
  chr_source_labels <- c(aqs = "aqs")
  list_resolutions <- list(hourly = hourly, daily = daily, static = static)
  # ----------------------------------------------------------------------------
  # Collect source tables for each requested temporal resolution.
  for (chr_resolution in names(list_resolutions)) {
    # --------------------------------------------------------------------------
    # Skip resolutions for which no covariate input was supplied.
    list_sources <- list_resolutions[[chr_resolution]]
    if (is.null(list_sources)) {
      next
    }
    # --------------------------------------------------------------------------
    # Wrap a file vector as one named table; keep named lists as separate
    # tables within the same resolution.
    if (is.character(list_sources)) {
      list_sources <- setNames(list(list_sources), chr_resolution)
    } else if (!is.list(list_sources)) {
      checkmate::assert_list(list_sources, .var.name = chr_resolution)
    }
    # --------------------------------------------------------------------------
    # An empty source list contributes no tables to the join.
    if (!length(list_sources)) {
      next
    }
    # --------------------------------------------------------------------------
    # Require distinct dataset labels within the active resolution.
    chr_names <- names(list_sources)
    # NEW CHECKMATE PROPOSAL: replace the compound name validation below.
    checkmate::assert_names(
      chr_names,
      type = "unique",
      .var.name = paste0("names(", chr_resolution, ")")
    )
    # --------------------------------------------------------------------------
    # Store each table's paths, resolution, and display label under one
    # generated source identifier for use in the later SQL-building loop.
    for (i in seq_along(list_sources)) {
      # ------------------------------------------------------------------------
      # Internal SQL names do not depend on user-supplied dataset labels.
      chr_source <- sprintf("source_%03d", length(list_inputs))
      list_inputs[chr_source] <- list(list_sources[[i]])
      chr_source_types[chr_source] <- chr_resolution
      chr_source_labels[chr_source] <- paste(
        chr_resolution,
        chr_names[i],
        sep = "/"
      )
    }
  }
  # ----------------------------------------------------------------------------
  # Preserve source order, with AQS first as the left-hand observation table.
  chr_sources <- names(list_inputs)
  # ----------------------------------------------------------------------------
  # Use view names specific to this call in the working global context.
  chr_view_prefix <- gsub(
    "[^A-Za-z0-9_]",
    "_",
    basename(tempfile("db_join_covariates_"))
  )
  # ----------------------------------------------------------------------------
  # Map internal source identifiers to their call-specific SedonaDB views.
  chr_source_views <- setNames(
    paste0(chr_view_prefix, "_", chr_sources),
    chr_sources
  )
  # ----------------------------------------------------------------------------
  # Track successfully registered views for the exit cleanup below.
  chr_join_views <- character(0)
  on.exit(amadeusdb::cleanup_views(chr_join_views), add = TRUE)
  # ----------------------------------------------------------------------------
  # Store SQL projections and output fields; source rows stay in SedonaDB.
  # Start with all AQS columns, then append each source's selected fields
  # and LEFT JOIN clause while tracking names for conflict detection.
  list_source_sql <- list()
  chr_output_columns <- character(0)
  chr_select <- "a.*"
  chr_join <- character(0)
  # ----------------------------------------------------------------------------
  # Prepare each source using its resolution-specific keys and display label.
  for (chr_source in chr_sources) {
    chr_resolution <- chr_source_types[[chr_source]]
    chr_source_label <- chr_source_labels[[chr_source]]
    # --------------------------------------------------------------------------
    # Resolve a source directory or file vector into its batch files.
    chr_paths <- list_inputs[[chr_source]]
    # NEW CHECKMATE PROPOSAL: separate path-vector and existence requirements.
    checkmate::assert_character(
      chr_paths,
      min.len = 1L,
      min.chars = 1L,
      any.missing = FALSE,
      .var.name = chr_source_label
    )
    checkmate::assert_true(
      all(file.exists(chr_paths)),
      .var.name = paste0(chr_source_label, " paths")
    )
    # --------------------------------------------------------------------------
    # Expand directories recursively into Parquet files and retain explicit
    # file paths, combining both forms into one vector for this source.
    chr_paths <- unlist(
      lapply(chr_paths, function(h) {
        if (dir.exists(h)) {
          list.files(
            h,
            pattern = "\\.parquet$",
            full.names = TRUE,
            recursive = TRUE
          )
        } else {
          h
        }
      }),
      use.names = FALSE
    )
    # --------------------------------------------------------------------------
    # Reject directories without Parquet files before registering the source.
    if (!length(chr_paths)) {
      stop("No Parquet files found for source: ", chr_source_label)
    }
    # --------------------------------------------------------------------------
    # Normalize paths before deduplication so repeated references to the same
    # file do not add its rows more than once within this source.
    chr_paths <- unique(normalizePath(
      chr_paths,
      winslash = "/",
      mustWork = TRUE
    ))

    # --------------------------------------------------------------------------
    # Register each file group as one logical table without collecting its rows.
    message("Registering ", chr_source_label, " data...")
    db_source <- sedonadb::sd_read_parquet(chr_paths)
    sedonadb::sd_to_view(db_source, chr_source_views[[chr_source]])
    # --------------------------------------------------------------------------
    # Add the view to cleanup only after registration succeeds.
    chr_join_views <- c(chr_join_views, chr_source_views[[chr_source]])
    # --------------------------------------------------------------------------
    # Retrieve only schema information using a zero-row result.
    chr_columns <- names(
      sedonadb::sd_sql(paste0(
        "SELECT * FROM ",
        chr_source_views[[chr_source]],
        " LIMIT 0"
      )) |>
        sedonadb::sd_collect()
    )
    # --------------------------------------------------------------------------
    # Switch keys based on active data being processed.
    # Hourly tables require location and timestamp; daily tables require
    # location and date; static tables require only the location identifier.
    chr_keys <- switch(
      chr_resolution,
      aqs = c(locs_id, time_id),
      hourly = c(locs_id, time_id),
      daily = c(locs_id, date_id),
      static = locs_id
    )
    # --------------------------------------------------------------------------
    # Validate join keys.
    # NEW CHECKMATE PROPOSAL: optional replacement for schema membership only.
    checkmate::assert_subset(
      chr_keys,
      chr_columns,
      .var.name = paste0("join columns in ", chr_source_label)
    )
    # --------------------------------------------------------------------------
    # Normalize the temporal key inside SQL and retain every other column.
    # Preserve the AQS timestamp; normalize the hourly right-hand key to HH:00.
    # Replace only the temporal field's projection, keeping its original
    # column name. Static source columns pass through without temporal casts.
    chr_projection <- amadeusdb::quote_sql(chr_columns)
    if (chr_resolution %in% c("aqs", "hourly")) {
      sql_time <- paste0("CAST(", time_id_sql, " AS TIMESTAMP(6))")
      if (chr_resolution == "hourly") {
        sql_time <- paste0("DATE_TRUNC('hour', ", sql_time, ")")
      }
      chr_projection[match(time_id, chr_columns)] <- paste0(
        sql_time,
        " AS ",
        time_id_sql
      )
    } else if (chr_resolution == "daily") {
      chr_projection[match(date_id, chr_columns)] <- paste0(
        "CAST(",
        date_id_sql,
        " AS DATE) AS ",
        date_id_sql
      )
    }
    # --------------------------------------------------------------------------
    # Build the normalized source query for both validation and joining.
    sql_source <- paste0(
      "SELECT ",
      paste(chr_projection, collapse = ", "),
      " FROM ",
      chr_source_views[[chr_source]]
    )
    # --------------------------------------------------------------------------
    # Store this query as a named common table expression for the final WITH
    # clause; building the SQL text does not materialize source rows.
    list_source_sql[[chr_source]] <- paste0(
      chr_source,
      "_data AS (\n",
      sql_source,
      "\n)"
    )
    # --------------------------------------------------------------------------
    # Keep all AQS columns and its existing row multiplicity.
    # AQS establishes the output fields and skips right-hand key validation;
    # repeated AQS observations are retained rather than deduplicated.
    if (chr_source == "aqs") {
      chr_output_columns <- chr_columns
      next
    }
    # --------------------------------------------------------------------------
    # Retain every right-hand data column, excluding only its join keys.
    # Optionally prefix data columns by resolution and dataset label.
    # Example with prefix = TRUE: hourly/geos + CO -> hourly_geos_CO.
    chr_data_columns <- setdiff(chr_columns, chr_keys)
    chr_data_names <- chr_data_columns
    if (prefix && length(chr_data_columns)) {
      chr_data_names <- paste0(
        gsub("/", "_", chr_source_label, fixed = TRUE),
        "_",
        chr_data_columns
      )
    }
    # --------------------------------------------------------------------------
    # Check the proposed names after optional prefixing against AQS and all
    # earlier source fields, then reserve them for subsequent checks.
    chr_conflicts <- intersect(chr_data_names, chr_output_columns)
    if (length(chr_conflicts)) {
      stop(
        "Conflicting data columns in ",
        chr_source_label,
        ": ",
        paste(chr_conflicts, collapse = ", "),
        ". Set prefix = TRUE or rename these columns before joining."
      )
    }
    chr_output_columns <- c(chr_output_columns, chr_data_names)
    # --------------------------------------------------------------------------
    # Check right-hand uniqueness AFTER timestamp/date normalization.
    # Duplicate right-hand keys would multiply AQS rows during the left join.
    # Only an at-most-one-row diagnostic is collected into R.
    # GROUP BY and HAVING detect repeated key combinations or NULL key
    # components. Multiple readings in one normalized hour are rejected,
    # rather than averaged or otherwise aggregated into one covariate row.
    message("Checking ", chr_source_label, " join keys...")
    chr_keys_sql <- amadeusdb::quote_sql(chr_keys)
    sql_keys <- paste(chr_keys_sql, collapse = ", ")
    sql_missing <- paste(paste(chr_keys_sql, "IS NULL"), collapse = " OR ")
    sql_check <- paste0(
      "SELECT ",
      sql_keys,
      " FROM (",
      sql_source,
      ") AS v",
      " GROUP BY ",
      sql_keys,
      " HAVING COUNT(*) > 1 OR (",
      sql_missing,
      ") LIMIT 1"
    )
    df_key_check <- sedonadb::sd_sql(sql_check) |>
      sedonadb::sd_collect()
    if (nrow(df_key_check)) {
      stop(
        "Duplicate or missing normalized join keys in source: ",
        chr_source_label
      )
    }
    # --------------------------------------------------------------------------
    # Build qualified LEFT JOIN conditions for each temporal resolution.
    # Each source receives its own alias and matches the AQS location key.
    # Static sources use this location condition without a temporal condition.
    chr_alias <- paste0("v", match(chr_source, chr_sources))
    sql_condition <- paste0(
      "a.",
      locs_id_sql,
      " = ",
      chr_alias,
      ".",
      locs_id_sql
    )
    if (chr_resolution == "hourly") {
      # ------------------------------------------------------------------------
      # Define SQL date join for hourly data.
      # Match the AQS hour to the already truncated covariate hour, while
      # retaining the original AQS timestamp in the selected output.
      sql_condition <- paste0(
        sql_condition,
        " AND DATE_TRUNC('hour', a.",
        time_id_sql,
        ") = ",
        chr_alias,
        ".",
        time_id_sql
      )
    } else if (chr_resolution == "daily") {
      # ------------------------------------------------------------------------
      # Define SQL date join for daily data.
      # Derive the observation date from the AQS timestamp, rather than
      # requiring a separate AQS date column.
      sql_condition <- paste0(
        sql_condition,
        " AND CAST(a.",
        time_id_sql,
        " AS DATE) = ",
        chr_alias,
        ".",
        date_id_sql
      )
    }
    # --------------------------------------------------------------------------
    # Define SQL left join statement.
    chr_join <- c(
      chr_join,
      paste0(
        "LEFT JOIN ",
        chr_source,
        "_data AS ",
        chr_alias,
        " ON ",
        sql_condition
      )
    )
    # --------------------------------------------------------------------------
    # Append qualified covariate fields with their chosen output names.
    # A source containing only join keys adds no fields to the SELECT list.
    if (length(chr_data_columns)) {
      chr_select <- c(
        chr_select,
        paste0(
          chr_alias,
          ".",
          amadeusdb::quote_sql(chr_data_columns),
          " AS ",
          amadeusdb::quote_sql(chr_data_names)
        )
      )
    }
  }
  # ----------------------------------------------------------------------------
  # Build one query. Daily/static values repeat over existing matching AQS rows.
  # No extra hours are generated, and missing covariates remain NULL.
  sql_merge <- paste0(
    "WITH\n",
    paste(unlist(list_source_sql, use.names = FALSE), collapse = ",\n"),
    "\nSELECT\n",
    paste(chr_select, collapse = ",\n"),
    "\nFROM aqs_data AS a\n",
    paste(chr_join, collapse = "\n")
  )
  # ----------------------------------------------------------------------------
  # Require every output column to be non-NULL.
  sql_complete <- paste0(
    "m.",
    amadeusdb::quote_sql(chr_output_columns),
    " IS NOT NULL",
    collapse = " AND "
  )
  # ----------------------------------------------------------------------------
  # Extract file names in output before parquet file writing.
  chr_output1 <- list.files(
    output,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.parquet$"
  )
  # ----------------------------------------------------------------------------
  # Peform SQL-defined join.
  message("Performing join...")
  db_extraction <- sedonadb::sd_sql(
    paste0(
      "SELECT * FROM (\n",
      sql_merge,
      "\n) AS m WHERE ",
      sql_complete
    )
  )
  # ----------------------------------------------------------------------------
  # Replace "." with "_" in all column names.
  db_extraction <- sedonadb::sd_select(
    db_extraction,
    tidyselect::all_of(stats::setNames(
      chr_output_columns,
      gsub(".", "_", chr_output_columns, fixed = TRUE)
    ))
  )
  # ----------------------------------------------------------------------------
  # Write joined data to a .parquet file/database (.parquet files).
  message("Saving to .parquet file(s)...")
  sedonadb::sd_write_parquet(
    db_extraction,
    output,
    single_file_output = FALSE
  )
  # ----------------------------------------------------------------------------
  # Extract file names in output after parquet file writing, and limit to only
  # the new .parquet files.
  chr_output2 <- list.files(
    output,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.parquet$"
  )
  # ----------------------------------------------------------------------------
  # Return only the newly written .parquet file path(s).
  setdiff(chr_output2, chr_output1)
}

################################################################################
#' Quote SQL identifiers
#'
#' @description
#' Enclose column or table names in double quotes and escape embedded double
#' quotes by doubling them.
#'
#' @param x character. SQL identifiers to quote.
#'
#' @details
#' Use this helper for identifiers such as column names, not for SQL string
#' values or complete expressions. Quoting preserves capitalization, spaces,
#' and punctuation in the names.
#'
#' @return A character vector of double-quoted SQL identifiers.
#' @seealso [join_covariates()], [geosunion()], [hmsunion()]
#' @keywords internal
#' @examples
#' amadeusdb::quote_sql(c("site_id", "CO", "site name", 'a"b'))
#' @export
quote_sql <- function(x) {
  paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
}
