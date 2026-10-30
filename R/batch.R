################################################################################
#' Split source row indices into consecutive batches
#'
#' @description
#' Divide the row indices of a data frame into consecutive groups containing
#' up to the requested number of rows. Preserve the original row order.
#'
#' @param df data.frame. Source-file metadata or other tabular input. Only its
#'   row count is used; column names and values do not affect batching.
#' @param batch integer(1). Maximum number of rows per batch, at least `1L`.
#'   The caller is responsible for supplying a valid batch size; this helper
#'   performs no explicit argument validation.
#'
#' @details
#' Row indices are assigned to groups using
#' `ceiling(seq_len(nrow(df)) / batch)`. The final group may contain fewer
#' rows than `batch`. The input data are not sorted or modified.
#'
#' The resulting index vectors can be used to subset `df` or supplied as the
#' `batch` argument to [batch_prep()] when constructing output paths.
#'
#' @return
#' A named list of integer row-index vectors, ordered by batch number and
#' named `"1"`, `"2"`, and so on. Returns an empty list when `df` has no rows.
#'
#' @seealso [batch_prep()], [sedona_geos()], [sedona_hms()]
#' @keywords internal
#' @examples
#' df <- data.frame(path = paste0("source_", 1:5, ".nc4"))
#' batch_split(df, batch = 2L)
#' batch_split(df[0, , drop = FALSE], batch = 2L)
#' @export
batch_split <- function(
  df,
  batch
) {
  split(
    seq_len(nrow(df)),
    ceiling(seq_len(nrow(df)) / batch)
  )
}

################################################################################
#' Construct Parquet output paths for prepared batches
#'
#' @description
#' Build one output path for each supplied group of source rows, using the
#' source name, variable label, and range of date or time labels in that batch.
#'
#' @param df data.frame. Source-file metadata containing the column named by
#'   `time_id`. Its values must be convertible to character date or time labels.
#' @param batch list. Non-empty list of non-empty integer vectors identifying
#'   rows in `df`. Each element defines one batch. List names are ignored;
#'   output paths follow the order of the list elements.
#' @param output character(1). Existing output directory. Converted to an
#'   absolute path with [base::normalizePath()], using forward slashes.
#'   The directory must exist; this helper does not create it.
#' @param source character(1). Source label used at the start of each filename,
#'   for example `"geos"` or `"hms"`.
#' @param variable character(1). Variable label included after the source name.
#'   Supply `""` when no variable label is needed. This argument has no default
#'   and must be supplied.
#' @param time_id character(1). Name of the column in `df` containing the date
#'   or time labels used for batch naming. Defaults to `"time"`.
#'
#' @details
#' For each batch, the selected labels are converted to character before
#' [base::range()] determines their minimum and maximum. Use labels whose
#' character ordering represents the desired date or time order. The function
#' does not parse dates or sort the rows in `df`.
#'
#' Hyphens are removed from the two range endpoints, and the filename is formed
#' as `<source>_<variable>_<first>_<last>.parquet`. Every occurrence of `"__"`
#' in the assembled filename stem is replaced with `"_"`, allowing an empty
#' variable label to produce `<source>_<first>_<last>.parquet`. Other date/time
#' punctuation, including spaces and colons, is retained.
#'
#' Batch membership is supplied by the caller. This helper constructs paths
#' without registering database views or writing files. It does not check for
#' existing output files or remove duplicate generated paths. Apart from path
#' normalization, no explicit argument validation is performed.
#'
#' @return
#' An unnamed character vector of absolute Parquet output paths, one per
#' supplied batch in list order.
#'
#' @seealso [check_output()], [sedona_geos()], [sedona_hms()]
#' @keywords internal
#' @examples
#' df <- data.frame(time = c("2018-01-01", "2018-01-02", "2018-01-03"))
#' batch_prep(
#'   df = df,
#'   batch = list(1:2, 3L),
#'   output = tempdir(),
#'   source = "hms",
#'   variable = ""
#' )
#' @export
batch_prep <- function(
  df,
  batch,
  output,
  source,
  variable = "",
  time_id = "time"
) {
  # ----------------------------------------------------------------------------
  # Generate a database view name for each batch.
  chr_batch <- vapply(
    batch,
    function(i) {
      # ------------------------------------------------------------------------
      # Define character range of dates/times.
      chr_range <- gsub("-", "", range(as.character(df[[time_id]][i])))
      # ------------------------------------------------------------------------
      # Define batch name based on source, variable (if supplied), and time.
      chr_batch_i <- sprintf(
        "%s_%s_%s_%s",
        source,
        variable,
        chr_range[1],
        chr_range[2]
      )
      # ------------------------------------------------------------------------
      # Change "__" to "_" for if variable is not supplied.
      gsub("__", "_", chr_batch_i)
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  # ----------------------------------------------------------------------------
  # Generate .parquet paths for each batch view.
  file.path(
    normalizePath(output, winslash = "/", mustWork = TRUE),
    paste0(chr_batch, ".parquet")
  )
}

################################################################################
#' Check for an existing Parquet batch
#'
#' @description
#' Return whether a batch output path already exists and emit a message
#' identifying an existing batch so the caller can skip processing it.
#'
#' @param parquet character(1). Path to the intended Parquet output file for
#'   one batch. Passed to [base::file.exists()] without additional validation.
#'
#' @details
#' An existing path produces the message `"Skipping existing batch: "` followed
#' by the path. File contents are not inspected and no files are modified.
#'
#' The caller controls loop execution. Use `if (batch_skip(path)) next` inside
#' the caller's batch loop to skip an existing output and continue with the
#' next batch. A missing path returns `FALSE` without emitting a message.
#'
#' @return
#' A logical scalar: `TRUE` when the supplied path exists, otherwise `FALSE`.
#'
#' @seealso [batch_prep()], [sedona_geos()], [sedona_hms()]
#' @keywords internal
#' @examples
#' chr_paths <- file.path(
#'   tempdir(), c("batch_001.parquet", "batch_002.parquet")
#' )
#' for (chr_path in chr_paths) {
#'   if (batch_skip(chr_path)) next
#'   # Process and write the batch when its output path is absent.
#' }
#' @export
batch_skip <- function(parquet) {
  lgl_skip <- file.exists(parquet)
  if (lgl_skip) {
    message("Skipping existing batch ", parquet)
  }
  lgl_skip
}

################################################################################
#' Drop registered views after writing a batch
#'
#' @description
#' Remove the supplied batch views from the default SedonaDB context and
#' return a filtered view-tracking vector. See the note about the current
#' filtering behavior.
#'
#' @param views character. Vector of registered view names tracked by the
#'   caller for cleanup when extraction finishes.
#' @param sub character. Non-empty vector of batch view names to remove.
#'   Each name is passed to [sedonadb::sd_drop_view()] in the supplied order.
#'
#' @details
#' Call this helper after the batch output has been written successfully.
#' Errors from [sedonadb::sd_drop_view()] propagate to the caller and stop
#' subsequent removals. No explicit argument validation is performed.
#'
#' @note
#' Each iteration currently filters the original `views` vector. Consequently,
#' although every name in `sub` is dropped when all removals succeed, the
#' returned vector excludes only the final name in `sub`. Earlier dropped
#' names remain in that vector. An empty `sub` leaves `chr_views` undefined.
#'
#' @return
#' A character vector containing `views` with entries equal to the final
#' element of `sub` removed, preserving the order of the retained entries.
#' The caller must assign the result to update its cleanup tracking vector.
#'
#' @importFrom sedonadb sd_drop_view
#' @seealso [cleanup_views()], [sedona_geos()], [sedona_hms()]
#' @keywords internal
#' @export
batch_drop <- function(
  views,
  sub
) {
  # --------------------------------------------------------------------------
  # Remove subset views now represented by the registered batch.
  for (s in sub) {
    sedonadb::sd_drop_view(s)
    views <- views[views != s]
  }
  # --------------------------------------------------------------------------
  # Return only unregistered views.
  views
}

################################################################################
#' Write a batch of registered extraction views to Parquet
#'
#' @description
#' Combine selected columns from registered SedonaDB views using `UNION ALL`
#' and write the results to a Parquet file or dataset.
#'
#' @param locs_id character(1). SQL identifier for the location column, already
#'   quoted if necessary, for example `quote_sql("site_id")`.
#' @param time_id character(1). SQL identifier or comma-separated SQL column
#'   list for temporal and related fields, for example `"date"` or
#'   `"time, level"`. Individual identifiers must already be quoted if needed.
#' @param columns character(1). Comma-separated SQL list of covariate columns
#'   to retain. Individual identifiers must already be quoted if necessary.
#' @param views character. Non-empty vector of registered SQL view names to
#'   combine. Names are inserted verbatim into the query and must be valid
#'   SQL identifiers, already quoted if necessary.
#' @param parquet character(1). Destination path passed to
#'   [sedonadb::sd_write_parquet()]. Supply a file path when `single = TRUE`
#'   or a dataset directory when `single = FALSE`.
#' @param single logical(1). Whether to write a single Parquet file. Defaults
#'   to `TRUE`. Passed as `single_file_output` to
#'   [sedonadb::sd_write_parquet()].
#'
#' @details
#' The query is constructed by [union_views()] and submitted through
#' [sedonadb::sd_sql()] in the default SedonaDB context. Each view must expose
#' the selected columns with compatible types. `UNION ALL` retains duplicate
#' rows, and the query does not specify an output row order.
#'
#' SQL identifiers and column lists are passed through without additional
#' quoting. No explicit argument validation or existing-output check is
#' performed here; callers can use [batch_skip()] before writing.
#'
#' Results are written without collecting them into an R data frame. After
#' writing, the local batch data-frame binding is removed. Source views remain
#' registered; callers manage their removal with [batch_drop()] or
#' [cleanup_views()] after the output has been written successfully.
#'
#' @return
#' `NULL`, invisibly. The batch results are written to `parquet` as a side
#' effect.
#'
#' @importFrom sedonadb sd_sql sd_write_parquet
#' @seealso [union_views()], [quote_sql()], [batch_prep()], [batch_skip()],
#'   [batch_drop()], [sedona_geos()], [sedona_hms()]
#' @keywords internal
#' @examples
#' \dontrun{
#' # The extraction views must already be registered in SedonaDB.
#' batch_write(
#'   locs_id = quote_sql("site_id"),
#'   time_id = "time, level",
#'   columns = quote_sql("co_0"),
#'   views = c("db_time_001", "db_time_002"),
#'   parquet = file.path(tempdir(), "geos_CO_batch.parquet"),
#'   single = TRUE
#' )
#' }
#' @export
batch_write <- function(
  locs_id,
  time_id,
  columns,
  views,
  parquet,
  single = TRUE
) {
  # --------------------------------------------------------------------------
  # Query database with UNION-ed views.
  db_batch <- sedonadb::sd_sql(
    amadeusdb::union_views(
      locs_id,
      time_id,
      columns,
      views
    )
  )
  # --------------------------------------------------------------------------
  # Write batch directly .parquet.
  sedonadb::sd_write_parquet(
    db_batch,
    parquet,
    single_file_output = single
  )
  # --------------------------------------------------------------------------
  # Remove batch database.
  rm(db_batch)
}

################################################################################
#' Read Parquet batches and register a SedonaDB view
#'
#' @description
#' Read a group of Parquet paths as one logical table, register it as a view
#' in the default SedonaDB context, and return an updated vector of view names
#' for cleanup tracking.
#'
#' @param path character. Parquet paths passed together to
#'   [sedonadb::sd_read_parquet()]. Multiple files represent batches of the
#'   same logical table and must have compatible schemas.
#' @param view character(1). Name of the view to register with
#'   [sedonadb::sd_to_view()]. Supply an unused view name; registration uses
#'   the default `overwrite = FALSE`.
#' @param all character. View names already tracked by the caller for cleanup.
#'   Supply `character(0)` when no views have been registered yet.
#'
#' @details
#' Reading is followed directly by view registration, without calling
#' [sedonadb::sd_compute()] or collecting rows into R. No explicit argument
#' validation, path normalization, or path deduplication is performed here.
#' Errors from reading or registration propagate to the caller.
#'
#' The view name is appended to `all` only after registration succeeds. The
#' caller must assign the returned vector to update its cleanup tracking and
#' arrange view removal with [cleanup_views()] or [batch_drop()]. The view
#' remains available after this helper returns.
#'
#' @return
#' A character vector equivalent to `c(all, view)`, preserving order and any
#' duplicate entries. The registered data remain in SedonaDB; the returned
#' value contains view names.
#'
#' @importFrom sedonadb sd_read_parquet sd_to_view
#' @seealso [view_schema()], [compute_view()], [cleanup_views()],
#'   [join_variables()], [join_covariates()]
#' @keywords internal
#' @examples
#' \dontrun{
#' chr_views <- batch_read(
#'   path = c("data/batch_001.parquet", "data/batch_002.parquet"),
#'   view = "db_source",
#'   all = character(0)
#' )
#' view_schema("db_source")
#' cleanup_views(chr_views)
#' }
#' @export
batch_read <- function(
  path,
  view,
  all
) {
  # --------------------------------------------------------------------------
  # Register each file group as one logical table without collecting its rows.
  db_source <- sedonadb::sd_read_parquet(path)
  sedonadb::sd_to_view(db_source, view)
  # --------------------------------------------------------------------------
  # Add the view to cleanup only after registration succeeds.
  c(all, view)
}
