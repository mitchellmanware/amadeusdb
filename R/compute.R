################################################################################
#' Compute and register a SedonaDB view
#'
#' @description
#' Execute a parameterized SQL query, materialize its result, and register the
#' result as a view in the default SedonaDB context. Return an updated vector
#' of view names for the caller to track during cleanup.
#'
#' @param sql character(1). SQL query passed to [sedonadb::sd_sql()]. Any source
#'   views referenced by the query must already be registered.
#' @param params list. Query parameter values passed to [sedonadb::sd_sql()].
#'   For positional placeholders such as `$1` and `$2`, supply values in the
#'   corresponding order.
#' @param view character(1). Name of the view to register. Passed to
#'   [sedonadb::sd_to_view()] with `overwrite = TRUE`, replacing any existing
#'   view with the same name.
#' @param all character. View names already tracked by the caller for cleanup.
#'   Supply `character(0)` when no views have been registered yet.
#'
#' @details
#' Query execution through [sedonadb::sd_sql()] is followed by
#' [sedonadb::sd_compute()] before registration. No explicit argument
#' validation is performed; errors from these operations propagate to the
#' caller.
#'
#' After registration, the local data-frame binding is removed, while the
#' registered view remains available for subsequent SQL queries. The view name
#' is appended to `all` only after successful registration. Duplicate names
#' are removed, preserving their first occurrence in the combined vector.
#'
#' The caller must assign the returned vector to update its cleanup tracking.
#' View removal remains the caller's responsibility, typically through
#' [batch_drop()] after writing or [cleanup_views()] on exit.
#'
#' @return
#' A character vector equivalent to `unique(c(all, view))`. The query result
#' remains registered in SedonaDB; the returned value contains view names.
#'
#' @importFrom sedonadb sd_sql sd_compute sd_to_view
#' @seealso [register_locs()], [batch_write()], [batch_drop()],
#'   [cleanup_views()], [sedona_geos()], [sedona_hms()]
#' @keywords internal
#' @examples
#' \dontrun{
#' chr_views <- compute_view(
#'   sql = "SELECT CAST($1 AS INTEGER) AS value",
#'   params = list(1L),
#'   view = "db_compute_example",
#'   all = character(0)
#' )
#' cleanup_views(chr_views)
#' }
#' @export
compute_view <- function(
  sql,
  params,
  view,
  all
) {
  # ------------------------------------------------------------------------
  # Register and compute the spatial join.
  db_time <- sedonadb::sd_sql(sql = sql, params = params) |>
    sedonadb::sd_compute()
  # ------------------------------------------------------------------------
  # Register post-spatial join data in the database.
  sedonadb::sd_to_view(
    db_time,
    view,
    overwrite = TRUE
  )
  # ------------------------------------------------------------------------
  # Remove time-ly database.
  rm(db_time)
  # ------------------------------------------------------------------------
  # Append view to vector of views.
  unique(c(all, view))
}
