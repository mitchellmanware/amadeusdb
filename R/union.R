################################################################################
#' Build an extraction union query
#'
#' @description
#' Construct SQL that stacks extraction views using `UNION ALL`, retaining
#' the specified location, temporal, and covariate columns.
#'
#' @param locs_id character(1). SQL identifier for the location column, already
#'   quoted if necessary.
#' @param time_id character(1). SQL identifier or comma-separated SQL column
#'   list for temporal and related fields. Defaults to `"time"`. Use `"date"`
#'   for daily results or `"time, level"` to retain a vertical level column.
#'   Individual identifiers must already be quoted if necessary.
#' @param columns character(1). Comma-separated SQL list of covariate columns
#'   to retain. Individual identifiers must already be quoted if necessary.
#' @param views character. Non-empty vector of SQL view names to stack, in query
#'   order. Names are inserted verbatim and must already be valid SQL
#'   identifiers.
#'
#' @details
#' Every view must expose the columns selected by `locs_id`, `time_id`, and
#' `columns`, with compatible types. A `level` column is selected only when
#' explicitly included in a column list. The query retains duplicate rows
#' and does not specify an output row order.
#'
#' This helper builds SQL text only; it does not execute the query, validate
#' the views, or add quoting to identifiers or column lists.
#'
#' @return A character(1) SQL query.
#' @seealso [batch_write()], [sedona_geos()], [sedona_hms()], [quote_sql()]
#' @keywords internal
#' @examples
#' amadeusdb::union_views(
#'   locs_id = '"site_id"',
#'   time_id = "time, level",
#'   columns = '"co_0"',
#'   views = c("db_time_001", "db_time_002")
#' )
#' @export
union_views <- function(
  locs_id,
  time_id = "time",
  columns,
  views
) {
  paste0(
    sprintf(
      "SELECT %s, %s, %s FROM %s",
      locs_id,
      time_id,
      columns,
      views
    ),
    collapse = "\nUNION ALL\n"
  )
}
