################################################################################
#' Build an extraction union query
#'
#' @description
#' Construct SQL that stacks GEOS extraction views using `UNION ALL`, retaining
#' the location identifier, `time`, `level`, and selected variable columns.
#'
#' @param locs_id character(1). SQL identifier for the location column, already
#' quoted if necessary.
#' @param locs_id character(1). SQL identifier for the time column, already
#' quoted if necessary.
#' @param columns character(1). SQL identifier for the variable columns, already
#'   quoted if necessary.
#' @param views character. Non-empty vector of SQL view names to stack, in query
#'   order. Names are inserted verbatim and must already be valid SQL
#'   identifiers.
#'
#' @details
#' Every view must contain the columns identified by `l` and `var`, together
#' with columns named `time` and `level`. The query retains duplicate rows.
#' This helper builds SQL text only; it does not execute the query or validate
#' the views.
#'
#' @return A character(1) SQL query.
#' @seealso [sedona_geos()], [quote_sql()]
#' @keywords internal
#' @examples
#' amadeusdb::union_views(
#'   locs_id = '"site_id"',
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
