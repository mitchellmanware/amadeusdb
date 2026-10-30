################################################################################
#' Read and interpolate a packaged SQL template
#'
#' @description
#' Read a SQL template from the package's `sql` directory and interpolate
#' supplied values into its brace-delimited expressions using
#' [glue::glue_data()]. Return the resulting SQL text for later execution.
#'
#' @param file character(1). Template filename relative to the installed
#'   package's `sql` directory, for example `"sedona_hms.sql"`. Templates are
#'   stored in `inst/sql/` in the source package. An error is raised if the
#'   requested template cannot be found.
#' @param args Named list or environment supplying values for template
#'   expressions such as `{locs_id_sql}`. Use scalar values to produce a
#'   single SQL string. SQL identifiers should already be quoted when needed.
#'
#' @details
#' The template is located with [base::system.file()] using
#' `package = "amadeusdb"` and `mustWork = TRUE`. Its lines are read with
#' [base::readLines()] and joined with newline characters before interpolation.
#' Use doubled braces (`{{` and `}}`) for literal braces in the template.
#'
#' Interpolation uses [glue::glue_data()] and inserts values as text without
#' automatic SQL identifier quoting or literal escaping. Use [quote_sql()]
#' when preparing identifier substitutions. Query parameter placeholders such
#' as `$1` remain in the SQL text; supply their values separately through
#' the `params` argument of [compute_view()] or [sedonadb::sd_sql()].
#'
#' This helper reads and formats SQL only. It does not execute the query,
#' register database views, or validate the generated SQL.
#'
#' @return
#' A `glue` character vector containing the interpolated SQL. With scalar
#' substitution values, the result contains one SQL string.
#'
#' @importFrom glue glue_data
#' @seealso [quote_sql()], [compute_view()], [sedona_hms()],
#'   [glue::glue_data()]
#' @keywords internal
#' @examples
#' sql_processhms <- amadeusdb::glue_sql(
#'   file = "sedona_hms.sql",
#'   args = list(
#'     locs_id_sql = quote_sql("site_id"),
#'     light_sql = quote_sql("light_00000"),
#'     medium_sql = quote_sql("medium_00000"),
#'     heavy_sql = quote_sql("heavy_00000")
#'   )
#' )
#' @export
glue_sql <- function(
  file,
  args
) {
  glue::glue_data(
    paste(
      readLines(
        system.file(
          "sql",
          file,
          package = "amadeusdb",
          mustWork = TRUE
        ),
        warn = FALSE
      ),
      collapse = "\n"
    ),
    .x = args
  )
}
