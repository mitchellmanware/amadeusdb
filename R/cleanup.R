################################################################################
#' Remove temporary views
#'
#' Removes views in reverse order, continuing if an individual removal fails.
#'
#' @param views Character vector of view names, typically in creation order.
#' @param drop_view Function accepting a single view name and removing that
#' view. Defaults to \code{sedonadb::sd_drop_view}.
#'
#' @details
#' Failed removals produce a message containing the view name and error details.
#' An empty vector requires no cleanup.
#'
#' Register this helper with \code{on.exit()} inside the function that creates
#' the views.
#'
#' @return \code{NULL}, invisibly.
#'
#' @keywords internal
#' @export
cleanup_views <- function(
  views,
  drop_view = sedonadb::sd_drop_view
) {
  # ----------------------------------------------------------------------------
  # Remove transient database views upon early or proper exit from function.
  for (h in rev(views)) {
    tryCatch(
      drop_view(h),
      error = function(e) {
        message("Could not remove view ", h, ": ", conditionMessage(e))
      }
    )
  }
  # ----------------------------------------------------------------------------
  # Return NULL.
  invisible(NULL)
}
