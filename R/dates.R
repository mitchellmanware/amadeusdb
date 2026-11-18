#' Extract the first and last date elements
#'
#' @description
#' Select the first and last elements of a date vector or list, preserving
#' their input order.
#'
#' @param dates A non-empty vector or list of dates, typically a character
#'   vector returned by [amadeus::sequence_dates()].
#'
#' @details
#' Dates are neither sorted nor validated. A single element is repeated so
#' that the result contains two elements. Input names are retained.
#'
#' @return A two-element vector or list, as produced by concatenating the
#'   first and last subsets of `dates`. Character input returns a character
#'   vector; list input returns a list. No character coercion is performed.
#'
#' @seealso [amadeus::sequence_dates()], [sedona_geos()], [sedona_hms()]
#' @keywords internal
#' @examples
#' fl_dates(c("2024-01-01", "2024-01-02", "2024-01-03"))
#' fl_dates("2024-01-01")
#' @export
fl_dates <- function(dates) c(dates[1], dates[length(dates)])
