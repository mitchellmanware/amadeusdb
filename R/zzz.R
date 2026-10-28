################################################################################
#' Store internal package data
#'
#' @description
#' Private environment containing data initialized when the amadeusdb namespace
#' is loaded. Package functions use this environment to access the source
#' coordinate reference system (CRS) lookup table.
#'
#' @format
#' An environment with an empty parent environment. After [.onLoad()] runs,
#' `df_crs` contains the data frame read from `extdata/df_crs.csv`, with all
#' columns stored as character vectors.
#'
#' @seealso [.onLoad()], [prepare_sf()]
#' @keywords internal
.amadeusdb <- new.env(parent = emptyenv())

################################################################################
#' Load the source CRS lookup table
#'
#' @description
#' Initialize `.amadeusdb$list_crs` from the RDS file installed with the
#' package. R calls this hook automatically when the amadeusdb namespace is
#' loaded, including through `library(amadeusdb)` or access with `amadeusdb::`.
#'
#' @param libname character(1). Library directory containing the installed
#'   package. Supplied by R when the namespace is loaded.
#' @param pkgname character(1). Name of the package being loaded. Supplied by R
#'   when the namespace is loaded.
#'
#' @details
#' Locate `extdata/list_crs.rds` in the specified package installation and read
#' it with [base::readRDS()]. The resulting list is stored in the private
#' [.amadeusdb] environment for use by package functions such as [prepare_sf()].
#'
#' @return
#' Invisibly, the list assigned to `.amadeusdb$list_crs`. The hook is called
#' for its initialization side effect.
#'
#' @seealso [.amadeusdb], [prepare_sf()], [base::.onLoad()]
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  .amadeusdb$list_crs <- base::readRDS(
    system.file(
      "extdata",
      "list_crs.rds",
      package = pkgname,
      lib.loc = libname,
      mustWork = TRUE
    )
  )
}
