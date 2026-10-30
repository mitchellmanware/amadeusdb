################################################################################
#' Validate shared SedonaDB extraction arguments
#'
#' @description
#' Check prepared locations, dates, input and output paths, and batch sizes
#' supplied by `sedona_*()` functions. Check raster-specific arguments only
#' when `raster = TRUE`.
#'
#' @param locs sf object. Locations returned by [prepare_locs()], with a column
#'   named `geometry` and exactly one geometry type: `POINT`, `POLYGON`, or
#'   `MULTIPOLYGON`.
#' @param date character(2). Start and end dates after the caller has repeated a
#'   single date. Only storage type and length are checked; date values,
#'   missingness, and chronological order are not checked.
#' @param input character(1). Non-empty path to an existing input directory.
#'   The immediate entries listed by [list.files()] must contain at least one
#'   file and no directories. This check does not search subdirectories or
#'   select source-specific filenames.
#' @param output character(1). Non-empty output directory path. Created
#'   recursively if absent, then checked for existence.
#' @param batch integer. Number of source files per batch, with values at least
#'   `1L`.
#' @param raster logical(1). Whether to check the raster-specific arguments.
#'   This flag is used directly in an `if` condition without an assertion.
#' @param fun character(1). Non-empty raster summary statistic name. Checked
#'   only when `raster = TRUE`; supported names are resolved by the caller.
#' @param variable character(1). Non-empty raster variable name. Checked only
#'   when `raster = TRUE`; existence in the source files is not checked.
#' @param band integer. Raster band or vertical level index, with values at
#'   least `1L`. Checked only when `raster = TRUE`.
#' @param touches logical(1). Non-missing flag controlling inclusion of raster
#'   cells touched by polygons. Checked only when `raster = TRUE`.
#' @param exclude_nodata logical(1). Non-missing flag controlling exclusion of
#'   raster NoData cells. Checked only when `raster = TRUE`.
#'
#' @details
#' This helper does not coerce inputs. The integer checks do not enforce
#' scalar `batch` or `band` values or reject missing integers. The arguments
#' `fun`, `variable`, `band`, `touches`, and `exclude_nodata` may be omitted
#' when `raster = FALSE`, because they are not evaluated in that branch.
#'
#' Creating `output` is a side effect and can occur before a later check fails.
#'
#' @return
#' Called for validation and directory creation; failures raise an error.
#' On successful completion, invisibly returns `exclude_nodata` when
#' `raster = TRUE`, or `NULL` otherwise.
#'
#' @importFrom checkmate assert_choice assert_class assert_directory_exists
#'   assert_file assert_flag assert_integer assert_string assert_subset
#' @importFrom sf st_geometry_type
#'
#' @seealso \code{\link{check_views}()}, [prepare_locs()], [sedona_geos()],
#'   [sedona_hms()]
#' @keywords internal
#' @export
check_arguments <- function(
  locs,
  date,
  input,
  output,
  batch,
  raster,
  fun,
  variable,
  band,
  touches,
  exclude_nodata
) {
  # ----------------------------------------------------------------------------
  # `locs` (passed from internal `prepare_locs`)
  # ----------------------------------------------------------------------------
  # Ensure locations are an `sf` object.
  checkmate::assert_class(locs, "sf")
  # ----------------------------------------------------------------------------
  # Ensure the "geometry" column is retained in the locations object.
  checkmate::assert_subset("geometry", names(locs))
  # ----------------------------------------------------------------------------
  # Ensure the `sf` object has POINT, POLYGON, or MULTIPOLYGON geometry.
  chr_locsgeom <- as.character(unique(sf::st_geometry_type(locs)))
  checkmate::assert_choice(chr_locsgeom, c("POINT", "POLYGON", "MULTIPOLYGON"))
  # ----------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # Ensure the `date` vector contains two dates after coersion (rep(...)).
  checkmate::assert_character(date, len = 2)
  # ----------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # `input`
  # ----------------------------------------------------------------------------
  # Ensure `input` is a character.
  checkmate::assert_string(input, min.chars = 1L)
  # ----------------------------------------------------------------------------
  # Ensure `input` directory exists.
  checkmate::assert_directory_exists(input)
  # ----------------------------------------------------------------------------
  # Ensure `input` contains files.
  checkmate::assert_file(list.files(input, full.names = TRUE))
  # ----------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # `output`
  # ----------------------------------------------------------------------------
  amadeusdb::check_output(output)
  # ----------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # Ensure `batch` is an integer value.
  checkmate::assert_integer(batch, lower = 1)
  # ----------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # Arguments `fun`, `variable`, `band`, `touches`, and `exclude_nodata` are
  # used in raster extraction data only.
  if (raster) {
    # --------------------------------------------------------------------------
    # Ensure `fun` is a character.
    checkmate::assert_string(fun, min.chars = 1)
    # --------------------------------------------------------------------------

    # --------------------------------------------------------------------------
    # Ensure `variable`` is a character.
    checkmate::assert_string(variable, min.chars = 1L)
    # --------------------------------------------------------------------------

    # --------------------------------------------------------------------------
    # Ensure `band` is an integer.
    # --------------------------------------------------------------------------
    checkmate::assert_integer(band, lower = 1)
    # --------------------------------------------------------------------------

    # --------------------------------------------------------------------------
    # Ensure `touches` is TRUE/FALSE.
    # --------------------------------------------------------------------------
    checkmate::assert_flag(touches)
    # --------------------------------------------------------------------------

    # --------------------------------------------------------------------------
    # Ensure `exclude_nodata` is TRUE/FALSE.
    # --------------------------------------------------------------------------
    checkmate::assert_flag(exclude_nodata)
    # --------------------------------------------------------------------------
  }
}

################################################################################
#' Validate source-file metadata for SedonaDB views
#'
#' @description
#' Check the metadata data frames assembled by `sedona_*()` functions before
#' source files are processed and database views are created.
#'
#' @param df data.frame. At least one row, with columns named `time`, `path`,
#'   and `view`. The `path` column must contain paths to existing files. The
#'   `view` column must be character with unique, non-missing values. Additional
#'   columns are allowed.
#'
#' @details
#' The `time` column is required, but its type, values, missingness, and
#' uniqueness are not checked. View names are not checked for valid SQL
#' syntax. This helper does not create or query database views or modify `df`.
#'
#' @return
#' Invisibly, `names(df)` after successful validation. Raises an error when a
#' check fails.
#'
#' @importFrom checkmate assert_character assert_data_frame assert_file_exists
#'   assert_names
#'
#' @seealso \code{\link{check_arguments}()}, [sedona_geos()], [sedona_hms()]
#' @keywords internal
#' @export
check_views <- function(df) {
  # ----------------------------------------------------------------------------
  # Ensure `df` is a data.frame.
  checkmate::assert_data_frame(df, min.rows = 1L)
  # ----------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # Ensure views are non-missing characters.
  checkmate::assert_character(df$view, any.missing = FALSE, unique = TRUE)
  # ----------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # Ensure data paths exist.
  checkmate::assert_file_exists(df$path)
  # ----------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # Ensure `df` contains "time", "path", and "view" columns.
  checkmate::assert_names(names(df), must.include = c("time", "path", "view"))
  # ----------------------------------------------------------------------------
}

#' Check for the `output` directory, and create if it does not exist.
#'
#' @param output character(1). Output directory.
#'
#' @importFrom checkmate assert_string assert_directory_exists
#'
#' @export
check_output <- function(output) {
  # ----------------------------------------------------------------------------
  # Ensure `output` is a character.
  checkmate::assert_string(output, min.chars = 1L)
  # ----------------------------------------------------------------------------
  # Create output path if does not exist.
  if (!dir.exists(output)) {
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
  }
  # ----------------------------------------------------------------------------
  # Ensure `output` exists.
  checkmate::assert_directory_exists(output)
}
