################################################################################
#' Prepare extraction locations for SedonaDB calculations
#'
#' @description
#' Validate the location identifier column, convert locations to an `sf` object,
#' transform them to the requested coordinate reference system (CRS), and apply
#' an optional buffer. The result can be supplied as `locs` to `sedona_*()`
#' functions.
#'
#' @param locs sf, SpatVector, or data.frame object. Extraction locations with
#'   an identifier column named by `locs_id`. Spatial objects must have a source
#'   CRS. Data frames must contain numeric `lon` and `lat` columns in longitude
#'   and latitude degrees, interpreted as `"EPSG:4326"`.
#' @param locs_id character(1). Name of the location identifier column in
#'   `locs`. The column is checked for presence; its values are not checked for
#'   uniqueness or missingness.
#' @param crs character(1). Non-empty target CRS description passed to
#'   [sf::st_transform()] after the validation in [prepare_sf()].
#' @param radius numeric(1). Non-negative buffer distance. For projected CRSs,
#'   use the CRS units; for longitude/latitude coordinates, distance units
#'   follow [sf::st_buffer()]. Defaults to `0`, which skips buffering.
#'
#' @return
#' An `sf` object with the location attributes and geometry transformed to
#' `crs`, buffered when `radius > 0`.
#'
#' @note
#' CRS validation is delegated to [prepare_sf()]; see its note about the
#' current file-path comparison.
#'
#' @importFrom checkmate assert_false assert_subset
#'
#' @seealso [amadeus::calc_prepare_locs()] for the corresponding base amadeus
#'   helper, [prepare_sf()], [prepare_radius()]
#' @export
prepare_locs <- function(
  locs,
  locs_id,
  crs,
  radius = 0
) {
  # ----------------------------------------------------------------------------
  # Validate non-NULL inputs.
  checkmate::assert_false(is.null(locs))
  checkmate::assert_false(is.null(locs_id))
  checkmate::assert_false(is.null(crs))
  checkmate::assert_false(is.null(radius))
  # ----------------------------------------------------------------------------
  # Ensure `locs_id` is a column in `locs`
  checkmate::assert_subset(locs_id, names(locs))
  # ----------------------------------------------------------------------------
  # Extraction locations as `sf` object.
  amadeusdb::prepare_sf(locs = locs, crs = crs, radius = radius)
}

################################################################################
#' Convert, project, and buffer extraction locations as sf
#'
#' @description
#' Convert supported location objects to `sf`, transform their geometry to the
#' requested CRS, and apply a buffer with [prepare_radius()].
#'
#' @param locs sf, SpatVector, or data.frame object. Spatial objects must have
#'   a source CRS. Data frames must contain numeric `lon` and `lat` columns in
#'   longitude and latitude degrees, interpreted as `"EPSG:4326"`.
#' @param crs character(1). Non-empty target CRS description passed to
#'   [sf::st_transform()]. See the note below for the current validation.
#' @param radius numeric(1). Non-negative buffer distance. For projected CRSs,
#'   use the CRS units; for longitude/latitude coordinates, distance units
#'   follow [sf::st_buffer()]. Use `0` to skip buffering.
#'
#' @details
#' Existing `sf` objects retain their attributes during transformation.
#' `SpatVector` objects are converted with [sf::st_as_sf()]; conversion warnings
#' are suppressed for objects with no rows. Data frames become point features,
#' retaining their original `lon` and `lat` columns. Those columns are not
#' updated when the geometry is transformed or buffered.
#'
#' @return
#' An `sf` object with the location attributes and geometry transformed to
#' `crs`, buffered when `radius > 0`.
#'
#' @note
#' The current CRS check compares `crs` with the path returned by
#' `system.file("amadeusdb/crs.csv", package = "amadeusdb")`, instead of reading
#' CRS values from that file. Standard CRS identifiers therefore fail this
#' check.
#'
#' @importFrom checkmate assert_number assert_string assert_subset
#' @importFrom methods is
#' @importFrom sf st_as_sf st_crs st_transform
#'
#' @seealso [amadeus::process_locs_vector()] for the corresponding base amadeus
#'   helper, [prepare_locs()], [prepare_radius()]
#' @export
prepare_sf <- function(
  locs,
  crs,
  radius
) {
  # ----------------------------------------------------------------------------
  # Validate numeric radius equal to or greater than 0.
  checkmate::assert_number(radius, lower = 0)
  # ----------------------------------------------------------------------------
  # Validate CRS matching one in known list of CRSs.
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_true(any(vapply(
    .amadeusdb$list_crs,
    function(x) isTRUE(crs == x$crs),
    logical(1)
  )))
  # ----------------------------------------------------------------------------
  if (methods::is(locs, "sf")) {
    # --------------------------------------------------------------------------
    # If already `sf` object, retain as is.
    sf_locs <- locs
  } else if (methods::is(locs, "SpatVector")) {
    # --------------------------------------------------------------------------
    # If `terra` SpatVector object, convert to `sf`.
    sf_locs <- if (nrow(locs) == 0L) {
      message("Supplied `locs` has no rows; returning empty `sf`.")
      suppressWarnings(sf::st_as_sf(locs))
    } else {
      sf::st_as_sf(locs)
    }
  } else if (methods::is(locs, "data.frame")) {
    # --------------------------------------------------------------------------
    # Validate "lon" and "lat" are in the data.frame to create coordinates.
    checkmate::assert_subset(c("lon", "lat"), names(locs))
    # --------------------------------------------------------------------------
    # If `data.frame` object, create an `sf` from lat and lon coordinates.
    sf_locs <- sf::st_as_sf(
      data.frame(locs),
      coords = c("lon", "lat"),
      crs = sf::st_crs("EPSG:4326"),
      remove = FALSE
    )
  } else {
    # --------------------------------------------------------------------------
    # Error if not one of `sf`, `SpatVector`, or `data.frame`.
    stop(
      paste0(
        "`locs` is not a `SpatVector`, `sf`, or `data.frame` object.\n"
      )
    )
  }
  # ----------------------------------------------------------------------------
  # Project to CRS defined by `crs`, after validation that `crs` matches one of
  # manually complied source CRS list.
  if (!sf::st_crs(sf_locs) == crs) {
    message(
      "Projecting extraction locations from ",
      sf::st_crs(locs)$input,
      " (current) to ",
      crs$input,
      " (data source CRS)."
    )
  }
  sf_locs_proj <- sf::st_transform(
    sf_locs,
    crs
  )
  # ----------------------------------------------------------------------------
  # Buffer locations according to `radius`, assumed that user selects `radius`
  # value with knowledge of the data's CRS.
  amadeusdb::prepare_radius(sf_locs_proj, radius)
}

################################################################################
#' Apply an optional buffer to extraction locations
#'
#' @description
#' Return locations unchanged when `radius` is zero, or buffer their geometry
#' with [sf::st_buffer()] when `radius` is positive.
#'
#' @param locs sf object. Extraction locations already transformed to the CRS
#'   in which buffering should be performed.
#' @param radius numeric(1). Non-negative buffer distance. For projected CRSs,
#'   use the CRS units; for longitude/latitude coordinates, distance units
#'   follow [sf::st_buffer()]. Use `0` to skip buffering.
#'
#' @details
#' A zero radius bypasses [sf::st_buffer()] to preserve the original geometry
#' type. Positive radii are passed to [sf::st_buffer()] with `nQuadSegs = 180L`.
#' This helper assumes valid inputs; [prepare_sf()] validates the radius.
#'
#' @return
#' The input `sf` object when `radius = 0`, or an `sf` object with buffered
#' geometry and the original attributes when `radius > 0`.
#'
#' @importFrom sf st_buffer
#'
#' @seealso [amadeus::process_locs_radius()] for the corresponding base amadeus
#'   helper, [prepare_locs()], [prepare_sf()]
#' @examples
#' locs <- sf::st_as_sf(
#'   data.frame(site_id = "001", lon = -78.90, lat = 35.97),
#'   coords = c("lon", "lat"),
#'   crs = 4326
#' )
#' locs <- sf::st_transform(locs, crs = 32617)
#' prepare_radius(locs, radius = 100)
#' @export
prepare_radius <- function(
  locs,
  radius
) {
  # --------------------------------------------------------------------------
  if (radius == 0) {
    # ------------------------------------------------------------------------
    # Buffering with 0L radius will still convert points to polygons, so
    # locations are explicitly returned without buffering if `radius == 0`.
    locs
  } else if (radius > 0) {
    # ------------------------------------------------------------------------
    # Apply `radius` buffer to locations, assumed that user selects `radius`
    # value with knowledge of the data's CRS.
    sf::st_buffer(
      locs,
      radius,
      nQuadSegs = 180L
    )
  }
}
