################################################################################
# Manual script for creating GEOS fixture data for unit/integration tests.
# Mitchell Manware
# 27 October 2026

# ------------------------------------------------------------------------------
# Load libraries.
library(ncdf4)

# ------------------------------------------------------------------------------
# Function to extract spatial and variable subset and create new .nc4 file.
# Written by Codex.
fixture_geos <- function(input, output, bbox, variable = "O3") {
  # ----------------------------------------------------------------------------
  stopifnot(
    normalizePath(input) != normalizePath(output, mustWork = FALSE)
  )
  # ----------------------------------------------------------------------------
  src <- ncdf4::nc_open(input)
  on.exit(ncdf4::nc_close(src), add = TRUE)
  # ----------------------------------------------------------------------------
  v <- src$var[[variable]]
  if (is.null(v)) {
    stop("Variable not found: ", variable)
  }
  # ----------------------------------------------------------------------------
  dim_names <- vapply(v$dim, function(d) d$name, character(1))
  stopifnot(all(c("lon", "lat") %in% dim_names))
  # ----------------------------------------------------------------------------
  # Select coordinates in their original order.
  indices <- lapply(v$dim, function(d) {
    idx <- switch(
      d$name,
      lon = which(
        d$vals >= bbox[["xmin"]] &
          d$vals <= bbox[["xmax"]]
      ),
      lat = which(
        d$vals >= bbox[["ymin"]] &
          d$vals <= bbox[["ymax"]]
      ),
      seq_len(d$len)
    )
    if (!length(idx)) {
      stop("Empty selection for dimension: ", d$name)
    }
    if (any(diff(idx) != 1L)) {
      stop("Selection must be contiguous")
    }
    idx
  })
  # ----------------------------------------------------------------------------
  start <- vapply(indices, min, integer(1))
  count <- lengths(indices)
  # ----------------------------------------------------------------------------
  # Keep singleton time/level dimensions and stored values.
  values <- ncdf4::ncvar_get(
    src,
    variable,
    start = start,
    count = count,
    collapse_degen = FALSE,
    raw_datavals = TRUE
  )
  # ----------------------------------------------------------------------------
  dims <- lapply(seq_along(v$dim), function(j) {
    d <- v$dim[[j]]
    ncdf4::ncdim_def(
      name = d$name,
      units = d$units,
      vals = d$vals[indices[[j]]],
      unlim = d$unlim,
      create_dimvar = d$create_dimvar
    )
  })
  # ----------------------------------------------------------------------------
  fill <- ncdf4::ncatt_get(src, variable, "_FillValue")
  # ----------------------------------------------------------------------------
  definition <- ncdf4::ncvar_def(
    name = variable,
    units = v$units,
    dim = dims,
    missval = if (fill$hasatt) fill$value else NULL,
    longname = v$longname,
    prec = v$prec
  )
  # ----------------------------------------------------------------------------
  dst <- ncdf4::nc_create(output, definition, force_v4 = TRUE)
  on.exit(ncdf4::nc_close(dst), add = TRUE)
  # ----------------------------------------------------------------------------
  ncdf4::ncvar_put(dst, variable, values)
  # ----------------------------------------------------------------------------
  # Copy variable and coordinate attributes, including time encoding.
  for (id in c(variable, dim_names)) {
    attributes <- ncdf4::ncatt_get(src, id)
    for (attribute in setdiff(names(attributes), "_FillValue")) {
      ncdf4::ncatt_put(dst, id, attribute, attributes[[attribute]])
    }
  }
  # ----------------------------------------------------------------------------
  invisible(output)
}

# ------------------------------------------------------------------------------
# Perform fixture creation for aqc and chm files.
chr_geos <- c("aqc", "chm")

# ------------------------------------------------------------------------------
for (g in chr_geos) {
  # ----------------------------------------------------------------------------
  # Identify NetCDF file names.
  chr_base <- list.files(
    paste0("tests/testdata/geos/full/", g),
    recursive = TRUE
  )
  # ----------------------------------------------------------------------------
  # Append file names to paths for full and subset data file path.s
  chr_full <- paste0("tests/testdata/geos/full/", g, "/", chr_base)
  chr_sub <- paste0("tests/testdata/geos/", g, "/", chr_base)
  # ----------------------------------------------------------------------------
  for (b in seq_along(chr_base)) {
    fixture_geos(
      input = chr_full[b],
      output = chr_sub[b],
      bbox = c(
        xmin = -79.625,
        xmax = -78.625,
        ymin = 35.625,
        ymax = 36.125
      ),
      variable = "O3"
    )
  }
}

# ------------------------------------------------------------------------------
# Test read.
terra::rast(chr_sub[1])
