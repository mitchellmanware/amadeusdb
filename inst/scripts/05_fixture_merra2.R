################################################################################
# Manual script for creating MERRA-2 fixture data for unit, integration, and
# alignment tests. Adapted from inst/scripts/02_fixture_geos.R.
#
# Run from the amadeusdb package root in a fresh R session:
# Rscript --vanilla inst/scripts/05_fixture_merra2.R
# Do not load amadeusdb or sedonadb in this session; the reference calculations
# use terra through the core amadeus package.
#
# Place original daily NetCDF files in tests/testdata/merra2/full/<collection>/.
# This script writes spatial subsets to tests/testdata/merra2/<collection>/,
# copies them into merra2/all/ for the mixed-collection check, and writes core
# amadeus reference outputs to tests/testdata/amadeus/merra2/.
# Rerunning the script replaces these generated fixtures.
# Core amadeus names the 2D value column T2M_0; sedona_merra2 uses t2m_0.
# Alignment tests should select T2M and normalize that column name when comparing.

# ------------------------------------------------------------------------------
# Load the NetCDF library independently of the raster extraction libraries.
library(ncdf4)

# ------------------------------------------------------------------------------
# Extract one 2D spatial variable, retaining every time slice in stored order.
# The subsetting and attribute-copying logic is reused from fixture_geos().
fixture_merra2 <- function(input, output, bbox, variable = "T2M") {
  # ----------------------------------------------------------------------------
  stopifnot(
    file.exists(input),
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
  if (length(dim_names) != 3L || !setequal(dim_names, c("lon", "lat", "time"))) {
    stop("Fixture variable must have only lon, lat, and time dimensions.")
  }
  # ----------------------------------------------------------------------------
  # Select spatial coordinates in their original order; keep the full day.
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
  # Preserve dimension order and stored values, including encoded missing data.
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
  definition <- ncdf4::ncvar_def(
    name = variable,
    units = v$units,
    dim = dims,
    missval = if (fill$hasatt) fill$value else NULL,
    longname = v$longname,
    prec = v$prec,
    compression = 4
  )
  # ----------------------------------------------------------------------------
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  dst <- ncdf4::nc_create(output, definition, force_v4 = TRUE)
  on.exit(ncdf4::nc_close(dst), add = TRUE)
  # ----------------------------------------------------------------------------
  ncdf4::ncvar_put(dst, variable, values)
  # ----------------------------------------------------------------------------
  # Copy variable and coordinate attributes, including time units and calendar.
  # Write packed values before adding any scale_factor or add_offset attributes.
  for (id in c(variable, dim_names)) {
    attributes <- ncdf4::ncatt_get(src, id)
    for (attribute in setdiff(names(attributes), "_FillValue")) {
      value <- attributes[[attribute]]
      # The integer time coordinate has floating-point limits near 1e15.
      # Use the attribute's value type instead of the coordinate's storage type.
      ncdf4::ncatt_put(
        dst, id, attribute, value,
        prec = if (is.double(value)) "double" else NA
      )
    }
  }
  # ----------------------------------------------------------------------------
  invisible(output)
}

################################################################################
# NetCDF fixture generation.
# ------------------------------------------------------------------------------
# Use the same date and location fixtures as the MERRA-2 tests.
chr_dates <- "2018-01-01"
chr_merra2path <- file.path("tests", "testdata", "merra2")
chr_locsid <- "NAME"

# ------------------------------------------------------------------------------
# MERRA-2 grid spacing is 0.625 degrees longitude by 0.5 degrees latitude.
# These four by four cell centers cover the point and polygon fixtures with
# a margin, preserving their neighboring cells for extraction comparisons.
num_bbox <- c(xmin = -80, xmax = -78.125, ymin = 35, ymax = 36.5)

# ------------------------------------------------------------------------------
# T2M is used for extraction and alignment tests. A second collection supplies
# the mixed-collection failure fixture. O3 is not a variable in these files;
# the chemistry collection contains total-column ozone, TO3.
chr_variables <- c(tavg1_2d_slv_Nx = "T2M", tavg1_2d_chm_Nx = "TO3")
chr_all <- file.path(chr_merra2path, "all")
dir.create(chr_all, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
for (collection in names(chr_variables)) {
  chr_base <- paste0(
    "MERRA2_400.", collection, ".", gsub("-", "", chr_dates), ".nc4"
  )
  chr_full <- file.path(chr_merra2path, "full", collection, chr_base)
  chr_sub <- file.path(chr_merra2path, collection, chr_base)
  if (!file.exists(chr_full)) {
    stop("Missing original MERRA-2 file: ", chr_full, call. = FALSE)
  }
  fixture_merra2(
    input = chr_full,
    output = chr_sub,
    bbox = num_bbox,
    variable = chr_variables[[collection]]
  )
  # ----------------------------------------------------------------------------
  # Keep original filenames so filename-based collection checks remain valid.
  stopifnot(file.copy(chr_sub, chr_all, overwrite = TRUE))
}

################################################################################
# Core amadeus reference outputs for alignment tests.
# ------------------------------------------------------------------------------
library(amadeus)

# ------------------------------------------------------------------------------
# Process the cropped T2M file once and reuse the complete hourly raster.
chr_merra2slv <- file.path(chr_merra2path, "tavg1_2d_slv_Nx")
chr_reference <- file.path("tests", "testdata", "amadeus", "merra2")
dir.create(chr_reference, recursive = TRUE, showWarnings = FALSE)
r_merra2 <- amadeus::process_merra2(
  date = chr_dates,
  path = chr_merra2slv,
  variable = "T2M",
  daily_agg = FALSE
)

# ------------------------------------------------------------------------------
# Check that cropping preserved the native hourly schedule, including :30.
time_expected <- seq(
  as.POSIXct(paste(chr_dates, "00:30:00"), tz = "UTC"),
  by = "hour",
  length.out = 24L
)
stopifnot(
  terra::nlyr(r_merra2) == 24L,
  identical(as.numeric(terra::time(r_merra2)), as.numeric(time_expected))
)

# ------------------------------------------------------------------------------
polygons <- sf::st_read(
  file.path("tests", "testdata", "locs", "polygons.shp"),
  quiet = TRUE
)
points <- sf::st_read(
  file.path("tests", "testdata", "locs", "points.shp"),
  quiet = TRUE
)

# ------------------------------------------------------------------------------
# Polygon mean, maximum, and minimum references use the core implementation
# directly. Preserve its output types and column names for comparison.
for (fun in c("mean", "max", "min")) {
  df_procalc <- amadeus::calculate_merra2(
    from = r_merra2,
    locs = polygons,
    locs_id = chr_locsid,
    radius = 0,
    geom = FALSE,
    fun = fun
  )
  stopifnot(
    nrow(df_procalc) == nrow(polygons) * length(time_expected),
    identical(names(df_procalc), c(chr_locsid, "time", "T2M_0")),
    all(is.finite(df_procalc$T2M_0)),
    setequal(as.numeric(df_procalc$time), as.numeric(time_expected))
  )
  saveRDS(
    df_procalc,
    file.path(chr_reference, paste0("df_procalc_", fun, ".rds"))
  )
}

# ------------------------------------------------------------------------------
# Point reference: one raster cell value per location and time slice.
df_procalc_points <- amadeus::calculate_merra2(
  from = r_merra2,
  locs = points,
  locs_id = chr_locsid,
  radius = 0,
  geom = FALSE
)
stopifnot(
  nrow(df_procalc_points) == nrow(points) * length(time_expected),
  identical(names(df_procalc_points), c(chr_locsid, "time", "T2M_0")),
  all(is.finite(df_procalc_points$T2M_0)),
  setequal(as.numeric(df_procalc_points$time), as.numeric(time_expected))
)
saveRDS(
  df_procalc_points,
  file.path(chr_reference, "df_procalc_points.rds")
)
