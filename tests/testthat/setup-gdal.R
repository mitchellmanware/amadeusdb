# ------------------------------------------------------------------------------
# Read the GDAL library override and active Conda environment path.
chr_gdal <- Sys.getenv("AMADEUSDB_GDAL_LIBRARY")
chr_conda <- Sys.getenv("CONDA_PREFIX")

# ------------------------------------------------------------------------------
# Fall back to the Linux Conda GDAL library when no override is provided.
if (!nzchar(chr_gdal) && nzchar(chr_conda)) {
  chr_gdal <- file.path(chr_conda, "lib", "libgdal.so")
}

# ------------------------------------------------------------------------------
# Require a nonempty path pointing to an existing GDAL library.
if (!nzchar(chr_gdal) || !file.exists(chr_gdal)) {
  stop("Set AMADEUSDB_GDAL_LIBRARY to the GDAL shared library path.")
}

# ------------------------------------------------------------------------------
# Load GDAL with symbols visible to other native libraries.
invisible(dyn.load(chr_gdal, local = FALSE, now = TRUE))

# ------------------------------------------------------------------------------
# Verify that the GDALOpenEx symbol is available.
stopifnot(is.loaded("GDALOpenEx"))
