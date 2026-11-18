sedona_merra2 <- function(
  date,
  input,
  output,
  variable,
  band = 1L,
  locs,
  locs_id,
  radius = 0,
  fun = "mean",
  touches = TRUE,
  exclude_nodata = TRUE,
  batch = 50L
) {
  # ----------------------------------------------------------------------------
  # Prepare extraction locations (internal checks for arguments).
  sf_locs <- amadeusdb::prepare_locs(
    locs = locs,
    locs_id = locs_id,
    crs = .amadeusdb$list_crs$geos$crs, # !!! replace with MERRA2 !!!
    radius = radius
  )
  # ----------------------------------------------------------------------------
  # Extract `sf` geometry type.
  chr_locsgeom <- as.character(unique(sf::st_geometry_type(sf_locs)))
  # ----------------------------------------------------------------------------
  # Coerce `date` to vector of 2 if not already.
  date <- fl_dates(amadeus::sequence_dates(date, hyphen = TRUE))
  # ----------------------------------------------------------------------------
  # Check zonal statistic function against SedonaDB supported (as of 10/21/26)
  fun <- match.arg(
    tolower(fun),
    c(
      "mean",
      "sum",
      "min",
      "max",
      "median",
      "count",
      "stddev",
      "variance",
      "mode"
    )
  )
  # ----------------------------------------------------------------------------
  # Check arguments.
  amadeusdb::check_arguments(
    locs = sf_locs,
    date = date,
    input = input,
    output = output,
    batch = batch,
    raster = TRUE,
    fun = fun,
    variable = variable,
    band = band,
    touches = touches,
    exclude_nodata = exclude_nodata
  )
  # ----------------------------------------------------------------------------
  # Identify time-relevant MERRA2 file paths in `input`.
  chr_datapaths <- amadeusdb::discover(
    date = date,
    input = input,
    pattern = "MERRA2_400.*.nc4",
    hyphen = FALSE
  )
  # ----------------------------------------------------------------------------
  # Store file path, NetCDF variable path, date, and generated db names.
  df_merra2paths <- data.frame(
    path = chr_datapaths,
    tchar = amadeus::process_collection(
      chr_datapaths,
      source = "merra2",
      date = TRUE
    )
  )
  df_merra2paths$time <- as.character(as.POSIXct(
    as.character(df_merra2paths$tchar),
    format = "%Y%m%d",
    tz = "UTC"
  ))
  df_merra2paths$var <- sprintf(
    'NETCDF:"%s":%s',
    normalizePath(df_merra2paths$path, winslash = "/", mustWork = TRUE),
    variable
  )
  df_merra2paths$view <- sprintf("db_merra2_%08s", df_merra2paths$tchar)
  # ----------------------------------------------------------------------------
  # Require one collection before reusing its vertical coordinate across files.
  chr_merra2_collection <- unique(amadeus::process_collection(
    df_merra2paths$path,
    source = "merra2",
    collection = TRUE
  ))
  if (length(chr_merra2_collection) != 1L) {
    stop("Selected MERRA2 files must belong to a single collection.")
  }
  # ----------------------------------------------------------------------------
  # Check dates, paths, and database view names.
  amadeusdb::check_views(df_merra2paths)
  # ----------------------------------------------------------------------------
  # Split all source rows before checking outputs to preserve batch membership.
  list_merra2_batch <- amadeusdb::batch_split(df_merra2paths, batch)
  # ----------------------------------------------------------------------------
  # Generate .parquet paths for every batch, including existing outputs.
  chr_merra2_parquet <- amadeusdb::batch_prep(
    df = df_merra2paths,
    batch = list_merra2_batch,
    output = output,
    source = "merra2",
    variable = variable,
    time_id = "tchar"
  )
  # ----------------------------------------------------------------------------
  # Check outputs before reading NetCDF contents or registering database views.
  lgl_merra2_skip <- vapply(
    chr_merra2_parquet,
    amadeusdb::batch_skip,
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
  if (all(lgl_merra2_skip)) {
    return(invisible(chr_merra2_parquet))
  }
  # ----------------------------------------------------------------------------
  # Read the shared coordinate once, using the first file that needs extraction.
  int_merra2_pending <- which(!lgl_merra2_skip)
  int_merra2_first <- list_merra2_batch[[int_merra2_pending[1L]]][1L]
  # num_merra2_levels <- amadeusdb::geoslev(
  #   df_merra2paths$path[int_merra2_first],
  #   "lev"
  # )
  # ----------------------------------------------------------------------------
  # Label every extraction with the coordinate of the requested raster band.
  # checkmate::assert_integer(band, upper = length(num_merra2_levels))
  # chr_merra2_level <- as.character(num_merra2_levels[band])
  # ----------------------------------------------------------------------------
  # Define location identifiers in SQL-friendly format.
  locs_id_sql <- amadeusdb::quote_sql(locs_id)
  # ----------------------------------------------------------------------------
  # Define variable identifiers in SQL-friendly format.
  variable_sql <- amadeusdb::quote_sql(
    sprintf("%s_%d", tolower(variable), radius)
  )
  # ----------------------------------------------------------------------------
  # Define SQL code for processing MERRA2 data for points or polygons.
  sql_extraction <- if (chr_locsgeom == "POINT") {
    "RS_Value(r.raster, db_locs.geometry, $4)"
  } else {
    "RS_ZonalStats(r.raster, db_locs.geometry, $4, $5, ($6 = 1), ($7 = 1))"
  }
  # ----------------------------------------------------------------------------
  # Define full SQL code including point/polygon distinction.
  sql_processmerra2 <- amadeusdb::glue_sql(
    file = "sedona_geos.sql",
    args = list(
      locs_id_sql = locs_id_sql,
      variable_sql = variable_sql,
      sql_extraction = sql_extraction
    )
  )
  # ----------------------------------------------------------------------------
  # Initiate database cleanup upon exit.
  chr_merra2_views <- character(0)
  on.exit(amadeusdb::cleanup_views(chr_merra2_views), add = TRUE)
  # ----------------------------------------------------------------------------
  # Register the extraction point data object in the SedonaDB session ! once !.
  amadeusdb::register_locs(sf_locs)
  chr_merra2_views <- c(chr_merra2_views, "db_locs")
  # ----------------------------------------------------------------------------
  # Perform spatial extraction within missing batches using original indices.
  for (l in int_merra2_pending) {
    # --------------------------------------------------------------------------
    # Index data files, times, and names to active batch.
    df_merra2_batch <- df_merra2paths[list_merra2_batch[[l]], , drop = FALSE]
    # --------------------------------------------------------------------------
    # Generate daily view names for active batch.
    chr_merra2_hourly <- sprintf("db_time_%03d", seq_len(nrow(df_merra2_batch)))
    # --------------------------------------------------------------------------
    message(variable, ": batch ", l, "/", length(list_merra2_batch))
    for (i in seq_len(nrow(df_merra2_batch))) {
      # ------------------------------------------------------------------------
      # Time extraction directly from NetCDF not supported (as of 10/21/26)
      list_params <- list(
        # $1: raster source
        as.character(df_merra2_batch$var[i]),
        # $2: timestamp
        df_merra2_batch$time[i],
        # $3: vertical depth/pressure level (for `lev` column output)
        1L, # chr_geos_level,
        # $4: vertical band to extract from (default = 1L)
        as.integer(band)
      )
      # ------------------------------------------------------------------------
      # Include additional arguments for polygon zonal statistics.
      if (!chr_locsgeom == "POINT") {
        list_params <- c(
          list_params,
          list(
            # $5: summary statistic
            fun, # $5: statistic
            # $6: summarize based on all touched cells (TRUE/FALSE)
            as.integer(touches),
            # $7: exclude no data (TRUE/FALSE)
            as.integer(exclude_nodata)
          )
        )
      }
      # ------------------------------------------------------------------------
      # Compute the spatial join.
      chr_merra2_views <- amadeusdb::compute_view(
        sql = sql_processmerra2,
        params = list_params,
        view = chr_merra2_hourly[i],
        all = chr_merra2_views
      )
    }
    # --------------------------------------------------------------------------
    # Write batch .parquet.
    amadeusdb::batch_write(
      locs_id = locs_id_sql,
      time_id = "time, level",
      columns = variable_sql,
      views = chr_merra2_hourly,
      parquet = chr_merra2_parquet[l],
      single = TRUE
    )
    # --------------------------------------------------------------------------
    # Remove hourly views now represented by the registered batch.
    chr_merra2_views <- amadeusdb::batch_drop(
      chr_merra2_views,
      chr_merra2_hourly
    )
  }
  # ----------------------------------------------------------------------------
  # Return parquet file out paths.
  invisible(chr_merra2_parquet)
}
