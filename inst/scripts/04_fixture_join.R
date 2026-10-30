################################################################################
# Manual script for creating fixture data for join_covariates edge case tests.
# Mitchell Manware
# 30 October 2026

# ----------------------------------------------------------------------------
library(sedonadb)

# ----------------------------------------------------------------------------
chr_outdir <- testthat::test_path("..", "testdata", "join")

# ----------------------------------------------------------------------------
# Fixture AQS (left) data with repeated `time` key.
df_aqs <- data.frame(
  site_id = rep("001", 3),
  time = as.POSIXct(
    c("2018-01-01 00:15:00", "2018-01-01 00:15:00", "2018-01-01 01:15:00"),
    tz = "UTC"
  ),
  value = c(10, 11, 12)
)

# ----------------------------------------------------------------------------
# Fixture GEOS and HMS data.
list_data <- list(
  aqs = df_aqs,
  geos = data.frame(
    site_id = rep("001", 2),
    time = as.POSIXct(
      c("2018-01-01 00:30:00", "2018-01-01 01:30:00"),
      tz = "UTC"
    ),
    value = c(20, 21)
  ),
  hms = data.frame(
    site_id = "001",
    date = as.Date("2018-01-01"),
    value = 30
  ),
  gmted = data.frame(site_id = "001", value = 40)
)

# ----------------------------------------------------------------------------
# An additional GEOS batch overlaps the first fixture hour after normalization.
list_data$geos_duplicate <- list_data$geos[1, , drop = FALSE]
list_data$geos_duplicate$time <- list_data$geos_duplicate$time + 25 * 60

# ----------------------------------------------------------------------------
# Distinct HMS timestamps normalize to the same date for the same location.
list_data$hms_duplicate <- list_data$hms[c(1, 1), , drop = FALSE]
list_data$hms_duplicate$date <- as.POSIXct(
  list_data$hms_duplicate$date,
  tz = "UTC"
) + c(5 * 60, 23 * 3600 + 55 * 60)

# ----------------------------------------------------------------------------
# Duplicate GMTED locations violate static source uniqueness.
list_data$gmted_duplicate <- list_data$gmted[c(1, 1), , drop = FALSE]

# ----------------------------------------------------------------------------
# Missing GEOS location and timestamp keys are separate failure cases.
list_data$geos_missing_site <- list_data$geos
list_data$geos_missing_site$site_id[2] <- NA_character_
list_data$geos_missing_time <- list_data$geos
list_data$geos_missing_time$time[2] <- NA

# ----------------------------------------------------------------------------
# Missing HMS location and date keys are separate failure cases.
list_data$hms_missing_site <- list_data$hms
list_data$hms_missing_site$site_id[1] <- NA_character_
list_data$hms_missing_date <- list_data$hms
list_data$hms_missing_date$date[1] <- NA

# ----------------------------------------------------------------------------
# Static sources require a non-missing location key.
list_data$gmted_missing_site <- list_data$gmted
list_data$gmted_missing_site$site_id[1] <- NA_character_

# ----------------------------------------------------------------------------
# Write small Parquet inputs without requiring spatial extraction.
chr_paths <- setNames(
  file.path(chr_outdir, paste0(names(list_data), ".parquet")),
  names(list_data)
)
for (chr_source in names(list_data)) {
  sedonadb::sd_write_parquet(
    sedonadb::as_sedonadb_dataframe(list_data[[chr_source]]),
    chr_paths[[chr_source]],
    single_file_output = TRUE
  )
}
