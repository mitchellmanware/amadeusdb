################################################################################
# Unit tests for the packge startup functions and auxiliaries.

################################################################################
library(testthat)

################################################################################
testthat::test_that(".amadeusdb", {
  # ----------------------------------------------------------------------------
  # Access the private package environment.
  env_amadeusdb <- amadeusdb:::.amadeusdb
  # ----------------------------------------------------------------------------
  # Verify that package data are stored in an environment.
  testthat::expect_true(is.environment(env_amadeusdb))
  # ----------------------------------------------------------------------------
  # Verify that the environment has no inherited bindings.
  testthat::expect_identical(parent.env(env_amadeusdb), emptyenv())
  # ----------------------------------------------------------------------------
  # Verify that the load hook initialized the CRS lookup as a list.
  testthat::expect_type(env_amadeusdb$list_crs, "list")
})

################################################################################
testthat::test_that(".onLoad", {
  # ----------------------------------------------------------------------------
  # Create isolated storage so this test cannot change real package data.
  env_empty <- new.env(parent = emptyenv())
  # ----------------------------------------------------------------------------
  # Temporarily direct the load hook to the isolated environment.
  testthat::local_mocked_bindings(
    .amadeusdb = env_empty,
    .package = "amadeusdb"
  )
  # ----------------------------------------------------------------------------
  # Locate the packaged lookup file.
  chr_path <- system.file(
    "extdata",
    "list_crs.rds",
    package = "amadeusdb",
    mustWork = TRUE
  )
  # ----------------------------------------------------------------------------
  # Read the expected lookup independently.
  list_crs <- readRDS(chr_path)
  # ----------------------------------------------------------------------------
  # Run the hook and verify its documented invisible return.
  # NULL lets system.file() use its default package lookup.
  testthat::expect_invisible(
    amadeusdb:::.onLoad(libname = NULL, pkgname = "amadeusdb")
  )
  # ----------------------------------------------------------------------------
  # Verify that the hook populated the environment with the complete lookup.
  testthat::expect_identical(.amadeusdb$list_crs, list_crs)
})
