################################################################################
# Unit and integration tests for the cleanup_views function.

################################################################################
library(testthat)

################################################################################
# Success cases.
################################################################################
testthat::test_that("cleanup_views; failed removal", {
  # ----------------------------------------------------------------------------
  # Initialize vector to record attempte removals.
  attempted <- character()
  # ----------------------------------------------------------------------------
  # Define a replacement fot the real view-removal function.
  fake_drop <- function(h) {
    # Append view name in test environment.
    attempted <<- c(attempted, h)
    # Simulate removal failure for view "b".
    if (h == "b") stop("simulated failure")
  }
  # ----------------------------------------------------------------------------
  # Expect message when `fake_drop` returns error;
  # mock for sedonadb::sedonadb::sd_drop_view
  testthat::expect_message(
    amadeusdb::cleanup_views(c("a", "b", "c"), drop_view = fake_drop),
    "Could not remove view b: simulated failure",
    fixed = TRUE
  )
  # ----------------------------------------------------------------------------
  # Expect "a" to still be reached despite "b" failure.
  testthat::expect_identical(attempted, c("c", "b", "a"))
})
