test_that("plotAncestryCausal parses ancestries correctly", {

  path_to_test_data <- "../../data/"

  # Run format_results() to simulate real data output
  results <- format_results(path_to_test_data)

  # Test ancestries EUR and AFR
  ancestries <- c("EUR", "AFR")

  # Expect the function to execute without errors
  expect_silent(
    plotAncestryCausal(results$summary, ancestries)
  )
})
