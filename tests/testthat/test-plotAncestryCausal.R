test_that("plotAncestryCausal generates error if the .summary results do not have POST_HOC_PROB_POP columns", {

  path_to_test_data <- "../../data/data-no-posthoc-prob-cols"

  # Run format_results() to simulate real data output
  results <- format_results(path_to_test_data)

  # Test ancestries EUR and AFR
  ancestries <- c("EUR", "AFR")

  # Expect the function to execute without errors
  expect_error(
    plotAncestryCausal(results$summary, ancestries)
  )

})

test_that("plotAncestryCausal does not generate an error if the .summary results does have POST_HOC_PROB_POP columns", {

  path_to_test_data <- "../../data/data-exists-posthoc-prob-cols"

  # Run format_results() to simulate real data output
  results <- format_results(path_to_test_data)

  # Test ancestries EUR and AFR
  ancestries <- c("EUR", "AFR")

  # Expect the function to execute without errors
  expect_error(
    plotAncestryCausal(results$summary, ancestries)
  )

})
