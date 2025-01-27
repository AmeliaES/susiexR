test_that("plotAncestryCausal generates error if the .summary results do not have POST_HOC_PROB_POP columns", {

  path_to_test_data <- system.file("extdata", "data-susiex-example-output-extra", package = "susiexR")

  # Test ancestries EUR and AFR
  ancestries <- c("EUR", "AFR")

  # Run format_results() to simulate real data output
  results <- format_results(path_to_test_data, ancestries = ancestries)

  # Ensure that results$summary exists and has the required structure before altering it
  expect_type(results, "list")
  expect_true("summary" %in% names(results))
  expect_s3_class(results$summary, "data.table")

  summary_results <- results$summary %>%
    dplyr::select(-starts_with("POST-HOC_PROB_POP"))

  # Expect the function to execute without errors
  expect_error(
    plotAncestryCausal(summary_results, ancestries),
    "Error: No POST_HOC_PROB_POP columns found in the input data. See the SuSiEx documentation for more information."
  )

})


