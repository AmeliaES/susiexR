test_that("number of columns in processed summary file is 13", {

  summary_file <- "../../data/data-susiex-example-output-extra/SuSiEx.EUR.AFR.output.cs95.summary"

  summary_results <- process_file(file = summary_file, file_ext = "summary")

  expect_equal(ncol(summary_results), 13)

})
