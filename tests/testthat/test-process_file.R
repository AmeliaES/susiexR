test_that("number of columns in processed summary file is 12 plus number of ancestries used in SuSiEx", {

  summary_file <- system.file("extdata", "data-susiex-example-output-extra", "SuSiEx.EUR.AFR.output.cs95.summary", package = "susiexR")

  summary_result <- process_file(file = summary_file, file_ext = "summary", ancestries = c("EUR", "AFR"))

  ancestries <- c("EUR", "AFR")

  expect_equal(ncol(summary_result$data), 12 + length(ancestries) + 3)

})

test_that("names of columns in processed summary file is correct", {

  summary_file <- system.file("extdata", "data-susiex-example-output-extra", "SuSiEx.EUR.AFR.output.cs95.summary", package = "susiexR")

  summary_result <- process_file(file = summary_file, file_ext = "summary", ancestries = c("EUR", "AFR"))

  ancestries <- c("EUR", "AFR")

  expected_col_names <- c("CS_ID", "CS_LENGTH", "CS_PURITY", "MAX_PIP_SNP",
                          "BP", "REF_ALLELE",  "ALT_ALLELE", "REF_FRQ",
                          "BETA", "SE", "-LOG10P", "MAX_PIP",
                          paste0("POST-HOC_PROB_POP", 1:length(ancestries)),
                          "CHR", "BP_START", "BP_END")

  expect_equal(colnames(summary_result$data), expected_col_names)

})
