test_that("check location at path exists", {
  # Test a temporary directory does exist
  path <- tempdir()  # Use a temporary directory for testing
  expect_true(dir.exists(path))  # Expect the path to exist

  # Negative test case
  expect_error(format_results("invalid/path/to/test", ancestries = c("EUR", "AFR")),
               "Path does not exist.")
  # Clean up
  unlink(path)
})

test_that("check location at path contains .summary, .snp and .cs files (in sets of 3, with the same base name)", {

  # Negative test case
  # Create a temporary directory
  temp_dir <- tempdir()

  # Locate and copy files from source to temporary directory
  source_path <- system.file("extdata", "data-susiex-example-output-extra", package = "susiexR")
  files <- list.files(source_path, full.names = TRUE)
  file.copy(files, temp_dir, overwrite = TRUE)

  # Rename files in temp_dir to create inconsistencies
  copied_files <- list.files(temp_dir, full.names = TRUE)
  file.rename(copied_files[1], file.path(temp_dir, paste0("invalid_", basename(copied_files[1]))))
  file.rename(copied_files[2], file.path(temp_dir, paste0("mismatch_", basename(copied_files[2]))))

  # Use the temporary directory as the path for testing
  expect_error(format_results(temp_dir, ancestries = c("EUR", "AFR")),
               "Error: The following files are invalid as their base names do not have exactly 3 associated files:")

  # Clean up the temporary directory
  unlink(list.files(temp_dir, full.names = TRUE))

})

test_that("format_results processes valid input correctly (positive case)", {

  # Locate the valid source path
  source_path <- system.file("extdata", "data-susiex-example-output-extra", package = "susiexR")

  # Call format_results with the valid source path
  results <- format_results(source_path, ancestries = c("EUR", "AFR"))

  # Assertions for the positive test case
  expect_type(results, "list") # Check the output type
  expect_named(results, c("summary", "cs", "snp")) # Check the expected names
  expect_true(nrow(results$summary) > 0) # Ensure `summary` contains data
  expect_true(nrow(do.call(rbind, results$snp)) > 0) # Ensure `snp` contains data
  expect_true(nrow(do.call(rbind, results$cs)) > 0) # Ensure `cs` contains data

  # Ensure number of items in .cs and .snp list are the same number of base names
  files <- list.files(source_path, pattern = "\\.summary$|\\.snp$|\\.cs$", full.names = TRUE)

  # Extract base names by removing the extensions
  base_names <- sub("\\.(summary|snp|cs)$", "", basename(files))

  expect_equal(length(results$cs), length(unique(base_names)))
  expect_equal(length(results$snp), length(unique(base_names)))

})

