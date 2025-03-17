#' Format SuSiEx Output Files
#'
#' Processes the SuSiEx output files by extracting position information from `.summary` files and
#' applying it to `.cs` and `.snp` files. Files that cannot be processed are excluded.
#'
#' @param path A character string specifying the directory containing the SuSiEx output files. The user must check there are no duplicate files of fine mapped regions.
#' @param ancestries A character vector specifying the ancestries used in the SuSiEx analysis.
#'
#' @return A list with the following components:
#'   - `summary`: A data frame containing the combined data from all `.summary` files.
#'   - `cs`: A list of data frames for `.cs` files, each annotated with position information.
#'   - `snp`: A list of data frames for `.snp` files, each annotated with position information.
#'
#' @examples
#' \dontrun{
#' format_results("path/to/susiex/results", ancestries = c("EUR", "AFR"))
#' }
#'
#' @import dplyr
#' @import purrr
#' @import stringr
#'
#' @export

format_results <- function(path, ancestries) {

  # Check location at path exists
  if (!dir.exists(path)) {
    stop("Path does not exist.")
  }

  # Check location at path contains .summary, .snp and .cs files,
  # in sets of 3, with the same base name
  # List all files with the desired extensions
  files <- list.files(path, pattern = "\\.summary$|\\.snp$|\\.cs$", full.names = TRUE)

  # Check correct file types
  if (length(files) == 0) {
    stop("No .summary, .snp or .cs files found in the specified directory.")
  }

  # All file names should contain the ancestry labels in one order
  # the order may differ to the order in the vector above (given as an argument to format_results function)

  # Get all possible orders of ancestries (permutations)
  permutations <- arrangements::permutations(ancestries)

  # Collapse the permutations into strings, separated by any common separator (hyphen, underscore or period)
  permutations_strings <- lapply(1:nrow(permutations), function(i) {
    paste(permutations[i, ], collapse = "[-_.]")  # Match hyphens, underscores, or periods
  })

  # Check for matches: We want to ensure that only one order is found across all files
  ancestry_string_in_all_file_names <- sapply(permutations_strings, function(perm) {
    # Loop over each ancestry order and check if it is found in all file names
    all(sapply(files, function(file) {
      grepl(perm, file)
    }))
  })

  # Check if exactly one valid ancestry order is found across all files
  if (sum(ancestry_string_in_all_file_names) != 1) {
    stop("Error: The file names do not have the same order of ancestry labels or the ancestries are not immediately separated by hyphens, underscores, or periods.")
  }

  # Extract base names by removing the extensions
  base_names <- sub("\\.(summary|snp|cs)$", "", basename(files))

  # Count occurrences of each base name
  base_name_counts <- table(base_names)

  # Check for invalid base names
  invalid_base_names <- names(base_name_counts[base_name_counts != 3])

  # If any invalid base names exist, throw an error with details
  if (length(invalid_base_names) > 0) {
    invalid_files <- files[base_names %in% invalid_base_names]
    stop("Error: The following files are invalid as their base names do not have exactly 3 associated files:\n",
         paste(invalid_files, collapse = "\n"))
  }

  # Identify all .summary files
  summary_files <- list.files(path, pattern = "\\.summary$", full.names = TRUE)

  # Process each .summary file and its related .cs and .snp files
  results <- lapply(summary_files, function(summary_file) {
    # Process the .summary file and extract CHR info
    summary_result <- process_file(file = summary_file, file_ext = "summary", ancestries = ancestries)
    if (is.null(summary_result)) return(NULL)

    # Extract CHR info from the .summary file
    chr_info <- summary_result$chr_info

    # Find corresponding .cs and .snp files
    base_name <- tools::file_path_sans_ext(basename(summary_file))
    cs_file <- file.path(path, paste0(base_name, ".cs"))
    snp_file <- file.path(path, paste0(base_name, ".snp"))

    # Process .cs and .snp files
    cs_result <- if (file.exists(cs_file)) process_file(cs_file, "cs", chr_info) else NULL
    snp_result <- if (file.exists(snp_file)) process_file(snp_file, "snp", chr_info) else NULL

    return(list(
      summary = summary_result$data,
      cs = cs_result,
      snp = snp_result
    ))
  })

  # Combine the results into a structured list
  summary_data <- do.call(rbind, lapply(results, `[[`, "summary"))
  cs_data <- lapply(results, `[[`, "cs")
  snp_data <- lapply(results, `[[`, "snp")

  return(list(
    summary = summary_data,
    cs = cs_data[!sapply(cs_data, is.null)],
    snp = snp_data[!sapply(snp_data, is.null)]
  ))
}
