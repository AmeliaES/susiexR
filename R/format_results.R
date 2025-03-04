#' Format SuSiEx Output Files
#'
#' Processes the SuSiEx output files by extracting position information from `.summary` files and
#' applying it to `.cs` and `.snp` files. Files that cannot be processed are excluded.
#'
#' @param path A character string specifying the directory containing the SuSiEx output files.
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

  # check there are no duplicate chr and regions in the file names
  fine_mapped_regions <- str_extract(base_names, "\\d+:\\d+:\\d+")

  length(fine_mapped_regions) == length(unique(fine_mapped_regions)) ||
    stop("Duplicate fine mapped regions found in file names.")

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
