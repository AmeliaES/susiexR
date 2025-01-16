#' Process Individual SuSiEx Output Files
#'
#' This function reads and processes individual files from SuSiEx output, such as `.summary`, `.cs`, and `.snp` files. It extracts relevant information, including chromosome and position data, and handles file reading and error management.
#'
#' - For `.summary` files, it reads the data skipping the header line.
#' - For `.cs` and `.snp` files, it reads them directly as data frames.
#' - If the file has no rows or contains only two columns (indicating that it is empty or invalid), the function returns `NULL` and skips processing that file.
#' - It extracts chromosome (CHR), start position (BP_START), and end position (BP_END) from the file name using regular expressions.
#'
#' This function is called internally by other functions in the package to process multiple files at once.
#'
#' @param file A character string specifying the path to the file to be processed.
#' @param file_ext A character string specifying the extension of the file (either "summary", "cs", or "snp").
#'
#' @return A data frame containing the processed results, or `NULL` if the file could not be processed (e.g., if it has no results or is invalid).
#'
#' @import data.table
#' @import dplyr
#' @import stringr
#'
#' @keywords internal

process_file <- function(file, file_ext) {

  # Error handling using tryCatch to handle any potential file read errors
  tryCatch({

    # Process .summary files
    if (file_ext == "summary") {
      # Read the file as lines
      results <- fread(file, skip = 1)
      # Process .cs or .snp files
    } else if (file_ext == "cs" | file_ext == "snp") {
      # Read in the file as a data.frame
      results <- fread(file)
    }
    # Check if the file has no rows (empty or failed file), in which case we skip it
    # Some of the SNP files are not empty, and contain only CHR and BP cols
    # These can be removed too because their corresponding .cs files contain NULL
    if (nrow(results) == 0 | (file_ext == "snp" & ncol(results) == 2)) {
      message(paste("Skipping file (it has no results):", file))
      return(NULL)
    }

    # Extract chromosome (CHR), start position (BP_START), and end position (BP_END)
    # These values are extracted from the file name using regular expressions
    # The regex pattern matches filenames like: "cs95_1:1000:2000.cs" or "cs95_1:1000:2000.snp"
    chr_parts <- str_match(file, "(\\d+):(\\d+):(\\d+)\\.(cs|snp|summary)")

    # Add the new columns to the data
    results <- results %>%
      mutate(
        CHR = as.numeric(chr_parts[2]),
        BP_START = as.integer(chr_parts[3]),
        BP_END = as.integer(chr_parts[4])
      )

    return(results)

  }, error = function(e) {
    message(paste("Error processing file:", file, "Error:", e$message))
    return(NULL)
  })
}
