#' Process Individual SuSiEx Output Files
#'
#' Reads and processes individual SuSiEx output files, extracting chromosome and position data from the `.summary` file
#' and using that information to process `.cs` and `.snp` files.
#'
#' @param file A character string specifying the path to the file to be processed.
#' @param file_ext A character string specifying the extension of the file ("summary", "cs", or "snp").
#' @param chr_info A named list containing chromosome (CHR), start position (BP_START), and end position (BP_END)
#' extracted from the `.summary` file, required for `.cs` and `.snp` files. Default is `NULL`.
#'
#' @return A data frame containing the processed results, or `NULL` if the file could not be processed.
#'
#' @importFrom data.table fread
#' @import dplyr
#' @import stringr
#'
#' @keywords internal

process_file <- function(file, file_ext, chr_info = NULL, ancestries) {

  tryCatch({
    # Process .summary files
    if (file_ext == "summary") {
      # Read the first line to extract CHR, BP_START, and BP_END
      header_line <- readLines(file, n = 1)
      chr_parts <- str_match(header_line, "#\\s*chr(\\d+):(\\d+)-(\\d+)")
      if (is.na(chr_parts[1])) {
        stop("Invalid .summary file header format")
      }

      # Read the remaining data, skipping the header line
      results <- fread(file, skip = 1, sep = "\t")

      # Check if number of columns are equal to 12 plus number of ancestries used in SuSiEx

      # Calculate 12 plus number of ancestries used in SuSiEx
      expected_ncol <- 12 + length(ancestries)

      if ( ncol(results) != expected_ncol  ) {
        stop("Invalid number of columns in .summary file")
      }

      # Check if column names are as expected
      # "POST-HOC_PROB_POP*" columns are added for each ancestry

      expected_col_names <- c("CS_ID", "CS_LENGTH", "CS_PURITY", "MAX_PIP_SNP",
                              "BP", "REF_ALLELE",  "ALT_ALLELE", "REF_FRQ",
                              "BETA", "SE", "-LOG10P", "MAX_PIP",
                              paste0("POST-HOC_PROB_POP", 1:length(ancestries)))

      if ( ! all(colnames(results) %in% expected_col_names) ) {
        stop("Invalid column names in .summary file")
      }

      # Add CHR, BP_START, and BP_END to the data
      results <- results %>%
        mutate(
          CHR = as.numeric(chr_parts[2]),
          BP_START = as.integer(chr_parts[3]),
          BP_END = as.integer(chr_parts[4])
        )

      return(list(
        data = results,
        chr_info = list(
          CHR = as.numeric(chr_parts[2]),
          BP_START = as.integer(chr_parts[3]),
          BP_END = as.integer(chr_parts[4])
        )
      ))
    }

    # Process .cs or .snp files
    if (file_ext == "cs" | file_ext == "snp") {
      if (is.null(chr_info)) {
        stop(paste("CHR information is required to process", file_ext, "files"))
      }

      # Read the file as a data frame
      results <- fread(file)

      # Check if the file is empty or invalid
      if (nrow(results) == 0 | (file_ext == "snp" & ncol(results) == 2)) {
        message(paste("Skipping file (it has no results):", file))
        return(NULL)
      }

      # Add CHR, BP_START, and BP_END to the data
      results <- results %>%
        mutate(
          CHR = chr_info$CHR,
          BP_START = chr_info$BP_START,
          BP_END = chr_info$BP_END
        )

      return(results)
    }

    stop("Unsupported file extension")

  }, error = function(e) {
    message(paste("Error processing file:", file, "Error:", e$message))
    return(NULL)
  })
}
