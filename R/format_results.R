#' Format SuSiEx Output Files
#'
#' This function processes the output files from SuSiEx, specifically those with
#' the extensions `.summary`, `.cs`, and `.snp`. It organises the results into a
#' structured format for further analysis.
#'
#' - The `.summary` files are combined into a single data frame, with an
#' additional column for genomic location (`CHR:BP_START:BP_END`).
#' - The `.cs` and `.snp` files are returned as a list of data frames, with
#' each list item corresponding to a fine-mapped region. Data frame has an
#' additional column for genomic location (`CHR:BP_START:BP_END`) for the region that was fine mapped.
#'
#' The function reads all relevant files in the specified directory where SuSiEx
#'  output. Files that cannot be
#' processed are excluded from the final output, ie. they contain the values
#' NULL or FAIL.
#'
#' @param path A character string specifying the directory containing the SuSiEx
#' output files. This directory should include files with the extensions
#' `.summary`, `.cs`, and `.snp`.
#'
#' @return A list with the following components:
#'   - `summary`: A data frame containing the combined data from all `.summary`
#'   files.
#'   - `cs`: A list of data frames, each representing a fine-mapped region from
#'   the `.cs` files.
#'   - `snp`: A list of data frames, each representing a fine-mapped region from
#'    the `.snp` files.
#'
#' @examples
#' format_results("path/to/susiex/results")
#'
#' @import dplyr
#' @import purrr
#' @import stringr
#'
#' @export

format_results <- function(path) {
  file_extensions <- c("summary", "cs", "snp")

  results <- lapply(file_extensions, function(ext) {
    paths <- list.files(path,
      pattern = paste0("\\.", ext, "$"),
      full.names = TRUE
    )

    processed_files <- lapply(paths, function(path) {
      process_file(file = path, file_ext = ext)
    })
    # Clean up NULL values (failed files)
    processed_files <- processed_files[!sapply(processed_files, is.null)]

    return(processed_files)
  })

  names(results) <- file_extensions
  results$summary <- do.call(rbind, results$summary)
  return(results)
}
