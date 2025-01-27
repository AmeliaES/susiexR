#' Plot Ancestry-Specific Causal Inference Probabilities
#'
#' This function generates a plot visualising the post-hoc probability of causal
#' SNPs across multiple ancestries. It processes the .summary results from a
#' SuSiEx output, organises the ancestry data, and produces a scatter plot with
#' the probability of being causal (POST-HOC_PROB_POP_CS) for each SNP across
#' different ancestries.
#'
#' @param summary_results A data frame containing the summary results from a
#' SuSiEx analysis, the object returned from format_results()$summary.
#' @param ancestries A character vector containing the names of ancestries
#' present in the summary results, used to separate the ancestry columns and
#' assign them to the correct columns in the output.
#'
#' @return A ggplot2 object containing a scatter plot of the post-hoc
#' probabilities for SNPs, showing causal inference probabilities across
#' different ancestries.
#'
#' @examples
#' \dontrun{
#' plotAncestryCausal(results$summary, c("EUR", "AFR", "SAS"))
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import stringr
#'
#' @export

utils::globalVariables(c(
  "ALT_ALLELE", "ANCESTRY", "CHR", "BP_START", "LOCATION", "POST_HOC_PROB_POP_ANCESTRY",
  ".", "POST_HOC_PROB_POP_CS"
))

plotAncestryCausal <-  function(summary_results, ancestries){

  # this function should not run if "POST_HOC_PROB_POP" columns are not present
  # from the output of SuSiEx
  if (!all(str_detect(names(summary_results), "POST_HOC_PROB_POP"))) {
    stop("No POST_HOC_PROB_POP columns found in the input data.
         See the SuSiEx documentation for more information.")
  }

  summary_results %>%
    # Create a new column called ANCESTRY which tells us which ancestries have data on that SNP
    # Many of the columns containing info from all ancestries
    # are separated by commas for each ancestry, in the order of the susiex command
    mutate(ANCESTRY = ALT_ALLELE) %>%
    separate(ANCESTRY, into = ancestries, sep = ",") %>%
    # If there is no data for that ancestry there is an NA
    mutate(ANCESTRY = pmap_chr(across(all_of(ancestries)), function(...) {
      values <- c(...)
      present_ancestries <- ancestries[values != "NA"]  # Check which are not NA
      if (length(present_ancestries) > 0) {
        paste(present_ancestries, collapse = ",")  # Concatenate present ancestries
      } else {
        "None"  # If all are NA (this should not happen)
      }
    })) %>%
    # remove the indiviudal ancestry columnms now we have the column we want
    dplyr::select(-all_of(ancestries)) %>%
    # create a column for location on the genome
    mutate(LOCATION = str_glue("{CHR}:{BP_START}:{BP_END}")) %>%
    # Ensure CHR and BP are in correct order on x-axis
    arrange(as.numeric(CHR), as.numeric(BP_START)) %>%
    mutate(LOCATION = factor(LOCATION, levels = LOCATION)) %>%
    # Reshape the data so that POST-HOC_PROB_POP columns are turned into long format
    pivot_longer(
      cols = starts_with("POST-HOC_PROB_POP"),
      names_to = "POST_HOC_PROB_POP_ANCESTRY",
      values_to = "POST_HOC_PROB_POP_CS"
    ) %>%
    mutate(POST_HOC_PROB_POP_ANCESTRY = ancestries[as.integer(str_extract(POST_HOC_PROB_POP_ANCESTRY, "\\d+"))]) %>%
    mutate(POST_HOC_PROB_POP_ANCESTRY = factor(POST_HOC_PROB_POP_ANCESTRY, levels = unique(POST_HOC_PROB_POP_ANCESTRY))) %>%
    ggplot(data = .) +
    geom_point(aes(x = LOCATION,
                   y = POST_HOC_PROB_POP_CS,
                   shape = POST_HOC_PROB_POP_ANCESTRY,
                   color = ANCESTRY)) +
    labs(
      x = "Fine mapped region",
      y = "Post-hoc probability credible set is causal",
      shape = "Ancestry",
      color = "Ancestries of LD references\nused in SuSiEx"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

