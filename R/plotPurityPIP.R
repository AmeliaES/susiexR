#' Plot Relationship Between Credible Set Characteristics and PIP
#'
#' This function generates a scatter plot showing the relationship between three key characteristics of credible sets:
#' - `CS_LENGTH`: The number of SNPs in the credible set.
#' - `CS_PURITY`: The purity of the credible set.
#' - `MAX_PIP`: The maximum posterior inclusion probability (PIP) in the credible set.
#'
#' The plot visualizes the data as follows:
#' - The x-axis represents `CS_PURITY` (the purity of the credible set).
#' - The y-axis represents `MAX_PIP` (the maximum posterior inclusion probability).
#' - The size of each point corresponds to the number of SNPs in the credible set (`CS_LENGTH`).
#' - Each point is colored by chromosome (`CHR`).
#'
#' @param summary_results A data frame containing summary results, which should include the columns `CS_PURITY`, `MAX_PIP`, `CS_LENGTH`, and `CHR`.
#'
#' @return A ggplot2 object that visualizes the relationship between `CS_PURITY`, `MAX_PIP`, and `CS_LENGTH` across chromosomes.
#'
#' @examples
#' \dontrun{
#' plotPurityPIP(results$summary)
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import stringr
#'
#' @export
#' @name plotPurityPIP

utils::globalVariables(c(
  "results", "CHR", ".", "CS_PURITY", "MAX_PIP", "CS_LENGTH"
))

plotPurityPIP <-  function(summary_results = results$summary){
  summary_results %>%
    arrange(as.numeric(CHR)) %>%
    mutate(CHR = factor(CHR, levels = unique(CHR))) %>%
    ggplot(., aes(x = CS_PURITY, y = MAX_PIP, size = CS_LENGTH)) +
    geom_point(alpha = 0.6, aes(colour = CHR)) +
    geom_text_repel(data = subset(summary_results, MAX_PIP > 0.8),
                    aes(label = CHR),
                    size = 3,
                    box.padding = 0.5, max.overlaps = Inf,
                    segment.color = "black") +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
    scale_size_continuous(range = c(1, 10)) +
    labs(
      x = "Credible Set Purity",
      y = "Maximum Posterior Inclusion Probability",
      size = "Number of SNPs\nin credible set",
      colour = "Chromosome"
    ) +
    theme_minimal() +
    theme(
      # Arrange the legends side by side and wrap CHR legend into two columns
      legend.position = "right", # Place legends at the bottom
      legend.box = "horizontal",  # Arrange legends horizontally
    ) +
    guides(
      # Set the legend for CHR (color) to have 2 columns
      colour = guide_legend(ncol = 2, order = 2),
      size = guide_legend(order = 1),  # Ensure size legend appears first
    )
}
