#' Generate Regional and Locus Zoom Plots for Fine-Mapped Genetic Data
#'
#' This function generates regional and locus zoom plots for fine-mapped genetic data across multiple ancestries.
#' It uses PLINK to calculate linkage disequilibrium (LD) and integrates summary statistics and credible set results.
#' The function produces plots for each ancestry and combines them into a single plot.
#'
#' @param cs_results Data frame containing credible set results.
#' @param snp_results Data frame containing SNP results.
#' @param sumstats List of data frames containing summary statistics for each ancestry.
#' @param ancestries Vector of ancestries to be analyzed.
#' @param plink_path String specifying the path to the PLINK executable.
#' @return A list containing the region identifier and the combined plot.
#' @export
mainPlot <- function(cs_results, snp_results, sumstats, ancestries, plink_path) {
  tryCatch(
    {
      # Prepare the credible set results
      cs <- cs_results %>%
        rename(CHROM = CHR, POS = BP) %>%
        separate(`-LOG10P`, into = paste0("logP_", ancestries), sep = ",") %>%
        mutate(P = as.numeric(logP_EUR))

      snp <- snp_results

      # Get the chromosome and base pair range for the fine-mapped region
      CHR <- unique(cs$CHR)
      BP_START <- unique(cs$BP_START)
      BP_END <- unique(cs$BP_END)
      region <- paste0(CHR, ":", BP_START, ":", BP_END)

      # Get a list of SNPs included in the fine mapping, ordered by PIP
      snp_list <- snp %>%
        arrange(desc(`PIP(CS1)`)) %>%
        pull(SNP)

      # Write the SNP list to a temporary file
      writeLines(snp_list, paste0("tmp/snp_list_", region, ".txt"))

      # Generate plots for each ancestry
      region_plots <- lapply(ancestries, function(ancestry) {
        # Define the path to the LD results file
        ld_results_file <- paste0("tmp/ld_results", ancestry, region, ".txt.vcor")

        # Run PLINK to calculate LD if the results file does not exist
        if (!file.exists(ld_results_file)) {
          system2(plink_path,
            args = c(
              "--bfile", paste0("reference/ukb_imp_v3.qc.geno02.mind02_", ancestry, "_", CHR),
              "--r2-phased",
              "--ld-snp", snp_list[1],
              "--ld-window-kb", "1000",
              "--ld-window-r2", "0.2", # r^2 < 0.2 is filtered out
              "--extract", paste0("tmp/snp_list_", region, ".txt"),
              "--out", paste0("tmp/ld_results", ancestry, region, ".txt")
            )
          )
        }

        # Load the LD results if the file exists
        if (file.exists(ld_results_file)) {
          ld_results <- fread(ld_results_file)

          if (nrow(ld_results) > 0) {
            # Join SNP results with LD results and summary statistics
            joined_results <- snp %>%
              left_join(., ld_results, by = c("SNP" = "ID_B")) %>%
              left_join(., sumstats[[ancestry]], by = "SNP") %>%
              dplyr::select(-ends_with(".y")) %>%
              rename_with(~ str_remove(., "\\.x$"), ends_with(".x")) %>%
              left_join(cs, by = "SNP") %>%
              dplyr::select(-ends_with(".y")) %>%
              rename_with(~ str_remove(., "\\.x$"), ends_with(".x")) %>%
              dplyr::select(SNP,
                CHROM = CHR,
                POS = BP,
                P,
                R2 = PHASED_R2,
                PIP_CS1 = `PIP(CS1)`,
                OVRL_PIP,
                CS_PIP
              ) %>%
              mutate(R2 = ifelse(SNP == snp_list[1], 1, R2))
          } else {
            # Join SNP results with summary statistics and credible set results
            joined_results <- snp %>%
              left_join(., sumstats[[ancestry]], by = "SNP") %>%
              dplyr::select(-ends_with(".y")) %>%
              rename_with(~ str_remove(., "\\.x$"), ends_with(".x")) %>%
              left_join(cs, by = "SNP") %>%
              dplyr::select(-ends_with(".y")) %>%
              rename_with(~ str_remove(., "\\.x$"), ends_with(".x")) %>%
              dplyr::select(SNP,
                CHROM = CHR,
                POS = BP,
                P,
                PIP_CS1 = `PIP(CS1)`,
                OVRL_PIP,
                CS_PIP
              )
          }
        } else {
          # Create a default joined_results
          joined_results <- snp %>%
            left_join(., sumstats[[ancestry]], by = "SNP") %>%
            dplyr::select(-ends_with(".y")) %>%
            rename_with(~ str_remove(., "\\.x$"), ends_with(".x")) %>%
            left_join(cs, by = "SNP") %>%
            dplyr::select(-ends_with(".y")) %>%
            rename_with(~ str_remove(., "\\.x$"), ends_with(".x")) %>%
            dplyr::select(SNP,
              CHROM = CHR,
              POS = BP,
              P,
              PIP_CS1 = `PIP(CS1)`,
              OVRL_PIP,
              CS_PIP
            )
        }

        # Generate the appropriate plot based on the availability of LD data
        if (file.exists(ld_results_file) &&
          (("R2" %in% colnames(joined_results)) &&
            nrow(joined_results %>% filter(R2 == 1) %>% drop_na()) > 0) &&
          (exists("ld_results") && nrow(ld_results) > 0)) {
          # Generate a locus zoom plot
          region_plot <- topr::locuszoom(
            df = joined_results,
            chr = CHR,
            xmin = BP_START,
            xmax = BP_END,
            log_trans_p = TRUE,
            build = 37,
            alpha = 0.5,
            extract_plots = TRUE,
            title = ancestry,
            legend_text_size = 8
          )
        } else {
          # Generate a region plot
          region_plot <- topr::regionplot(
            df = joined_results,
            region = paste0(CHR, ":", BP_START, "-", BP_END),
            chr = CHR,
            xmin = BP_START,
            xmax = BP_END,
            color = "grey",
            build = 37,
            alpha = 0.5,
            extract_plots = TRUE
          )

          region_plot$main_plot <- region_plot$main_plot +
            ggtitle(ancestry)
        }

        return(region_plot)
      })

      # Extract the main locus zoom plots
      main_plots <- lapply(region_plots, function(x) x$main_plot)

      # Extract the gene plot from the first region plot
      gene_plot <- region_plots[[1]]$gene_plot

      gene_plot_clean <- gene_plot
      gene_plot_clean$layers <- gene_plot_clean$layers[-1]

      # Get the x-axis limits and breaks from one of the main plots
      x_axis_limits <- ggplot_build(main_plots[[1]])$layout$panel_params[[1]]$x.range
      rounded_min <- floor(x_axis_limits[1] / 50000) * 50000
      rounded_max <- ceiling(x_axis_limits[2] / 50000) * 50000
      x_axis_breaks <- seq(from = rounded_min, to = rounded_max, by = 50000)

      # Check if all required columns are available for the gene plot
      if (all(c("gene_symbol", "y", "gene_start", "biotype") %in% colnames(gene_plot_clean$data))) {
        tryCatch(
          {
            gene_plot <- gene_plot_clean +
              ggrepel::geom_text_repel(
                aes(
                  label = gene_symbol,
                  y = y,
                  x = gene_start,
                  family = biotype
                ),
                nudge_y = 0.25,
                nudge_x = 0.25
              )
          },
          error = function(e) {
            message("An error occurred while creating the gene_plot: ", e$message)
            gene_plot <- NULL
          }
        )
      } else {
        message('Required columns ("gene_symbol", "y", "gene_start", "biotype") are missing in the gene_plot data. This probably means there are no genes in the region plotted.')
        gene_plot <- gene_plot +
          scale_x_continuous(limits = x_axis_limits, breaks = x_axis_breaks, expand = c(.01, .01))
      }

      # Generate the PIP plot
      PIP_results <- snp %>%
        left_join(cs, by = "SNP") %>%
        dplyr::select(-ends_with(".y")) %>%
        rename_with(~ str_remove(., "\\.x$"), ends_with(".x")) %>%
        dplyr::select(SNP,
          CHROM = CHR,
          POS = BP,
          PIP_CS1 = `PIP(CS1)`,
          OVRL_PIP,
          CS_PIP
        ) %>%
        mutate(PIP_color = case_when(
          PIP_CS1 == OVRL_PIP ~ TRUE, # If condition is TRUE, set to TRUE
          is.na(PIP_CS1 == OVRL_PIP) ~ FALSE # If NA set to FALSE
        )) %>%
        filter(PIP_color)

      pip_plot <- ggplot(PIP_results) +
        geom_point(aes(x = POS, y = PIP_CS1)) +
        scale_x_continuous(
          limits = c(BP_START, BP_END),
          expand = c(.01, .01)
        ) +
        scale_y_continuous(limits = c(-0.01, 1.01)) +
        labs(
          x = "",
          y = "PIP"
        ) +
        theme_bw() +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")

      # Combine all plots into a single plot
      plot <- cowplot::plot_grid(plotlist = align_plots(plotlist = c(main_plots, list(pip_plot, gene_plot)), align = "hv"), ncol = 1)
    },
    error = function(e) {
      cat("Error in fine map for region: ", region, "\n")
      cat("Error message:", e$message, "\n")
    }
  )

  return(list(region = region, plot = plot))
}
