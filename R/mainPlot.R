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
#' @param plink2_path String specifying the path to the PLINK executable.
#' @param bfile_paths String specifying the path to the PLINK binary files.
#'
#' @param plink_path String specifying the path to the PLINK executable.
#'
#' @return A list containing the region identifier and the combined plot.
#'
#' @import cowplot
#' @import ggrepel
#' @import topr
#' @import dplyr
#' @import purrr
#' @import stringr
#'
#' @export
#' @name mainPlot

utils::globalVariables(c(
  "BP", "-LOG10P", "logP_EUR", "PIP(CS1)", "SNP", ".", "PHASED_R2",
  "OVRL_PIP", "CS_PIP", "R2", "gene_symbol", "y", "gene_start", "biotype",
  "PIP_color", "POS", "PIP_CS1", "P"
))

mainPlot <- function(cs_results, snp_results, sumstats, ancestries, plink2_path="plink2", bfile_paths){
  tryCatch({

    get_bfile_prefixes <- function(bfile_path) {
      # List all files in the directory
      files <- list.files(bfile_path, full.names = TRUE)

      # Extract file prefixes that have .bim, .bed, and .fam files
      file_prefixes <- unique(tools::file_path_sans_ext(basename(files)))

      # Check that each prefix has all three required files
      valid_prefixes <- file_prefixes[sapply(file_prefixes, function(prefix) {
        all(file.exists(file.path(bfile_path, paste0(prefix, c(".bim", ".bed", ".fam")))))
      })]

      return(paste0(bfile_path, "/", valid_prefixes))
    }

    bfile_list <- get_bfile_prefixes(bfile_path = bfile_paths)

    # Extract ancestry from file names (so they are in the same order as files)
    ancestries_reordered <- str_extract(bfile_list, paste0(ancestries, collapse = "|"))

    # Create dataframe
    bfile_df <- data.frame(ancestry = ancestries_reordered, bfile = bfile_list, stringsAsFactors = FALSE)

    # Extract CHR from the bfile names (last part after the last underscore)
    bfile_df$CHR <- as.numeric(sub(".*_(\\d+)$", "\\1", bfile_df$bfile))

    # ---- Format susiex results
    cs <- cs_results %>%
      rename(CHROM = CHR, POS = BP)

    snp <- snp_results

    # Get CHR:BP:BP for the fine mapped region
    CHR <- unique(cs$CHR)
    BP_START <- unique(cs$BP_START)
    BP_END <- unique(cs$BP_END)
    region <- paste0(CHR, ":", BP_START, ":", BP_END)

    # Region plot -log10(p) vs snp requires R^2 between SNPs
    # Get a list of SNPs that were included in the fine mapping
    # arrange by PIP so the first row is the SNP with highest PIP
    snp_list <- snp %>%
      arrange(desc(`PIP(CS1)`)) %>%
      pull(SNP)

    writeLines(snp_list, paste0("snp_list_", region, ".txt"))

    # Per ancestry:
    region_plots <- lapply(ancestries, function(ancestry){

      # Add r2 column to results, conduct in PLINK
      # Define the path to the LD results file
      ld_results_file <- paste0("ld_results", ancestry, region, ".txt.vcor")

      bfile_path <- bfile_df[bfile_df$ancestry == ancestry & bfile_df$CHR == CHR,]$bfile

      if (!file.exists(ld_results_file)) {
        system2(plink2_path,
                args = c(
                  "--bfile", bfile_path,
                  "--r2-phased",
                  "--ld-snp", snp_list[1],
                  "--ld-window-kb", "1000",
                  "--ld-window-r2", "0.2", # r^2 < 0.2 is filtered out
                  "--extract", paste0("snp_list_", region, ".txt"),
                  "--out", paste0("ld_results",ancestry, region, ".txt")
                )
        )
      }

      # Define the path to the LD results file
      ld_results_file <- paste0("ld_results", ancestry, region, ".txt.vcor")


      # Check if the LD results file exists
      if (file.exists(ld_results_file)) {
        # If the file exists, load the LD results
        ld_results <- fread(ld_results_file)

        if(nrow(ld_results) > 0){
          # Perform the usual joining logic
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
                          CS_PIP) %>%
            mutate(R2 = ifelse(SNP == snp_list[1], 1, R2))
        } else {
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
                          CS_PIP)
        }
      } else {
        # If the file does not exist, create a default joined_results with R2 set to NA for all rows
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
                        CS_PIP)
      }

      # If there is LD data then do a locus zoom plot
      # else do a region plot instead

      if ( file.exists(ld_results_file) &&
           (("R2" %in% colnames(joined_results)) &&
            nrow(joined_results %>% filter(R2 == 1) %>% drop_na()) > 0) &&
           (exists("ld_results") && nrow(ld_results) > 0) ) {

        # Region plot (one per ancestry):
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
          legend_text_size = 8)

      }else{
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


    # get all the main locus zoom plots
    main_plots <- lapply(region_plots, function(x) x$main_plot)

    # get a gene plot for one of them
    gene_plot <- region_plots[[1]]$gene_plot

    # PIP plot (one for all ancestries):
    PIP_results <- snp %>%
      left_join(cs, by = "SNP") %>%
      dplyr::select(-ends_with(".y")) %>%
      rename_with(~ str_remove(., "\\.x$"), ends_with(".x")) %>%
      dplyr::select(SNP,
                    CHROM = CHR,
                    POS = BP,
                    PIP_CS1 = `PIP(CS1)`,
                    OVRL_PIP,
                    CS_PIP) %>%
      mutate(PIP_color = case_when(
        PIP_CS1 == OVRL_PIP ~ TRUE,   # If condition is TRUE, set to TRUE
        is.na(PIP_CS1 == OVRL_PIP) ~ FALSE  # If NA set to FALSE
      )) %>%
      filter(PIP_color)

    pip_plot <- ggplot(PIP_results) +
      geom_point(aes(x = POS, y = PIP_CS1))+
      scale_x_continuous(limits = c(BP_START, BP_END),
                         expand=c(.01,.01)) +
      scale_y_continuous(limits = c(-0.01, 1.01)) +
      labs(x = "",
           y = "PIP") +
      theme_bw()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")


    plot <- cowplot::plot_grid(plotlist = align_plots(plotlist = c(main_plots, list(pip_plot, gene_plot)), align = 'hv'), ncol = 1)

  }, error = function(e) {
    cat("Error in fine map for region index: ", i, "\n")
    cat("Error message:", e$message, "\n")
  })

  return(list(region = region, plot = plot))

}

