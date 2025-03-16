# Debug by writing tests
# Script to outline the steps to debug the issue

###############################################################################
# Edited from "check_nextflow_output.R"
# -----------------------------------------------------------------------------

library(cowplot)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)
library(devtools)
load_all()

# ---- Read in variables:
path_to_susiex_results <- "../antidep-gwas/fineMapping/output"

ancestries <- c("SAS", "AFR", "EUR")

# ---- Read in susiex results
results <- susiexR::format_results(path_to_susiex_results, ancestries)

# Check that each processed file type has the same number of fine mapped regions
nrow(results$summary) == length(results$cs) && length(results$cs) == length(results$snp)

# ---- Plot the probability the top SNP in the credible set is causal in each ancestry.
# Ensure these are in the same order as specified to susiex
ancestries_susiex_order <- list.files(path_to_susiex_results, pattern = ".summary")[1] %>% # the first one will do
  str_extract("(?<=\\.)([A-Za-z-]+)(?=\\.)") %>% # extract the ancestry from the file name
  str_split("-") %>% unlist()  # split the ancestry by hyphen

print(plotAncestryCausal(results$summary, ancestries = ancestries_susiex_order))

###############################################################################
# Run the debugger on plotAncestryCausal

# ---- Added breakpoint within the very long summary_results
# (this needs refactoring so it's easier to debug in future)

# From running the below code, I can see that the issue is with the summary_results object
# I know the data, and the ancestry columns look incorrect to me

# summary_results %>%
#   # Create a new column called ANCESTRY which tells us which ancestries have data on that SNP
#   # Many of the columns containing info from all ancestries
#   # are separated by commas for each ancestry, in the order of the susiex command
#   mutate(ANCESTRY = ALT_ALLELE) %>%
#   separate(ANCESTRY, into = ancestries, sep = ",")

###############################################################################
# Double check the .summary files
# There should be more than 9 files with non null values,
# because there are more than 9 locus zoom plots from the last rerun of susie
# Actually it turns out there are only 9 files with results, so this means
# the locus zoom plots contain old results!
# I'm going to clear nextflow cache and rerun SuSiEx to ensure i have correct results
###############################################################################
# Re-run the above code after re-running SuSiEx, now we are confident we have the output from that latest re-run
# A different plot occurs, but I still want to check it is correct.
# I want to be sure there's no bug in the function.
# Check nrow of results$summary with nrow of the dataframe in plotAncestryCausal
nrow(results$summary)
# Use debugger to check nrow of datframe inside plotAncestryCausal()
ancestries_susiex_order <- list.files(path_to_susiex_results, pattern = ".summary")[1] %>% # the first one will do
  str_extract("(?<=\\.)([A-Za-z-]+)(?=\\.)") %>% # extract the ancestry from the file name
  str_split("-") %>% unlist()  # split the ancestry by hyphen

plotAncestryCausal(results$summary, ancestries = ancestries_susiex_order)
# Great this dataframe has the same number of rows before pivoting to long format

# Ah ha! I have found the bug! The issue is with the order of the ancestries
# For some reason, one run of nextflow has meant the ancestries are passed in different orders to SuSieX
# this means we have results of SuSiEx with different ancestry orders
# The function in my package assumes the order of the ancestries is the same as the order passed to SuSiEx
# The only way to catch this error is to get all the file names in the directory where SuSiEx results are stored
# As the file names contain the order in which the ancestries were passed to SuSiEx
# Let's write a test to the formatResults() function that checks this

###############################################################################



