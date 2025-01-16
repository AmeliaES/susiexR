# R package for formatting and plotting output from SuSiEx 

## What is SuSiEx?
"SuSiEx is a C++ based command line tool that performs cross-ancestry fine-mapping using GWAS summary statistics and LD reference panels. The method is built on the Sum of Single Effects (SuSiE) model:
Wang, G., Sarkar, A., Carbonetto, P. & Stephens, M. A simple new approach to variable selection in regression, with application to genetic fine mapping. J. R. Stat. Soc. Series B Stat. Methodol. 82, 1273–1300 (2020). https://doi.org/10.1111/rssb.12388"

Please see more detail on SuSiEx software [here](https://github.com/getian107/SuSiEx).

## What is susiexR?
An R package for formatting and plotting output from SuSiEx. The functions in this package reads the output from SuSiEx (ie. `.summary`,`.cs` and `.snp` files) and formats them into data frames that can be used as input to ggplot for plotting.
