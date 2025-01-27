library(stringr)
library(data.table)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tidyr)

# Output from SuSiEx example code:
data_cs <- fread("data/data-susiex-example-output-extra/SuSiEx.EUR.AFR.output.cs95.cs")
data_summary <- fread("data/data-susiex-example-output-extra/SuSiEx.EUR.AFR.output.cs95.summary")
data_snp <- fread("data/data-susiex-example-output-extra/SuSiEx.EUR.AFR.output.cs95.snp")

data_cs
data_summary
data_snp
