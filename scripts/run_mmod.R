library(OpenMx)
source('scripts/eclsk2011data.R')
source('scripts/mxMmodModel.R')

read_rds(snakemake@input[[1]]) %>%
  rlang::eval_tidy() %>%
  mxOption(key='Number of Threads', value=snakemake@threads) %>%
  mxRun(checkpoint = T) %>%
  summary() %>%
  write_rds(snakemake@output[[1]])
