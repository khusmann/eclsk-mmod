source('scripts/eclsk2011data.R')

read_rds(snakemake@input[[1]])() %>%
  mxOption(key='Number of Threads', value=snakemake@threads) %>%
  mxRun(checkpoint = T)
