source('scripts/eclsk2011data.R')

run_mmod <- function(measures, name, fiml, outfile, threads) {
  mmodModel(measures, name, fiml) %>%
    mxOption(key='Number of Threads', value=threads) %>%
    mxRun() %>%
    write_rds(outfile)
}

run_mmod(measures=snakemake@params[['measures']],
         name=snakemake@wildcards[['model']], 
         fiml=snakemake@config[['FIML']],
         outfile=snakemake@output[[1]],
         threads=snakemake@threads)
