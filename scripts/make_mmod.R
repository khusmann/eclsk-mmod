source('scripts/eclsk2011data.R')

make_mmod <- function(measures, name, fiml) {
  force(measures)
  force(name)
  force(fiml)
  quo(
    mmodModel(measures, name, fiml) %>%
    mxOption('Checkpoint Directory', 'data/mmod') %>%
    mxOption('Checkpoint Prefix', paste0(name, '_')) %>%
    mxRestore(chkpt.directory='data/mmod', chkpt.prefix=paste0(name, '_'))
  )
}

make_mmod(measures=snakemake@params[['measures']],
          name=snakemake@wildcards[['model']], 
          fiml=snakemake@config[['FIML']]) %>% write_rds(snakemake@output[[1]])
