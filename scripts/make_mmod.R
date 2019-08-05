source('scripts/eclsk2011data.R')

make_mmod <- function(measures, name, fiml) {
  measures <- measures
  name <- name
  fiml <- fiml
  function() {
    mmodModel(measures, name, fiml) %>%
      mxOption('Checkpoint Directory', 'data/mmod') %>%
      mxRestore()
  }
}

make_mmod(measures=snakemake@params[['measures']],
          name=snakemake@wildcards[['model']], 
          fiml=snakemake@config[['FIML']]) %>% write_rds(snakemake@output[[1]])
