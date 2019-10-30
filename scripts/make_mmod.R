options(tidyverse.quiet = T)
library(tidyverse)

make_mmod <- function(data, subset, split, measures, name, fiml, outdir) {
  force(data)
  force(subset)
  force(split)
  force(measures)
  force(name)
  force(fiml)
  force(outdir)
  quo({
    require(tidyverse)
    source('scripts/mxMmodModel.R')
    load(paste0('data/', data, '.rda'))
    get(data)[[subset]] %>%
    group_by(occasion) %>%
    mutate_if(is.numeric, scale) %>%
    ungroup() %>%
    filter(split == !!split) %>%
    mxMmodModel(name, idvar='CHILDID', timevar='occasion', measures, fiml) %>%
    mxOption('Checkpoint Directory', outdir) %>%
    mxOption('Checkpoint Prefix', paste0(name, '_')) %>%
    mxRestore(chkpt.directory=outdir, chkpt.prefix=paste0(name, '_'))
  })
}

make_mmod(data=snakemake@params[['data']],
          subset=snakemake@params[['subset']],
          split=snakemake@params[['split']],
          measures=snakemake@params[['measures']],
          name=snakemake@wildcards[['model']], 
          fiml=snakemake@config[['FIML']],
          outdir=dirname(snakemake@output[[1]])) %>% write_rds(snakemake@output[[1]])
