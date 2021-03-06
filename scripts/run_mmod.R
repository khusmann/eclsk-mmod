options(tidyverse.quiet = T)
library(tidyverse)
library(OpenMx)

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
    require(OpenMx)
    require(mxmmod)
    load(paste0('data/', data, '.rda'))
    get(data)[[subset]] %>%
      filter(split == !!split) %>%
      mxMmodModel(name, idvar='CHILDID', timevar='grade', measures, fiml) %>%
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
          outdir=dirname(snakemake@output[[1]])) %>%
  write_rds(snakemake@params[['model_save']]) %>%
  rlang::eval_tidy() %>%
  mxOption(key='Number of Threads', value=snakemake@threads) %>%
  mxRun(checkpoint = T) %>%
  {
    if (snakemake@config[['FIML']]) {
      summary(., refModels=mxRefModels(., run = T))
    } else {
      summary(.)
    }
  } %>%
  write_rds(snakemake@output[[1]])
