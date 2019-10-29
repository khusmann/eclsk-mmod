source('scripts/eclsk2011data2.R')
source('scripts/mxMmodModel.R')

make_mmod <- function(data, measures, name, fiml, outdir) {
  force(data)
  force(measures)
  force(name)
  force(fiml)
  force(outdir)
  quo(
    eclsk2011[[data]] %>%
    group_by(occasion) %>%
    mutate_if(is.numeric, scale) %>%
    ungroup() %>%
    # TODO: FIXME
    #filter(split == 'test') %>%
    mxMmodModel(name, idvar='CHILDID', timevar='occasion', measures, fiml) %>%
    mxOption('Checkpoint Directory', outdir) %>%
    mxOption('Checkpoint Prefix', paste0(name, '_')) %>%
    mxRestore(chkpt.directory=outdir, chkpt.prefix=paste0(name, '_'))
  )
}

make_mmod(data=snakemake@params[['data']],
          measures=snakemake@params[['measures']],
          name=snakemake@wildcards[['model']], 
          fiml=snakemake@config[['FIML']],
          outdir=dirname(snakemake@output[[1]])) %>% write_rds(snakemake@output[[1]])
