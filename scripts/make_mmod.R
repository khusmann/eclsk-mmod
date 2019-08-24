source('scripts/eclsk2011data.R')

make_mmod <- function(data, measures, occasions, name, fiml, outdir) {
  force(data)
  force(measures)
  force(occasions)
  force(name)
  force(fiml)
  force(outdir)
  quo(
    mmodModel(eclsk2011data[[data]], measures, occasions, name, fiml) %>%
    mxOption('Checkpoint Directory', outdir) %>%
    mxOption('Checkpoint Prefix', paste0(name, '_')) %>%
    mxRestore(chkpt.directory=outdir, chkpt.prefix=paste0(name, '_'))
  )
}

make_mmod(data=snakemake@params[['data']],
          measures=snakemake@params[['measures']],
          occasions=snakemake@params[['occasions']],
          name=snakemake@wildcards[['model']], 
          fiml=snakemake@config[['FIML']],
          outdir=dirname(snakemake@output[[1]])) %>% write_rds(snakemake@output[[1]])
