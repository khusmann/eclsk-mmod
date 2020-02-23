source('scripts/eclsk2011_study1_tables/common.R')
library(OpenMx)

cat(
  sprintf('\\newcommand\\rversion{%s}\n', R.version.string),
  sprintf('\\newcommand\\mxversion{%s}\n', mxVersion()),
  sprintf('\\newcommand\\snakeversion{%s}\n', system('snakemake --version', intern=T)),
  file=file.path(OUTDIR, 'version_info.tex')
)
