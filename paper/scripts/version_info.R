library(tidyverse)
library(OpenMx)

cat(
  sprintf('\\newcommand\\rversion{%s}\n', R.version.string),
  sprintf('\\newcommand\\mxversion{%s}\n', mxVersion()),
  file='paper/tables/version_info.tex'
)