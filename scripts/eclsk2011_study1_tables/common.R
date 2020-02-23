library(tidyverse)
library(kableExtra)
library(corrr)

data(eclsk2011)

OUTDIR <- file.path('data', 'res', 'eclsk2011_study1', 'tables')

if (!dir.exists(OUTDIR)) {
   dir.create(OUTDIR, recursive=T)
}

generate_all_tables <- function() {
  NO_PREVIEW <<- T
  Sys.glob('scripts/eclsk2011_study1_tables/*.R') %>%
    setdiff('scripts/eclsk2011_study1_tables/common.R') %>%
    walk(source)
  remove(NO_PREVIEW, inherits = T)
}

save_latex_table <- function(latex_table, name) {
  latex_table %>%
    cat(file=file.path(OUTDIR, paste0(name, '.tex')))
  
  viewer <- getOption('viewer')
  if (!is.null(viewer) && !exists('NO_PREVIEW')) {
    preview_file <- latex_table %>%
      save_kable(tempfile(name, fileext='.pdf'))
    viewer(preview_file)  
  }  
}
