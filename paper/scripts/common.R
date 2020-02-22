library(tidyverse)
library(kableExtra)
library(corrr)

data(eclsk2011)

generate_all_tables <- function() {
  NO_PREVIEW <<- T
  Sys.glob('paper/scripts/*.R') %>%
    setdiff('paper/scripts/common.R') %>%
    walk(source)
  remove(NO_PREVIEW, inherits = T)
}

save_latex_table <- function(latex_table, name) {
  preview_file <- latex_table %>%
    save_kable(tempfile(name, fileext='.pdf'))
  
  latex_table %>%
    cat(file=file.path('paper', 'tables', paste0(name, '.tex')))
  
  viewer <- getOption('viewer')
  
  if (!is.null(viewer) && !exists('NO_PREVIEW')) {
    print(preview_file)
    viewer(preview_file)  
  }  
}