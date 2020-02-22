library(tidyverse)
library(kableExtra)
library(corrr)

data(eclsk2011)

generate_all_tables <- function() {
  Sys.glob('paper/scripts/*.R') %>%
    setdiff('paper/scripts/common.R') %>%
    map(source) %>%
    invisible()
}

save_latex_table <- function(latex_table, name) {
  preview_file <- latex_table %>%
    save_kable(file.path('paper', 'tables', paste0(name, '.pdf')))
  
  latex_table %>%
    cat(file=file.path('paper', 'tables', paste0(name, '.tex')))
  
  viewer <- getOption('viewer')
  
  if (!is.null(viewer)) {
    viewer(preview_file)  
  }  
}