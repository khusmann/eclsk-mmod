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

study1_measures <- c('TKEEPS', 'TSHOWS', 'TWORKS', 'TADAPTS', 'TFOLLOW', 'TPERSIS', 'TATTEN',
                     'TBEZDAC', 'TBNOFIN', 'TBGCCLR', 'TBGCBLD', 'TBEZDSL', 'TBABSBK',
                     'TBWTTSK', 'TBTRBST', 'TBFLWIN', 'TBSTNO') # Omit: 'TBPLNAC', 'TBAPRRK'

# Create subset for candidate factor structures
df_train_measures <- eclsk2011$study1 %>%
  filter(split == 'train') %>%
  select(c('occasion', study1_measures))

onames <- c(
  `1` = 'Fall of Kindergarten',
  `2` = 'Spring of Kindergarten',
  `4` = 'Spring of 1st Grade'
)

fnames <- list(
  `2` = c('ATL+AF+IC', 'BE'),
  `3` = c('ATL', 'AF+IC', 'BE'),
  `4` = c('ATL', 'AF', 'IC', 'BE'),
  `5` = c('ATL', 'AF', 'AF2', 'IC', 'BE')
)

fsort_order <- list(
  `2` = list(
    `1` = c(1,2),
    `2` = c(1,2),
    `4` = c(1,2)
  ),
  `3` = list(
    `1` = c(2,1,3),
    `2` = c(2,1,3),
    `4` = c(1,2,3)
  ),
  `4` = list(
    `1` = c(1,2,3,4),
    `2` = c(1,3,2,4),
    `4` = c(1,2,3,4)
  ),
  `5` = list(
    `1` = c(1,5,2,3,4),
    `2` = c(1,5,4,2,3),
    `4` = c(2,3,5,1,4)
  )   
)

freflects <- list(
  `2` = list(
    `1` = c(),
    `2` = c(),
    `4` = c()
  ),
  `3` = list(
    `1` = c(),
    `2` = c(),
    `4` = c()
  ),
  `4` = list(
    `1` = c(2),
    `2` = c(2),
    `4` = c(2)
  ),
  `5` = list(
    `1` = c(2,3),
    `2` = c(2,3),
    `4` = c(2,3)
  )   
)

study1_explore <- function(occasion, nfactors) {
  require(psych)
  df_train_measures %>%
    filter(occasion == !!occasion) %>%
    select(-occasion) %>%
    na.omit() %>%
    fa(nfactors, rotate='promax') %>%
    `$<-`('score.cor', matrix(nrow=nfactors,ncol=nfactors)) %>% # TODO: this is a workaround for a fa.organize bug
    fa.organize(o=fsort_order[[as.character(nfactors)]][[as.character(occasion)]]) %>%
    reflect(freflects[[as.character(nfactors)]][[as.character(occasion)]])
}