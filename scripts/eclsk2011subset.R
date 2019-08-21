source('scripts/eclsk2011env.R')

library(haven)

if (file.exists(VARFILE)) {
  eclsk2011vars <- read_rds(VARFILE)

  navars <- eclsk2011measures$all[!(eclsk2011measures$all %in% eclsk2011vars)]

  if (length(navars)) {
    stop(paste(navars, collapse=', '))
  }
} 

df <- read_spss(RAWFILE)

write_rds(colnames(df), VARFILE)

select(df)

df %>%
  rename_at(vars(starts_with('G8')), funs(str_replace(., 'G8', 'T8'))) %>%
  select(eclsk2011measures$all) %>%
  write_rds(SUBSETFILE)
