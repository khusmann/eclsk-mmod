# http://supp.apa.org/psycarticles/supplemental/dev_43_6_1428/dev_43_6_1428_supp.html
# https://nces.ed.gov/edat

VARFILE <- 'data/cache/eclsk2011vars.rds'
SUBSETFILE <- 'data/cache/eclsk_subset_2011.rds'
RAWFILE <- 'data/src/eclsk_2011_childk4.sav'

options(tidyverse.quiet = T)
library(tidyverse)
library(labelled)

eclsk2011 <- list()

eclsk2011$ALL_OCCASIONS <- list(1:8)

eclsk2011$expandMeasure <- function(measure, occasions) {
  # 'TKEEPS', c(1, 2) -> T1KEEPS, T2KEEPS
  if (is.null(occasions)) {
    measure
  } else {
    str_c(str_sub(measure, 1, 1), occasions, str_sub(measure, 2))
  }
}

eclsk2011$measures <- list(
  tibble(measure = c( # Teacher-rated ATL
      'XTCHAPP', 'TKEEPS', 'TSHOWS', 'TWORKS', 'TADAPTS', 'TFOLLOW', 'TPERSIS', 'TATTEN'
    ), occasions = eclsk2011$ALL_OCCASIONS, na_vals = list(c(-9, 5)), use_label = F
  ),
  tibble(measure = c( # Parent-rated ATL
      'PWRKFIN', 'PSHWINT', 'PCONCEN', 'PCHORES', 'PLEARN', 'PCREATV'
    ), occasions = list(c(1,2,4)), na_vals = list(c(-9, -8, -7, 5)), use_label = F
  ),
  tibble(measure = c( # TMCQ
      'XINTMCQ', 'TBSPTLD', 'TBLKARO', 'TBSPQIK', 'TBEZWAT',
      'TBEZDAC', 'TBEZDSL', 'TBFLWIN',
      'XATTMCQ', 'TBHTATN', 'TBHTTLK', 'TBPYATN', 'TBDSATN', 'TBPLANS', 'TBHTSLW'
    ), occasions = list(c(6,7,8)), na_vals = list(c(-9, 6)), use_label = F
  ),
  tibble(measure = c( # CBQ
      'XATTNFS', 'TBNOFIN', 'TBGCCLR', 'TBGCBLD', 'TBABSBK',
      'TBEZDAC', 'TBEZDSL', 'TBFLWIN',
      'XINBCNT', 'TBWTTSK', 'TBPLNAC', 'TBTRBST', 'TBAPRRK', 'TBSTNO'
    ), occasions = list(c(1,2,4)), na_vals = list(c(-9, 8)), use_label = F
  ),
  tibble(measure = c( # EF Card Sort
      'XNRSSCR'
    ), occasions = eclsk2011$ALL_OCCASIONS, na_vals = list(c(-9, -1)), use_label = F
  ),   
  tibble(measure = c( # Reading / math scores
    'XRSCALK4', 'XMSCALK4'
    ), occasions = eclsk2011$ALL_OCCASIONS, na_vals = list(-9), use_label = F
  ),
  tibble(measure = 'X_RACETHP_R', na_vals = list(-9), use_label = T),
  tibble(measure = 'X_CHSEX_R', na_vals = list(-9), use_label = T),
  tibble(measure = 'X1FIRKDG', na_vals = list(-9), use_label = T),
  tibble(measure = 'S_ID', na_vals = list(''), occasions = eclsk2011$ALL_OCCASIONS, use_label = F)
) %>% bind_rows()

eclsk2011$subset_vars <- map2(eclsk2011$measures$measure,
                              eclsk2011$measures$occasions,
                              eclsk2011$expandMeasure) %>%
                         unlist() %>%
                         c('CHILDID')

if (!file.exists(SUBSETFILE)) {
  require(haven)
  warning(paste(SUBSETFILE, "doesn't exist, creating..."))

  if (file.exists(VARFILE)) {
    avail_vars <- read_rds(VARFILE)

    navars <- eclsk2011$subset_vars[!(eclsk2011$subset_vars %in% avail_vars)]

    if (length(navars)) {
      stop(paste(navars, collapse=', '))
    }
  } 

  df <- read_spss(RAWFILE) %>%
        rename_at(vars(starts_with('G8')), ~str_replace(., 'G8', 'T8'))

  write_rds(colnames(df), VARFILE)

  df %>%
    select(eclsk2011$subset_vars) %>%
    write_rds(SUBSETFILE)
}

eclsk2011$subset <- read_rds(SUBSETFILE)

eclsk2011$subset_tall <- eclsk2011$measures %>%
      pmap(function (measure, occasions, na_vals, use_label) {
        allItems <- eclsk2011$expandMeasure(measure, occasions)
        
        # Subset to current measure 
        result <- eclsk2011$subset[allItems]
        
        # Convert vals to NA 
        if (!is.null(na_vals)) {
          result <- result %>%
                    mutate_at(vars(one_of(allItems)), `na_values<-`, na_vals) %>%
                    mutate_at(vars(one_of(allItems)), user_na_to_na)           
        }
        
        # Convert vals to their label
        if (use_label) {
          result <- result %>%
                    mutate_at(vars(one_of(allItems)), to_factor) %>%
                    mutate_at(vars(one_of(allItems)), as.character)           
        }
        
        # Everything should be a primitive type now. (dbl, chr, etc.)
        # Strip all attributes from columns
        result <- mutate_all(result, `attributes<-`, NULL)
        
        # Rename T1LEARN -> TLEARN_1
        if (!is.null(occasions)) {
          result <- rename_at(result, vars(one_of(allItems)),
                              str_replace, '(.)(\\d)(.*)', '\\1\\3__\\2')
        }
        
        result
      }) %>%
  bind_cols() %>%
  add_column(CHILDID = `attributes<-`(eclsk2011$subset$CHILDID, NULL), .before=0) %>%
  pivot_longer(matches('__'), names_to = c('.value', 'occasion'), names_sep = '__')

eclsk2011$validation_split <- function(df, id) {
  id_vals <- unique(df[id])
  
  train <- id_vals %>% sample_frac(0.5) %>% add_column(split = 'train')
  test <- id_vals %>% anti_join(train, by=id) %>% add_column(split = 'test')
  
  bind_rows(train, test)
}

eclsk2011$validation_split_new <- function(df, id) {
  spec = c(train = 0.33, test = 0.33, val = 0.33)
  
  id_vals <- unique(df[id]) 
  
  tibble(
    !!!id_vals,
    split = sample(cut(
      seq(nrow(id_vals)), 
      nrow(id_vals)*cumsum(c(0,spec)),
      labels = names(spec)
    ))
  )
}

set.seed(9001)

eclsk2011$study1 <- eclsk2011$subset_tall %>%
                    filter(occasion %in% c(1,2,4)) %>%
                    filter(X1FIRKDG == '1: YES') %>% # First time kindergartener
                    group_by(CHILDID) %>%
                    filter(var(as.numeric(S_ID)) == 0) %>% # At same school all occasions
                    ungroup() %>%
                    full_join(eclsk2011$validation_split(., 'CHILDID'), by='CHILDID')

eclsk2011$study2 <- eclsk2011$subset_tall %>%
                    filter(occasion %in% c(6,7,8)) %>%
                    filter(X1FIRKDG == '1: YES') %>% # First time kindergartener
                    group_by(CHILDID) %>%
                    filter(var(as.numeric(S_ID)) == 0) %>% # At same school all occasions
                    ungroup() %>%
                    full_join(eclsk2011$validation_split(., 'CHILDID'), by='CHILDID')

save(eclsk2011, file = 'data/eclsk2011.rda')