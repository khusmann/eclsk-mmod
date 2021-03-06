# http://supp.apa.org/psycarticles/supplemental/dev_43_6_1428/dev_43_6_1428_supp.html
# https://nces.ed.gov/edat

VARFILE <- 'data/cache/eclsk2011_vars.rds'
SUBSETFILE <- 'data/cache/eclsk2011_subset.rds'
RAWFILE <- 'data/src/eclsk2011k5/childK5p.rds'

options(tidyverse.quiet = T)
library(tidyverse)
library(labelled)

eclsk2011 <- list()

eclsk2011$ALL_OCCASIONS <- list(1:9)

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
  tibble(measure = c( # CBQ
      'XATTNFS', 'TBNOFIN', 'TBGCCLR', 'TBGCBLD', 'TBABSBK',
      'TBEZDAC', 'TBEZDSL', 'TBFLWIN',
      'XINBCNT', 'TBWTTSK', 'TBPLNAC', 'TBTRBST', 'TBAPRRK', 'TBSTNO'
    ), occasions = list(c(1,2,4)), na_vals = list(c(-9, 8)), use_label = F
  ),
  tibble(measure = c( # Reading / math scores
      'XRSCALK5', 'XMSCALK5',
      'XRTHETK5', 'XMTHETK5',
      'XRSETHK5', 'XMSETHK5'
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
  warning(paste(SUBSETFILE, "doesn't exist, creating..."))

  dir.create(dirname(SUBSETFILE), showWarnings=F)
  dir.create(dirname(VARFILE), showWarnings=F)

  if (file.exists(VARFILE)) {
    avail_vars <- read_rds(VARFILE)

    navars <- eclsk2011$subset_vars[!(eclsk2011$subset_vars %in% avail_vars)]

    if (length(navars)) {
      stop(paste(navars, collapse=', '))
    }
  } 

  df <- read_rds(RAWFILE) %>%
        rename_at(vars(starts_with('G8')), ~str_replace(., 'G8', 'T8')) %>%
        rename_at(vars(starts_with('G9')), ~str_replace(., 'G9', 'T9'))

  write_rds(colnames(df), VARFILE)

  df %>%
    select(eclsk2011$subset_vars) %>%
    write_rds(SUBSETFILE)
}

eclsk2011$subset <- read_rds(SUBSETFILE)

eclsk2011$subset_tall <- eclsk2011$measures %>%
      pmap(function(measure, occasions, na_vals, use_label) {
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
        
        # Rename T1LEARN -> TLEARN__1
        if (!is.null(occasions)) {
          result <- rename_at(result, vars(one_of(allItems)),
                              str_replace, '(.)(\\d)(.*)', '\\1\\3__\\2')
        }
        
        result
      }) %>%
  bind_cols() %>%
  add_column(CHILDID = `attributes<-`(eclsk2011$subset$CHILDID, NULL), .before=0) %>%
  pivot_longer(matches('__'), names_to = c('.value', 'occasion'), names_sep = '__',
               names_ptypes = list(occasion = integer())) %>%
  mutate(grade = case_when(occasion == 1 ~ 0,
                           occasion == 2 ~ 0.5,
                           occasion == 3 ~ 1,
                           occasion == 4 ~ 1.5,
                           occasion == 5 ~ 2,
                           occasion == 6 ~ 2.5,
                           occasion == 7 ~ 3.5,
                           occasion == 8 ~ 4.5))

eclsk2011$validation_split <- function(df, id) {
  spec = c(train = 0.25, test = 0.25, val = 0.5)
  
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
                    filter(n_distinct(S_ID) == 1) %>% # At same school all occasions
                    ungroup() %>%
                    full_join(eclsk2011$validation_split(., 'CHILDID'), by='CHILDID')

save(eclsk2011, file = 'data/eclsk2011.rda')
