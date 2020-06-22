# http://supp.apa.org/psycarticles/supplemental/dev_43_6_1428/dev_43_6_1428_supp.html
# https://nces.ed.gov/edat

VARFILE <- 'data/cache/eclsk_vars.rds'
SUBSETFILE <- 'data/cache/eclsk_subset.rds'
RAWFILE <- 'data/src/eclsk8/childk8p.rds'

options(tidyverse.quiet = T)
library(tidyverse)
library(labelled)

eclsk <- list()

eclsk$ALL_OCCASIONS <- list(1:7)

eclsk$expandMeasure <- function(measure, occasions) {
  # 'TKEEPS', c(1, 2) -> T1KEEPS, T2KEEPS
  if (is.null(occasions)) {
    measure
  } else {
    str_c(str_sub(measure, 1, 1), occasions, str_sub(measure, 2))
  }
}

eclsk$measures <- list(
  tibble(measure = c( # Teacher-rated ATL
      'TLEARN', 'TEXTERN', 'TINTERN', 'TINTERP'
    ), occasions = list(c(1,2,4,5,6)), na_vals = list(c(-9)), use_label = F
  ),
  tibble(measure = c( # Reading Tutoring
      'TTTRRD', 'TSGRDG'
    ), occasions = list(c(2, 4, 5, 6)), na_vals = list(c(3, -9, -8, -7)), use_label = T
  ),
  tibble(measure = c( # Reading / math scores
      'CR4RSCL', 'CR4MSCL', # Scale score
      'CR4RTSC', 'CR4MTSC', # T score
      'CR4RTHT_R', 'CR4MTHT_R'  # Theta score
    ), occasions = eclsk$ALL_OCCASIONS, na_vals = list(-9), use_label = F
  ),
  tibble(measure = c('T4REGVWL', 'T4IRGVWL', 'T4STORY', 'T4RD1FLN'), na_vals = list(c(7, -9, -8, -7)), use_label = F),
  tibble(measure = c('RACE', 'GENDER'), na_vals = list(-9), use_label = T),
  tibble(measure = 'P1FIRKDG', na_vals = list(c(-9, -8)), use_label = T),
  tibble(measure = 'S_ID', na_vals = list(''), occasions = eclsk$ALL_OCCASIONS, use_label = F)
) %>% bind_rows()

eclsk$subset_vars <- map2(eclsk$measures$measure,
                          eclsk$measures$occasions,
                          eclsk$expandMeasure) %>%
                         unlist() %>%
                         c('CHILDID')

if (!file.exists(SUBSETFILE)) {
  warning(paste(SUBSETFILE, "doesn't exist, creating..."))

  dir.create(dirname(SUBSETFILE), showWarnings=F)
  dir.create(dirname(VARFILE), showWarnings=F)

  if (file.exists(VARFILE)) {
    avail_vars <- read_rds(VARFILE)

    navars <- eclsk$subset_vars[!(eclsk$subset_vars %in% avail_vars)]

    if (length(navars)) {
      stop(paste(navars, collapse=', '))
    }
  } 

  df <- read_rds(RAWFILE) %>%
        rename(T6TTRRD = G6TTRRD,
               T6SGRDG = G6SGRDG)
#        rename_at(vars(starts_with('G6')), ~str_replace(., 'G6', 'T6')) %>%
#        rename_at(vars(starts_with('G7')), ~str_replace(., 'G7', 'T7'))

  write_rds(colnames(df), VARFILE)

  df %>%
    select(eclsk$subset_vars) %>%
    write_rds(SUBSETFILE)
}

eclsk$subset <- read_rds(SUBSETFILE)

eclsk$subset_tall <- eclsk$measures %>%
      pmap(function(measure, occasions, na_vals, use_label) {
        allItems <- eclsk$expandMeasure(measure, occasions)
        
        # Subset to current measure 
        result <- eclsk$subset[allItems]
        
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
  add_column(CHILDID = `attributes<-`(eclsk$subset$CHILDID, NULL), .before=0) %>%
  pivot_longer(matches('__'), names_to = c('.value', 'occasion'), names_sep = '__',
               names_ptypes = list(occasion = integer())) %>%
  mutate(grade = case_when(occasion == 1 ~ 0,
                           occasion == 2 ~ 0.5,
                           occasion == 3 ~ 1,
                           occasion == 4 ~ 1.5,
                           occasion == 5 ~ 3,
                           occasion == 6 ~ 5,
                           occasion == 7 ~ 8))

eclsk$validation_split <- function(df, id) {
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

#eclsk$study1 <- eclsk$subset_tall %>%
#                filter(occasion %in% c(1,2,4)) %>%
#                filter(X1FIRKDG == '1: YES') %>% # First time kindergartener
#                group_by(CHILDID) %>%
#                filter(n_distinct(S_ID) == 1) %>% # At same school all occasions
#                ungroup() %>%
#                full_join(eclsk$validation_split(., 'CHILDID'), by='CHILDID')

save(eclsk, file = 'data/eclsk.rda')
