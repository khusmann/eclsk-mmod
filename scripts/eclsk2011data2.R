VARFILE <- 'data/res/eclsk2011vars.rds'
SUBSETFILE <- 'data/res/eclsk_subset_2011.rds'
RAWFILE <- 'data/src/eclsk_2011_childk4.sav'

options(tidyverse.quiet = T)
library(tidyverse)
library(labelled)

eclsk2011 <- list()

eclsk2011$subset <- read_rds(SUBSETFILE)

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
      'XATTMCQ', 'TBHTATN', 'TBHTTLK', 'TBPYATN', 'TBDSATN', 'TBPLANS', 'TBHTSLW'
    ), occasions = list(c(6,7,8)), na_vals = list(c(-9, 6)), use_label = F
  ),
  tibble(measure = c( # CBQ
      'XATTNFS', 'TBNOFIN', 'TBGCCLR', 'TBGCBLD', 'TBABSBK',
      'XINBCNT', 'TBWTTSK', 'TBPLNAC', 'TBTRBST', 'TBAPRRK', 'TBSTNO'
    ), occasions = list(c(1,2,4)), na_vals = list(c(-9, 8)), use_label = F
  ),
  tibble(measure = c( # TMCQ+CBQ
      'TBEZDAC', 'TBEZDSL', 'TBFLWIN'
    ), occasions = list(c(1,2,4,6,7,8)), na_vals = list(c(-9, 8)), use_label = F
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
                         unlist()

eclsk2011$subset_tall <- eclsk2011$measures %>%
      pmap(function (measure, occasions, na_vals, use_label) {
        allItems <- eclsk2011$expandMeasure(measure, occasions)
        
        # Subset to current measure 
        result <- eclsk2011$subset[c('CHILDID', allItems)]
        
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

        # Convert wide to tall
        if (!is.null(occasions)) {
          result <- result %>%
                    gather(m, v, allItems) %>%
                    mutate(occasion = as.numeric(str_sub(m, 2, 2)),
                           m = str_replace(measure, '\\d+', '')) %>%
                    spread(m, v)           
        }
        
        result
      }) %>%
  reduce(full_join)
