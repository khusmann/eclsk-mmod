source('paper/tables/common.R')

mmod_results <- function(studyname) {
  mmod_results <- Sys.glob(paste0('data/res/', studyname, '/*_result.rds')) %>%
                  map(read_rds)
  names(mmod_results) <- map(mmod_results, 'modelName')
  mmod_results
}

mmod_fits <- function(mmod_results) {
  tibble(
    name=map_chr(mmod_results, 'modelName'),
    n=map_dbl(mmod_results, 'numObs'),
    chisq=map_dbl(mmod_results, 'Chi'),
    dof=map_dbl(mmod_results, 'ChiDoF'),
    `-2ll`=map_dbl(mmod_results, 'Minus2LogLikelihood'),
    aic=map_dbl(mmod_results, 'AIC.Mx'),
    bic=map_dbl(mmod_results, 'BIC.Mx'),
    rmsea=map_dbl(mmod_results, 'RMSEA'),
    cfi=map_dbl(mmod_results, 'CFI'),
    tli=map_dbl(mmod_results, 'TLI')
  )
}

study1_results <- mmod_results('eclsk2011_study1')
study1_fits <- mmod_fits(study1_results)

study1_fits %>%
  mutate(name = fct_relevel(name, 'theory', after=0)) %>%
  arrange(name) %>%
  extract(name, c('numFct', 'occ'), 'efa(.)occ(.+)') %>%
  mutate(occ = map_chr(str_split(occ, ''), str_c, collapse = ', ')) %>%
  mutate(occ = str_replace(occ, '1, 2, 4', 'All')) %>%
  mutate(occ = str_replace(occ, '1', 'Fall Kindergarten')) %>%
  mutate(occ = str_replace(occ, '2', 'Spring Kindergarten')) %>%
  mutate(occ = str_replace(occ, '4', 'Spring 1st Grade')) %>%
  mutate(numFct = replace_na(numFct, 3),
         occ = replace_na(occ, '-')) %>%
  mutate_at(vars(n:bic), ~sprintf('%0.0f', .)) %>%
  mutate_at(vars(rmsea:tli), ~sprintf('%0.3f', .)) %>%
  filter(numFct != '5') %>%
  select(-numFct) %>%
  rename(`Occasions With Same Structure (from training set)` = occ,
         `N` = n,
         `Chisq` = chisq,
         `df` = dof,
         `-2LL` = `-2ll`,
         `AIC` = aic,
         `BIC` = bic,
         rmsea = rmsea,
         cfi = cfi,
         tli = tli) %>%
  kable('latex', caption='MMOD Model Fit Statistics of Candidate Factor Structures',
        label='mmod_fits', booktabs=T, align=c('l', rep('l', 10)), escape=F) %>%
  kable_styling(latex_options = c('scale_down')) %>%
  row_spec(1, bold = T) %>%
  row_spec(5, bold = T) %>%
  group_rows(index=c('Theoretical Structure (3 Factors)' = 1,
                     'EFA 2 Factor' = 1,
                     'EFA 3 Factor' = 2,
                     'EFA 4 Factor' = 2), latex_gap_space='2em') %>%
  add_header_above(c(' ' = 1, 'MMOD Model Fit Statistics (on test set)' = 9)) %>%
  save_latex_table('mmod_fits')