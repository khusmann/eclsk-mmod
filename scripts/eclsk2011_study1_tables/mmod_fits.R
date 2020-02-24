source('scripts/eclsk2011_study1_tables/common.R')

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

Sys.glob('data/res/eclsk2011_study1/mmod/*_result.rds') %>%
  map(read_rds) %>%
  setNames(map(., 'modelName')) %>%
  mmod_fits() %>%
  mutate(name = fct_relevel(name, 'theory', after=Inf)) %>%
  arrange(name) %>%
  extract(name, c('numFct', 'occ'), 'efa(.)occ(.+)') %>% 
  mutate(occ = map(str_split(occ, pattern=''), as.numeric)) %>% 
  unnest(occ) %>%
  mutate(occ = map_chr(str_split(occ, ''), str_c, collapse = ', ')) %>%
  mutate(occ = str_replace(occ, '1', 'Fall Kindergarten')) %>%
  mutate(occ = str_replace(occ, '2', 'Spring Kindergarten')) %>%
  mutate(occ = str_replace(occ, '4', 'Spring 1st Grade')) %>%
  mutate(numFct = replace_na(numFct, 3)) %>%
  filter(between(as.numeric(numFct), 2, 4)) %>%
  mutate(numFct = str_c(numFct, '-Factor'),
         occ = replace_na(occ, 'Theory')) %>%
  mutate_at(vars(n:bic), ~sprintf('%0.0f', .)) %>%
  mutate_at(vars(rmsea:tli), ~sprintf('%0.3f', .)) %>%
  mutate_at(vars(n:tli), ~cell_spec(., 'latex', bold = row_number() %in% c(7,8))) %>%
  transmute(`Structure Type` = numFct,
            `Structure Source` = occ,
            AIC = aic,
            BIC = bic,
            rmsea = rmsea,
            cfi = cfi,
            tli = tli) %>%
  kable('latex', caption='MMOD Model Fit Statistics of Candidate Factor Structures on Test Subset',
        label='mmod_fits', booktabs=T, escape=F) %>%
  kable_styling(latex_options = c('scale_down')) %>%
  collapse_rows(1:7) %>%
  footnote(general='According to the MMOD, the best-fitting factor structure was the 4-factor structure found in both the Fall and Spring of Kindergarten measurement occasions (fit statistics in bold)',
           threeparttable=T) %>%
  save_latex_table('mmod_fits')
