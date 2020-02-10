source('paper/scripts/common.R')

make_correlations_table <- function(occasion, nfactors, caption) {
  flist <- fnames[[as.character(nfactors)]]
  tname <- paste0('candidate_factor_correlations', occasion, nfactors)
  study1_explore(occasion, nfactors) %>%
    `[[`('Phi') %>%
    `dimnames<-`(list(flist, flist)) %>%
    as_cordf(diagonal=1) %>%
    shave() %>%
    mutate_at(vars(-rowname), ~if_else(is.na(.), '', sprintf('%0.3f', .))) %>%
    rename(` ` = rowname) %>%
    kable('latex', booktabs=T, caption=caption, escape=F, label=tname) %>%
    kable_styling(latex_options = c()) %>%
    save_latex_table(tname)
}

make_correlations_table(1, 2, 'Fall of Kindergarten, 2-Factor structure')
make_correlations_table(2, 2, 'Spring of Kindergarten, 2-Factor structure')
make_correlations_table(4, 2, 'Spring of 1st Grade, 2-Factor structure')

make_correlations_table(1, 3, 'Fall of Kindergarten, 3-Factor structure')
make_correlations_table(2, 3, 'Spring of Kindergarten, 3-Factor structure')
make_correlations_table(4, 3, 'Spring of 1st Grade, 3-Factor structure')

make_correlations_table(1, 4, 'Fall of Kindergarten, 4-Factor structure')
make_correlations_table(2, 4, 'Spring of Kindergarten, 4-Factor structure')
make_correlations_table(4, 4, 'Spring of 1st Grade, 4-Factor structure')

make_correlations_table(1, 5, 'Fall of Kindergarten, 5-Factor structure')
make_correlations_table(2, 5, 'Spring of Kindergarten, 5-Factor structure')
make_correlations_table(4, 5, 'Spring of 1st Grade, 5-Factor structure')