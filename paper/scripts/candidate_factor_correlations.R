source('paper/scripts/common.R')

make_correlations_table <- function(occasion, nfactors) {
  caption <- paste0(nfactors, '-Factor Candidate Structure Intercorrelations: ',
                    onames[[as.character(occasion)]])
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

make_correlations_table(1, 2)
make_correlations_table(2, 2)
make_correlations_table(4, 2)

make_correlations_table(1, 3)
make_correlations_table(2, 3)
make_correlations_table(4, 3)

make_correlations_table(1, 4)
make_correlations_table(2, 4)
make_correlations_table(4, 4)

make_correlations_table(1, 5)
make_correlations_table(2, 5)
make_correlations_table(4, 5)