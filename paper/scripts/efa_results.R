source('paper/scripts/common.R')

nfactor_range <- c(2,3,4,5)

occasion_list <- c(
  `Fall of Kindergarten` = 1,
  `Spring of Kindergarten` = 2,
  `Spring of 1st Grade` = 4
)

make_loadings_table <- function(nfactors, foot = NULL) {
  map(occasion_list, function(occasion) {
      study1_explore(occasion, nfactors) %>%
      loadings() %>%
      {data.frame(matrix(as.numeric(.), attributes(.)$dim, dimnames=attributes(.)$dimnames))} %>%
      rownames_to_column('item') %>%
      pivot_longer(-item, names_to = 'name', values_to = 'value') %>%
      group_by(item) %>%
      mutate(value = cell_spec(sprintf('%0.3f', value),
                               'latex', bold = (near(abs(value), max(abs(value)))))) %>%
      ungroup() %>%
      pivot_wider(id_cols='item', names_from='name', values_from='value') %>%
      select(-item)
  }) %>%
  bind_cols() %>%
  add_column(study1_measures, .before=0) %>%
  add_column(c(rep('ATL', 7), rep('AF', 6), rep('IC', 4)), .before=0) %>%
  kable('latex', caption=paste0(nfactors, '-Factor Candidate Structures'),
        col.names = c('Theoretical scale','Measure',
                      rep(fnames[[as.character(nfactors)]], length(occasion_list))),
        booktabs=T, escape=F, label=paste0('candidate_factor_structures', nfactors),
        align = 'c') %>%
  kable_styling(latex_options=c('scale_down')) %>%
  column_spec(2+nfactors+seq_len(nfactors), background='gray!30') %>%
  add_header_above(c(' ' = 2, map_dbl(occasion_list, ~nfactors))) %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
  { if (!is.null(foot)) footnote(., general = foot, threeparttable = T) else . } %>%
  save_latex_table(paste0('candidate_factor_structures', nfactors))
}

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


footer <- 'Items in bold are the factor loadings with the highest absolute value across factors and included in the factor parcel.'

# 2-Factor Structures
make_loadings_table(2, footer)
make_correlations_table(1, 2)
make_correlations_table(2, 2)
make_correlations_table(4, 2)

# 3-Factor Structures
make_loadings_table(3, footer)
make_correlations_table(1, 3)
make_correlations_table(2, 3)
make_correlations_table(4, 3)

# 4-Factor Structures
make_loadings_table(4, footer)
make_correlations_table(1, 4)
make_correlations_table(2, 4)
make_correlations_table(4, 4)

# 5-Factor Structures
make_loadings_table(5, footer)
ake_correlations_table(1, 5)
make_correlations_table(2, 5)
make_correlations_table(4, 5)