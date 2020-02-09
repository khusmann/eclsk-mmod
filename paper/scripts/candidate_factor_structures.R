source('paper/scripts/common.R')

nfactor_range <- c(2,3,4,5)

make_loadings_table <- function(occasion, caption) {
  map(nfactor_range, function(nfactors) {
    df_train_measures %>%
      do_explore(occasion, nfactors) %>%
      loadings() %>%
      {data.frame(matrix(as.numeric(.), attributes(.)$dim, dimnames=attributes(.)$dimnames))} %>%
      rownames_to_column('item') %>%
      pivot_longer(-item, names_to = 'name', values_to = 'value') %>%
      group_by(item) %>%
      mutate(value = cell_spec(sprintf('%0.3f', value), 'latex',
                               bold = (near(abs(value), max(abs(value)))))) %>%
      ungroup() %>%
      pivot_wider(id_cols='item', names_from='name', values_from='value') %>%
      select(-item)
  }) %>%
  bind_cols() %>%
  add_column(study1_measures, .before=0) %>%
  kable('latex', caption=caption,
        col.names = c('', unlist(map(nfactor_range, ~paste0('F', 1:.)))),
        booktabs=T, escape=F, label=paste0('candidate_factor_structures', occasion)) %>%
  kable_styling(latex_options=c('scale_down')) %>%
  add_header_above(c(' ' = 1,
                     '2-Factor' = 2,
                     '3-Factor' = 3,
                     '4-Factor' = 4,
                     '5-Factor' = 5)) %>%
  column_spec(2:3, background='gray!30') %>%
  column_spec(7:10, background='gray!30') %>%
  pack_rows(index=c('Items from theoretical ATL scale' = 7,
                   'Items from theoretical AF scale' = 6,
                   'Items from theoretical IC scale' = 4)) %>%
  landscape() %>%
  save_latex_table(paste0('candidate_factor_structures', occasion))
}

make_loadings_table(1, 'Fall Kindergarten')
make_loadings_table(2, 'Spring Kindergarten')
make_loadings_table(4, 'Spring 1st Grade')