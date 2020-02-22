source('scripts/eclsk2011_study1_tables/common.R')

nfactor_range <- c(2,3,4,5)

occasion_list <- c(
  `Fall of Kindergarten` = 1,
  `Spring of Kindergarten` = 2,
  `Spring of 1st Grade` = 4
)

onames <- c(
  `1` = 'Fall of Kindergarten',
  `2` = 'Spring of Kindergarten',
  `4` = 'Spring of 1st Grade'
)

fnames <- list(
  `2` = c('ATL+AF+IC', 'BE'),
  `3` = c('ATL', 'AF+IC', 'BE'),
  `4` = c('ATL', 'AF', 'IC', 'BE'),
  `5` = c('ATL', 'AF', 'AF2', 'IC', 'BE')
)

filter_occasion <- function(data, o) {
  data %>%
    filter(occasion == o) %>%
    select(-occasion) %>%
    na.omit()
}

efa_measures <- c('TKEEPS', 'TSHOWS', 'TWORKS', 'TADAPTS', 'TFOLLOW', 'TPERSIS', 'TATTEN',
                  'TBEZDAC', 'TBNOFIN', 'TBGCCLR', 'TBGCBLD', 'TBEZDSL', 'TBABSBK',
                  'TBWTTSK', 'TBTRBST', 'TBFLWIN', 'TBSTNO') # Omit: 'TBPLNAC', 'TBAPRRK'

do_explore <- function(occasion, nfactors) {
  require(psych)
  fsort_order <- list(
    `2` = list(
      `1` = c(1,2),
      `2` = c(1,2),
      `4` = c(1,2)
    ),
    `3` = list(
      `1` = c(2,1,3),
      `2` = c(2,1,3),
      `4` = c(1,2,3)
    ),
    `4` = list(
      `1` = c(1,2,3,4),
      `2` = c(1,3,2,4),
      `4` = c(1,2,3,4)
    ),
    `5` = list(
      `1` = c(1,5,2,3,4),
      `2` = c(1,5,4,2,3),
      `4` = c(2,3,5,1,4)
    )   
  )
  
  freflects <- list(
    `2` = list(
      `1` = c(),
      `2` = c(),
      `4` = c()
    ),
    `3` = list(
      `1` = c(),
      `2` = c(),
      `4` = c()
    ),
    `4` = list(
      `1` = c(2),
      `2` = c(2),
      `4` = c(2)
    ),
    `5` = list(
      `1` = c(2,3),
      `2` = c(2,3),
      `4` = c(2,3)
    )   
  )
  

  eclsk2011$study1 %>%
    filter(split == 'train') %>%
    select(c('occasion', efa_measures)) %>%
    filter_occasion(occasion) %>%
    fa(nfactors, rotate='promax') %>%
    `$<-`('score.cor', matrix(nrow=nfactors,ncol=nfactors)) %>% # TODO: this is a workaround for a fa.organize bug
    fa.organize(o=fsort_order[[as.character(nfactors)]][[as.character(occasion)]]) %>%
    reflect(freflects[[as.character(nfactors)]][[as.character(occasion)]])
}

make_loadings_table <- function(nfactors, foot = NULL) {
  map(occasion_list, function(occasion) {
      do_explore(occasion, nfactors) %>%
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
  add_column(efa_measures, .before=0) %>%
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
  do_explore(occasion, nfactors) %>%
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
make_correlations_table(1, 5)
make_correlations_table(2, 5)
make_correlations_table(4, 5)
