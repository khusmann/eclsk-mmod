library(tidyverse)
library(kableExtra)
library(corrr)

data(eclsk2011)

generate_all_tables <- function() {
  Sys.glob('paper/scripts/*.R') %>%
    setdiff('paper/scripts/common.R') %>%
    map(source) %>%
    invisible()
}

save_latex_table <- function(latex_table, name) {
  preview_file <- latex_table %>%
    save_kable(file.path('paper', 'tables', paste0(name, '.pdf')))
  
  latex_table %>%
    cat(file=file.path('paper', 'tables', paste0(name, '.tex')))
  
  viewer <- getOption('viewer')
  
  if (!is.null(viewer)) {
    viewer(preview_file)  
  }  
}

study1_measures <- c('TKEEPS', 'TSHOWS', 'TWORKS', 'TADAPTS', 'TFOLLOW', 'TPERSIS', 'TATTEN',
                     'TBEZDAC', 'TBNOFIN', 'TBGCCLR', 'TBGCBLD', 'TBEZDSL', 'TBABSBK',
                     'TBWTTSK', 'TBTRBST', 'TBFLWIN', 'TBSTNO') # Omit: 'TBPLNAC', 'TBAPRRK'

# Create subset for candidate factor structures
df_train_measures <- eclsk2011$study1 %>%
  filter(split == 'train') %>%
  select(c('occasion', study1_measures))

fnames <- list(
  `2` = c('ATL+AF+IC', 'BE'),
  `3` = c('ATL', 'AF+IC', 'BE'),
  `4` = c('ATL', 'AF', 'IC', 'BE'),
  `5` = c('ATL', 'AF', 'AF2', 'IC', 'BE')
)

fsort_order <- list(
  `2` = list(
    `1` = c(1,2),
    `2` = c(1,2),
    `4` = c(1,2)
  ),
  `3` = list(
    `1` = c(2,1,3),
    `2` = c(2,1,3),
    `4` = c(2,1,3)
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

study1_explore <- function(occasion, nfactors) {
  require(psych)
  df_train_measures %>%
    filter(occasion == !!occasion) %>%
    select(-occasion) %>%
    na.omit() %>%
    fa(nfactors, rotate='promax') %>%
    `$<-`('score.cor', matrix(nrow=nfactors,ncol=nfactors)) %>% # TODO: this is a workaround for a fa.organize bug
    fa.organize(o=fsort_order[[as.character(nfactors)]][[as.character(occasion)]]) %>%
    reflect(freflects[[as.character(nfactors)]][[as.character(occasion)]])
}

# Create parcel scores on validation set

df_val_parcel <- eclsk2011$study1 %>%
  filter(split == 'val') %>%
  filter_at(vars(TWORKS, TPERSIS, TSHOWS, TADAPTS, TKEEPS, TATTEN,
                 TBGCCLR, TBGCBLD, TBABSBK, TBEZDSL, TBTRBST, TBEZDAC, TBNOFIN,
                 TBSTNO, TBWTTSK, TFOLLOW, TBFLWIN), all_vars(!is.na(.))) %>% # Exclude NAs
  mutate(
    TFOLLOW_sc = TFOLLOW/4*7, # Scaled version of TFOLLOW to use with MINHIB_F4
#    TFOLLOW_sc = TFOLLOW, # Use raw score of TFOLLOW instead of scaling to match other vals
  ) %>%
  mutate( # Compute parcel scores for MMOD-chosen factor structure
    MATL_F1 = rowMeans(cbind(TWORKS, TPERSIS, TSHOWS, TADAPTS, TKEEPS, TATTEN), na.rm=T),
    MENG_F2 = rowMeans(cbind(TBGCCLR, TBGCBLD, TBABSBK), na.rm=T),
    MATTEN_F3 = rowMeans(cbind(8-TBEZDSL, 8-TBTRBST, 8-TBEZDAC, 8-TBNOFIN), na.rm=T),
    MINHIB_F4 = rowMeans(cbind(TBSTNO, TBWTTSK, TFOLLOW_sc, TBFLWIN), na.rm=T)
  ) %>%
  mutate( # Compute parcel scores for theoretical structure
    XTCHAPP_F1 = rowMeans(cbind(TWORKS, TPERSIS, TSHOWS, TADAPTS,
                                      TKEEPS, TATTEN, TFOLLOW), na.rm=T),
    XATTNFS_F3 = rowMeans(cbind(8-TBEZDAC, 8-TBNOFIN, 8-TBEZDSL,
                                      TBGCBLD, TBGCCLR, TBABSBK), na.rm=T),
    XINBCNT_F4 = rowMeans(cbind(8-TBTRBST, TBSTNO, TBWTTSK,
                                      TBFLWIN, TBPLNAC, TBAPRRK), na.rm=T)
  ) %>%
  mutate_at( # Standardize all scales (mean=0, sd=1)
    vars(
      MATL_F1, MENG_F2, MATTEN_F3, MINHIB_F4,
      XTCHAPP_F1, XATTNFS_F3, XINBCNT_F4,
      XTCHAPP, XATTNFS, XINBCNT
    ), scale
  )

# Verify that our manually created parcels scores for the theoretical structure match
# those created by the ECLS-K:2011
stopifnot(all(near(df_val_parcel$XTCHAPP_F1, df_val_parcel$XTCHAPP, .01)))
stopifnot(all(near(df_val_parcel$XATTNFS_F3, df_val_parcel$XATTNFS, .01)))
stopifnot(all(near(df_val_parcel$XINBCNT_F4, df_val_parcel$XINBCNT, .01)))
