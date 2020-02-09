library(tidyverse)
library(kableExtra)
library(corrr)

data(eclsk2011)

generate_all_tables <- function() {
  Sys.glob('paper/tables/*.R') %>%
    setdiff('paper/tables/common.R') %>%
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

# Create subset for candidate factor structures

filter_occasion <- function(data, o) {
  data %>%
    filter(occasion == o) %>%
    select(-occasion) %>%
    na.omit()
}

do_explore <- function(data, occasion, nfactors) {
  require(psych)
  data %>%
    filter_occasion(occasion) %>%
    fa(nfactors, rotate='promax')
}

study1_measures <- c('TKEEPS', 'TSHOWS', 'TWORKS', 'TADAPTS', 'TFOLLOW', 'TPERSIS', 'TATTEN',
                     'TBEZDAC', 'TBNOFIN', 'TBGCCLR', 'TBGCBLD', 'TBEZDSL', 'TBABSBK',
                     'TBWTTSK', 'TBTRBST', 'TBFLWIN', 'TBSTNO') # Omit: 'TBPLNAC', 'TBAPRRK'

df_train_measures <- eclsk2011$study1 %>%
  filter(split == 'train') %>%
  select(c('occasion', study1_measures))

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