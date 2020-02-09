source('paper/scripts/common.R')

df_val_parcel %>%
  select(`Approaches to Learning` = XTCHAPP_F1,
         `Attentional Focusing` = XATTNFS_F3,
         `Inhibitory Control` = XINBCNT_F4) %>% 
  correlate(diagonal=1) %>%
  shave() %>%
  mutate_at(vars(-rowname), ~if_else(is.na(.), '', sprintf('%0.3f', .))) %>%
  rename(` ` = rowname) %>%
  kable('latex', booktabs=T, align=c('l', 'c'), escape=F) %>%
  kable_styling(latex_options = c('scale_down')) %>%
  save_latex_table('theory_parcel_correlations')