source('paper/tables/common.R')

df_val_parcel %>%
  select(`Approaches to Learning` = MATL_F1,
         `Engagement` = MENG_F2,
         `Attentional Focusing` = MATTEN_F3,
         `Inhibitory Control` = MINHIB_F4) %>%
  correlate(diagonal=1) %>%
  shave() %>%
  mutate_at(vars(-rowname), ~if_else(is.na(.), '', sprintf('%0.3f', .))) %>%
  rename(` ` = rowname) %>%
  kable('latex', booktabs=T, align=c('l', 'c'), escape=F) %>%
  kable_styling(latex_options = c('scale_down')) %>%
  save_latex_table('mmod_selected_parcel_correlations')