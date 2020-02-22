source('paper/scripts/common.R')

library(lmerTest)
library(broom.mixed)
library(corrr)

###################### Create parcel scores on validation set

df_val_parcel <- eclsk2011$study1 %>%
  filter(split == 'val') %>%
  filter_at(vars(TWORKS, TPERSIS, TSHOWS, TADAPTS, TKEEPS, TATTEN,
                 TBGCCLR, TBGCBLD, TBABSBK, TBEZDSL, TBTRBST, TBEZDAC, TBNOFIN,
                 TBSTNO, TBWTTSK, TFOLLOW, TBFLWIN), all_vars(!is.na(.))) %>% # Exclude NAs
  mutate(
    TFOLLOW_sc = TFOLLOW/4*7, # Scaled version of TFOLLOW to use with MINHIB_F4
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
  group_by(CHILDID) %>%
  filter(n()==3) %>% # Data present at all occasions
  ungroup() %>%
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

############### Define Models

models_read <- list(
  theory = lmer(XRTHETK5 ~ grade + XATTNFS_F3 + XTCHAPP_F1 + XINBCNT_F4 + (1|CHILDID), df_val_parcel,
                REML=F, control=lmerControl(optimizer='bobyqa')),
  
  f4 = lmer(XRTHETK5 ~ grade + MATL_F1 + MENG_F2 + MINHIB_F4 + MATTEN_F3 + (1|CHILDID), df_val_parcel,
            REML=F, control=lmerControl(optimizer='bobyqa'))
)

models_math <- list(
  theory = lmer(XMTHETK5 ~ grade + XATTNFS_F3 + XTCHAPP_F1 + XINBCNT_F4 + (1|CHILDID), df_val_parcel,
                REML=F, control=lmerControl(optimizer='bobyqa')),
  
  f4 = lmer(XMTHETK5 ~ grade + MATL_F1 + MENG_F2 + MINHIB_F4 + MATTEN_F3 + (1|CHILDID), df_val_parcel,
            REML=F, control=lmerControl(optimizer='bobyqa'))
  
)

################## Run models

models <- tibble(
  type = c('read', 'math'),
  model = list(models_read, models_math),
  struct = map(model, names)
) %>%
  unnest(c(model, struct)) %>%
  mutate(id = str_c(type, struct, sep='.'),
         summary = map(model, summary),
         tdy = map(model, tidy, conf.int=T, conf.method='Wald', quiet=T),
         glnc = map(model, glance))

#################### Build table

table_col <- function(mod) {
  tidy_mod <- tidy(mod)
  glance_mod <- glance(mod)
  
  format_est <- function(i) {
    tbl <- tidy_mod %>%
      filter(str_detect(term, i))
    
    if (nrow(tbl) == 0) {
      ''
    } else {
      sig = tbl[['p.value']] < 0.05
      if (is.na(sig) || !sig) {
        sprintf('% 06.3f ', tbl[['estimate']])
      } else {
        sprintf('% 06.3f*', tbl[['estimate']])
      }
    }
  }
  
  format_se <- function(i) {
    tbl <- tidy_mod %>%
      filter(str_detect(term, i))   
    if (nrow(tbl) == 0 || is.na(tbl[['std.error']])) {
      '' 
    } else {
      sprintf('% 05.3f', tbl[['std.error']])
    }
  }
  
  format_mod <- function(i) {
    sprintf('%0.0f', glance_mod[[i]])
  }
  
  mappings <- c(
    `Approaches to Learning` = '.*F1',
    `Attentional Focusing` = '.*F3',
    `Inhibitory Control` = '.*F4',
    `Behavioral Engagement` = '.*F2',
    `(Intercept)` = '^\\(Intercept\\)',
    `Grade` = '^grade',      
    `sd(Intercept)` = 'sd__\\(Intercept\\)',
    `sd(resid)` = 'sd__Observation'
  )
  
  mappings_glnc <- c(
    `LL` = 'logLik',
    `df` = 'df.residual',
    `AIC` = 'AIC',
    `BIC` = 'BIC'
  )
  
  bind_rows(
    tibble(
      term = names(mappings),
      est = map_chr(mappings, format_est),
      se = map_chr(mappings, format_se)
    ),
    tibble(
      term = names(mappings_glnc),
      est = map_chr(mappings_glnc, format_mod),
      se = ''
    )
  )
  
}

models %>%
  mutate(tbl_c = map(model, table_col)) %>%
  select(id, tbl_c) %>%
  unnest(tbl_c) %>%
  pivot_longer(-c(id, term), names_to = 'var', values_to = 'val') %>%
  mutate(id = str_c(id, var, sep='.')) %>%
  select(-var) %>%
  pivot_wider(names_from=id, values_from=val) %>%
  set_names(c(' ', rep(c('Estimate', 'SE'), 4))) %>%
  kable('latex', caption='Results from multilevel models predicting reading and math achievement on the validation data set using the 4-factor structure chosen by best MMOD fit as compared to the theoretical factor structure', label='mlm_results',
        booktabs=T, align=c('l', 'c'), escape=F) %>%
  kable_styling(latex_options = c('scale_down')) %>%
  add_header_above(c(' ' = 1,
                     rep(c('Theory 3 Factor' = 2,
                           'MMOD 4 Factor' = 2), 2))
  )  %>%
  add_header_above(c(' ' = 1, 'Reading Theta Score' = 4, 'Math Theta Score' = 4)) %>%
  group_rows(index=c('Factor Fixed Effects' = 4,
                     'Other Fixed Effects' = 2,
                     'Random Effects' = 2,
                     'Model Fit' = 4)) %>%
  save_latex_table('mlm_results')
