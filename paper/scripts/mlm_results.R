source('paper/scripts/common.R')

library(lmerTest)
library(broom.mixed)
library(corrr)

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
    `Engagement` = '.*F2',
    `Attentional Focusing` = '.*F3',
    `Inhibitory Control` = '.*F4',
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

latex_table <- models %>%
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
