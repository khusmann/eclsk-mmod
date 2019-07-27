source('scripts/eclsk2011env.R')

library(haven)

subsetvars <- c(
  # IDs
  'CHILDID', 'PARENTID',
  # Race
  'X_RACETHP_R', 'X_WHITE_R', 'X_BLACK_R', 'X_HISP_R', 'X_ASIAN_R',
  # Features
  'X_CHSEX_R', 'X1KAGE_R', 'X2KAGE_R', 'X3AGE', 'X4AGE', 'X5AGE', 'X6AGE', 'X7AGE', 'X8AGE',
  # IRT Reading
  'X1RSCALK4', 'X2RSCALK4', 'X3RSCALK4', 'X4RSCALK4', 'X5RSCALK4', 'X6RSCALK4', 'X7RSCALK4', 'X8RSCALK4',
  # IRT Math
  'X1MSCALK4', 'X2MSCALK4', 'X3MSCALK4', 'X4MSCALK4', 'X5MSCALK4', 'X6MSCALK4', 'X7MSCALK4', 'X8MSCALK4',
  # Vars of interest
  'X1TCHPER', 'X1TCHEXT', 'X1TCHINT', 'X1TCHAPP', 'X2SSCALK4',
  # Item level approaches to learning
  'T1KEEPS', 'T1SHOWS', 'T1WORKS', 'T1ADAPTS', 'T1FOLLOW', 'T1PERSIS', 'T1ATTEN',
  'T2KEEPS', 'T2SHOWS', 'T2WORKS', 'T2ADAPTS', 'T2FOLLOW', 'T2PERSIS', 'T2ATTEN',
  'T4KEEPS', 'T4SHOWS', 'T4WORKS', 'T4ADAPTS', 'T4FOLLOW', 'T4PERSIS', 'T4ATTEN',
  # Parent ATL
  'P1WRKFIN', 'P1SHWINT', 'P1CONCEN', 'P1CHORES', 'P1LEARN', 'P1CREATV',
  'P2WRKFIN', 'P2SHWINT', 'P2CONCEN', 'P2CHORES', 'P2LEARN', 'P2CREATV',
  'P4WRKFIN', 'P4SHWINT', 'P4CONCEN', 'P4CHORES', 'P4LEARN', 'P4CREATV',
  # Item level child behavior
  'T1BEZDAC', 'T1BNOFIN', 'T1BGCCLR', 'T1BGCBLD', 'T1BEZDSL', 'T1BABSBK', 'T1BWTTSK', 'T1BPLNAC', 'T1BTRBST', 'T1BFLWIN', 'T1BAPRRK', 'T1BSTNO',
  'T2BEZDAC', 'T2BNOFIN', 'T2BGCCLR', 'T2BGCBLD', 'T2BEZDSL', 'T2BABSBK', 'T2BWTTSK', 'T2BPLNAC', 'T2BTRBST', 'T2BFLWIN', 'T2BAPRRK', 'T2BSTNO',
  'T4BEZDAC', 'T4BNOFIN', 'T4BGCCLR', 'T4BGCBLD', 'T4BEZDSL', 'T4BABSBK', 'T4BWTTSK', 'T4BPLNAC', 'T4BTRBST', 'T4BFLWIN', 'T4BAPRRK', 'T4BSTNO',
  # Item level exec function
  'X1DCCSTOT', 'X1CSPRES', 'X1CSPSSC', 'X1CSBGSC',
  'X2DCCSTOT', 'X2CSPRES', 'X2CSPSSC', 'X2CSBGSC',
  'X4DCCSTOT', 'X4CSPRES', 'X4CSPSSC', 'X4CSBGSC',
  # exec function
  'X1NRSSCR',
  'X2NRSSCR',
  'X4NRSSCR',
  # First kindergarten
  'X1FIRKDG',
  # School ids
  'S1_ID', 'S2_ID', 'S3_ID', 'S4_ID', 'S5_ID', 'S6_ID', 'S7_ID', 'S8_ID'
)

if (file.exists(VARFILE)) {
  ecls2011vars <- read_rds(VARFILE)

  navars <- subsetvars[!(subsetvars %in% ecls2011vars)]

  if (length(navars)) {
    stop(paste(navars, collapse=', '))
  }
} 

df <- read_spss(RAWFILE)

write_rds(colnames(df), VARFILE)

select(df)

df %>%
  select(subsetvars) %>%
  write_rds(SUBSETFILE)
