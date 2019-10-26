# http://supp.apa.org/psycarticles/supplemental/dev_43_6_1428/dev_43_6_1428_supp.html
# https://nces.ed.gov/edat

VARFILE <- 'data/res/eclsk2011vars.rds'
SUBSETFILE <- 'data/res/eclsk_subset_2011.rds'
RAWFILE <- 'data/src/eclsk_2011_childk4.sav'

options(tidyverse.quiet = T)
library(tidyverse)

itemName = function(o, m) {
  # itemName(1, 'TKEEPS') -> T1KEEPS
  str_c(str_sub(m, 1, 1), o, str_sub(m, 2))    
}

allOccasionsFor = function(measures, occasions) {
  unlist(map(occasions, ~itemName(., measures)))
}

eclsk2011measures <- list(
  four_level = c(
    allOccasionsFor(c('XTCHAPP'), c(1,2,3,4,5,6,7,8)),
    allOccasionsFor(c('TKEEPS', 'TSHOWS', 'TWORKS', 'TADAPTS', 'TFOLLOW', 'TPERSIS', 'TATTEN'), c(1,2,3,4,5,6,7,8)), # Teacher-rated ATL
    allOccasionsFor(c('PWRKFIN', 'PSHWINT', 'PCONCEN', 'PCHORES', 'PLEARN', 'PCREATV'), c(1,2,4)) # Parent ATL
  ),
  five_level = c( 
    allOccasionsFor(c('XATTMCQ', 'XINTMCQ'), c(6,7,8)),
    allOccasionsFor(c('TBEZDSL', 'TBSPTLD', 'TBLKARO', 'TBSPQIK', 'TBEZDAC', 'TBEZWAT'), c(6,7,8)), # TMCQ Inhibition
    allOccasionsFor(c('TBHTATN', 'TBHTTLK', 'TBPYATN', 'TBDSATN', 'TBPLANS', 'TBFLWIN', 'TBHTSLW'), c(6,7,8)) # TMCQ Attention
  ),
  seven_level = c(
    allOccasionsFor(c('XATTNFS', 'XINBCNT'), c(1,2,4)),
    allOccasionsFor(c('TBEZDAC', 'TBNOFIN', 'TBGCCLR', 'TBGCBLD', 'TBEZDSL', 'TBABSBK'), c(1,2,4)), # CBQ Attentional Focusing
    allOccasionsFor(c('TBWTTSK', 'TBPLNAC', 'TBTRBST', 'TBFLWIN', 'TBAPRRK', 'TBSTNO'), c(1,2,4)) # CBQ Inhibitory Control
  ),
  numbers = c(
    allOccasionsFor(c('XNRSSCR'), c(1,2,3,4,5,6,7,8)), # EF: Card sort task
    allOccasionsFor(c('XRSCALK4', 'XMSCALK4'), c(1,2,3,4,5,6,7,8)) # Reading + Math scale scores
  ),
  factors = c(
    'CHILDID', # Unique ID
    'X_CHSEX_R', # Sex
    'X_RACETHP_R', # Race
    'X1FIRKDG' # First-time kindergartener
  ),
  strings = c(
    allOccasionsFor(c('S_ID'), c(1,2,3,4,5,6,7,8)) # School ID
  )
)

eclsk2011measures$all <- unlist(eclsk2011measures, use.name=F)
