# http://supp.apa.org/psycarticles/supplemental/dev_43_6_1428/dev_43_6_1428_supp.html
# https://nces.ed.gov/edat

VARFILE <- 'data/ecls2011vars.rds'
SUBSETFILE <- 'data/eclsk_subset_2011.rds'
RAWFILE <- 'data/raw/eclsk_2011_childk4.sav'

library(tidyverse)

occasions <- c(1, 2, 4)
four_level_measures <- c('TKEEPS', 'TSHOWS', 'TWORKS', 'TADAPTS', 'TFOLLOW', 'TPERSIS', 'TATTEN',
                         'PWRKFIN', 'PSHWINT', 'PCONCEN', 'PCHORES', 'PLEARN', 'PCREATV')
seven_level_measures <- c('TBEZDAC', 'TBNOFIN', 'TBGCCLR', 'TBGCBLD', 'TBEZDSL', 'TBABSBK',
                          'TBWTTSK', 'TBPLNAC', 'TBTRBST', 'TBFLWIN', 'TBAPRRK', 'TBSTNO')
other_measures <- c('XNRSSCR', 'XCSBGSC')

all_measures <- c(four_level_measures, seven_level_measures, other_measures)

itemName = function(o, m) {
  # itemName(1, 'TKEEPS') -> T1KEEPS
  str_c(str_sub(m, 1, 1), o, str_sub(m, 2))    
}

allOccasionsFor = function(m) {
  unlist(map(occasions, ~itemName(., m)))
}
