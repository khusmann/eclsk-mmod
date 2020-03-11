options(tidyverse.quiet = T)
library(tidyverse)
library(labelled)

DAT_FILE <- 'data/src/eclsk2011k5/childK5p.dat.fwf.gz'
SPS_FILE <- 'data/src/eclsk2011k5/ECLSK2011_K5PUF.sps'
OUTPUT_FILE <- 'data/src/eclsk2011k5/childK5p.rds'

full_syntax <- read_lines(SPS_FILE)
fwf_syntax <- full_syntax[12:26097]
label_syntax <- full_syntax[26101:52160]
val_syntax <- full_syntax[52164:142239]

fwf_defs <- tibble(
  raw = fwf_syntax
) %>%
  extract(raw, c('cname', 'begin', 'end', 'type_str'), '   (.*) (\\d+)-(\\d+) ?(.*)', convert=T) %>%
  mutate(len = end-begin+1,
         type = case_when(
           type_str == '(A)' ~ 'c',
           type_str == '' ~ 'n',
           TRUE ~ 'd'
         )
  ) %>%
  filter(!is.na(cname))


label_defs <- tibble(
  raw = label_syntax
) %>%
  extract(raw, c('cname', 'label'), '      (.*)        "(.*)"')

all_defs <- full_join(fwf_defs, label_defs, by='cname')

cat('-- Unpacking value types\n')
val_defs <- tibble(
  begin = str_which(val_syntax, '   / '),
  end = lead(begin, default=length(val_syntax)),
  cname = str_match(val_syntax[begin], '   / (.*)')[,2],
  val_dict = map2(begin+1, end-1, function(b, e) {
    result <- tibble(
      raw = val_syntax[b:e]
    ) %>%
      extract(raw, c('val', 'label'), '      (.*)  "(.*)"')
    if (str_sub(result$val[1], -1) == '"') {
      result <- result %>%
        mutate(val = str_sub(val, 2, -2))
    } else {
      result <- result %>%
        mutate(val = as.integer(val))
    }
    setNames(result$val, result$label)
  })
)

cat('-- Loading data set\n')
childK5p <- read_fwf(DAT_FILE, fwf_widths(all_defs$len, all_defs$cname), paste(all_defs$type, collapse=''))

cat('-- Setting variable labels\n')
var_label(childK5p) <- all_defs$label

cat('-- Set value labels\n')
val_labels(childK5p) <- setNames(val_defs$val_dict, val_defs$cname)

cat('-- Writing file\n')
write_rds(childK5p, OUTPUT_FILE, compress='gz')

compare_to_spss <- function() {
  # If you have access to SPSS, run the syntax file and use
  # it to compare here. If the result is empty, everything is good!
  require(haven)
  SPSS_FILE <- 'data/src/eclsk2011k5/ECLSK2011_K5PUF.sav'
  if (!exists('childK5p_spss')) {
    childK5p_spss <<- read_spss(SPSS_FILE) %>%
    mutate_if(is.character, ~na_if(., ''))
  }
  result <- map2_chr(childK5p, childK5p_spss, all.equal, check.attributes=F)
  tibble(
    cname = names(result),
    eq = result
  ) %>%
    filter(eq != 'TRUE')
}
