options(tidyverse.quiet = T)
library(tidyverse)
library(labelled)

DAT_FILE <- snakemake@input[["dat_file"]]
SPS_FILE <- snakemake@input[["sps_file"]]
OUTPUT_FILE <- snakemake@output[[1]]

#DAT_FILE <- 'data/src/eclsk8/childk8p.dat.fwf.gz'
#SPS_FILE <- 'data/src/eclsk8/ECLSK_Kto8_child_SPSS.sps'
#OUTPUT_FILE <- 'data/src/eclsk8/childk8p.rds'

full_syntax <- read_lines(SPS_FILE)

fwf_start <- which(str_detect(full_syntax, '^/1$'))
label_start <- which(str_detect(full_syntax, '^\\s*VARIABLE LABEL\\s*$'))
val_start <- which(str_detect(full_syntax, '^\\s*VALUE LABELS\\s*$'))
select_start <- which(str_detect(full_syntax, '^\\s*SELECT\\s*$'))

fwf_syntax <- full_syntax[fwf_start:label_start]
label_syntax <- full_syntax[label_start:val_start]
val_syntax <- full_syntax[val_start:select_start]

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
  extract(raw, c('cname', 'label'), '      (.*)        "(.*)"') %>%
  filter(!is.na(cname))

all_defs <- full_join(fwf_defs, label_defs, by='cname')

cat('-- Unpacking value types\n')
val_defs <- tibble(
  begin = str_which(val_syntax, '   / '),
  end = lead(begin, default=tail(which(str_detect(val_syntax, '      (.*)  "(.*)"')), 1)),
  cname = str_match(val_syntax[begin], '   / (.*)')[,2],
  val_dict = map2(begin+1, end-1, function(b, e) {
    result <- tibble(
      raw = val_syntax[b:e]
    ) %>%
      extract(raw, c('val', 'label'), '      (.*)  "(.*)"')
    if (str_sub(result$val[1], -1) == '"') {
      # val is a string
      result <- result %>%
        mutate(val = str_sub(val, 2, -2))
    } else if (str_detect(result$val[1], ' - ')) {
      # val is a range
      cat(paste('TODO: support range labels like', result$val[1], '\n'))
      return(NA)
    } else {
      # val is a number
      result <- result %>%
        mutate(val = as.integer(val))
    }
    setNames(result$val, result$label)
  })
) %>%
  filter(!is.na(val_dict))

cat('-- Loading data set\n')
df <- read_fwf(DAT_FILE, fwf_widths(all_defs$len, all_defs$cname), paste(all_defs$type, collapse=''))

cat('-- Setting variable labels\n')
var_label(df) <- all_defs$label

cat('-- Set value labels\n')
val_labels(df) <- setNames(val_defs$val_dict, val_defs$cname)

cat('-- Writing file\n')
write_rds(df, OUTPUT_FILE, compress='gz')

# TODO: try this for eclsk1999 also
compare_to_spss2011 <- function() {
  # If you have access to SPSS, run the syntax file and use
  # it to compare here. If the result is empty, everything is good!
  require(haven)
  SPSS_FILE <- 'data/src/eclsk2011k5/ECLSK2011_K5PUF.sav'
  if (!exists('df_spss')) {
    df_spss <<- read_spss(SPSS_FILE) %>%
    mutate_if(is.character, ~na_if(., ''))
  }
  result <- map2_chr(df, df_spss, all.equal, check.attributes=F)
  tibble(
    cname = names(result),
    eq = result
  ) %>%
    filter(eq != 'TRUE')
}
