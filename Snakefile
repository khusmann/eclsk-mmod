
rule subset_2011:
  input:
    'data/raw/eclsk_2011_childk4.sav'
  output:
    'data/ecls2011vars.rds',
    'data/eclsk_subset_2011.rds'
  conda:
    'envs/eclsk-analysis.yml'
  script:
    'scripts/eclsk2011subset.R'

