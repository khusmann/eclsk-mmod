configfile: 'config.yml'

rule all:
  input: expand('data/mmod/{model}.rds', model=config['eclsk2011_models'])

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

rule mmod_2011:
  input:
    'data/eclsk_subset_2011.rds'
  output:
    'data/mmod/{model}.rds'
  params:
    measures=lambda wildcards: config['eclsk2011_models'][wildcards.model]
  conda:
    'envs/eclsk-analysis.yml'
  script:
    'scripts/run_mmod.R'
