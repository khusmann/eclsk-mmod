configfile: 'config.yml'

localrules: all, subset_2011, make_mmod_2011

rule all:
  input: expand('data/mmod/{model}_result.rds', model=config['eclsk2011_models'])

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

rule make_mmod_2011:
  input:
    'data/eclsk_subset_2011.rds'
  output:
    'data/mmod/{model}_model.rds'
  params:
    measures=lambda wildcards: config['eclsk2011_models'][wildcards.model]
  conda:
    'envs/eclsk-analysis.yml'
  script:
    'scripts/make_mmod.R'

rule run_mmod_2011:
  input:
    'data/mmod/{model}_model.rds'
  output:
    'data/mmod/{model}_result.rds'
  resources:
    mem_mb=4000,
    walltime_min=2*60
  conda:
    'envs/eclsk-analysis.yml'
  threads: 8
  script:
    'scripts/run_mmod.R'
